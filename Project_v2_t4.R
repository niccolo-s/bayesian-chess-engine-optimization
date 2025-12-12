library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(patchwork)
library(posterior)

# Load data ----------
games <- read_csv("games.csv")

# Data pre-processing ----------
unique_engines <- unique(c(games$white, games$black))
engine_ids <- data.frame(
  engine = unique_engines,
  id = 1:length(unique_engines)
)

games_processed <- games %>%
  separate(timecontrol, into = c("base_time", "increment"), 
           sep = "\\+", convert = TRUE) %>%
  mutate(
    tc = base_time + 40 * increment,
  ) %>%
  left_join(
    data.frame(
      engine = unique(c(games$white, games$black)),
      id = 1:length(unique(c(games$white, games$black)))
    ),
    by = c("white" = "engine")
  ) %>%
  rename(white_id = id) %>%
  left_join(
    data.frame(
      engine = unique(c(games$white, games$black)),
      id = 1:length(unique(c(games$white, games$black)))
    ),
    by = c("black" = "engine")
  ) %>%
  rename(black_id = id) %>%
  mutate(
    outcome = case_when(
      result == "0-1"     ~ 1,
      result == "1/2-1/2" ~ 2,
      result == "1-0"     ~ 3
    )
  )

games_processed$outcome = as.numeric(games_processed$outcome)

# Inspect the time controls
games_processed %>%
  select(base_time, increment, tc) %>%
  distinct() %>%
  arrange(tc)

summary(games_processed$tc)

# Model fitting ----------
stan_data <- list(
  N = nrow(games_processed),
  K = length(unique_engines),
  white_id = games_processed$white_id,
  black_id = games_processed$black_id,
  outcome = games_processed$outcome,
  tc = games_processed$tc
)

# Build model
mod <- cmdstan_model("elo_model_v2.stan")

# Run MCMC sampling
fit <- mod$sample(
  data = stan_data,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)


# Create tier list
rating_summary <- fit$summary(variables = "rating")

rating_df <- data.frame(
  engine = unique_engines,
  mean_rating = rating_summary$mean,
  sd = rating_summary$sd,
  lower_95 = rating_summary$q5,
  upper_95 = rating_summary$q95
) %>%
  arrange(desc(mean_rating))

rating_df

# Extract posteriors - UPDATED METHOD
draws_rvars <- as_draws_rvars(fit$draws())
posterior <- list(
  rating = draws_of(draws_rvars$rating),
  beta = draws_of(draws_rvars$beta)
)

# Define the 4 time controls
time_controls <- c(15, 60, 260, 600)

# For each time control
tier_lists <- list()

for (j in 1:length(time_controls)) {
  tc_value <- time_controls[j]
  
  # Calculate ratings for each engine at this time control
  ratings_at_tc <- matrix(NA, nrow = dim(posterior$rating)[1], ncol = stan_data$K)
  
  for (i in 1:dim(posterior$rating)[1]) {
    ratings_at_tc[i, ] <- posterior$rating[i, ] + posterior$beta[i, ] * tc_value
  }
  
  # Create tier list
  tier_lists[[j]] <- data.frame(
    engine = unique_engines,
    tc = time_controls[j],
    mean_rating = apply(ratings_at_tc, 2, mean),
    sd = apply(ratings_at_tc, 2, sd),
    lower_95 = apply(ratings_at_tc, 2, quantile, 0.025),
    upper_95 = apply(ratings_at_tc, 2, quantile, 0.975)
  ) %>%
    arrange(desc(mean_rating))
}

# Combine all tier lists
all_tiers <- bind_rows(tier_lists)

# View tier list for each TC
for (tc_val in time_controls) {
  cat("\n=== Tier List for TC =", tc_val, "===\n")
  print(all_tiers %>% filter(tc == tc_val) %>% select(engine, mean_rating, lower_95, upper_95))
}





#------------------------------------------
# Task 4: Sequential Testing Development
#------------------------------------------
# 1. Extract Parameters from Fitted Model -------------------------------------
posterior_summary <- fit$summary()

# Extract global parameters
white_adv   <- posterior_summary$mean[posterior_summary$variable == "white_advantage"]
p_draw_base <- posterior_summary$mean[posterior_summary$variable == "p_draw_base"]
draw_scale  <- posterior_summary$mean[posterior_summary$variable == "draw_scale"]

# Extract BOTH rating and beta for each engine
engine_params <- data.frame(
  engine_id = 1:length(unique_engines),
  engine = unique_engines
) %>%
  mutate(
    rating = sapply(engine_id, function(i) {
      posterior_summary$mean[posterior_summary$variable == paste0("rating[", i, "]")]
    }),
    # OVDJE JE PROMJENA: "beta[" u "beta_tc["
    beta = sapply(engine_id, function(i) {
      posterior_summary$mean[posterior_summary$variable == paste0("beta_tc[", i, "]")]
    })
  )

# 2. Sequential Test Function (FINAL CORRECTED SPRT) --------------------------
# Tests H0: Delta = 0 (No improvement)
#    vs H1: Delta = E0 (Improvement by E0)

sequential_test <- function(games_subset, new_engine, base_engine, 
                            engine_params, E0 = 10, 
                            alpha = 0.05, beta_param = 0.05) {
  
  # Get parameters for both engines
  new_params  <- engine_params %>% filter(engine == new_engine)
  base_params <- engine_params %>% filter(engine == base_engine)
  
  # SPRT boundaries
  bound_A <- log(beta_param / (1 - alpha))      # Accept H0
  bound_B <- log((1 - beta_param) / alpha)      # Accept H1
  
  LLR <- 0  # Log-likelihood ratio
  
  for (i in 1:nrow(games_subset)) {
    game <- games_subset[i, ]
    tc <- game$tc
    
    # Calculate Time Control effect difference
    # diff_tc = (Beta_New - Beta_Base) * tc
    diff_tc <- (new_params$beta - base_params$beta) * tc
    
    # --- H0: No improvement (Base Diff = 0) ---
    if (game$white == new_engine) {
      # New is White: (New - Base) + Adv
      # Base Diff=0 -> Total = 0 + diff_tc + adv
      diff_H0 <- 0 + diff_tc + white_adv
    } else {
      # Base is White: (Base - New) + Adv
      # Base Diff=0 -> Total = -(0 + diff_tc) + adv = -diff_tc + adv
      diff_H0 <- -diff_tc + white_adv
    }
    
    # Probabilities H0
    exp_score_H0 <- 1 / (1 + 10^(-diff_H0 / 400))
    p_draw_H0    <- p_draw_base * exp(-abs(diff_H0) / draw_scale)
    
    probs_H0 <- c(
      (1 - p_draw_H0) * (1 - exp_score_H0), # Black win
      p_draw_H0,                            # Draw
      (1 - p_draw_H0) * exp_score_H0        # White win
    )
    
    # --- H1: Improvement (Base Diff = E0) ---
    if (game$white == new_engine) {
      # New is White: (New - Base) is E0
      # Total = E0 + diff_tc + adv
      diff_H1 <- E0 + diff_tc + white_adv
    } else {
      # Base is White: (Base - New) is -E0
      # Total = -E0 - diff_tc + adv
      diff_H1 <- -E0 - diff_tc + white_adv
    }
    
    # Probabilities H1
    exp_score_H1 <- 1 / (1 + 10^(-diff_H1 / 400))
    p_draw_H1    <- p_draw_base * exp(-abs(diff_H1) / draw_scale)
    
    probs_H1 <- c(
      (1 - p_draw_H1) * (1 - exp_score_H1),
      p_draw_H1,
      (1 - p_draw_H1) * exp_score_H1
    )
    
    # Update LLR
    outcome_idx <- game$outcome
    lik_H1 <- max(probs_H1[outcome_idx], 1e-10)
    lik_H0 <- max(probs_H0[outcome_idx], 1e-10)
    
    LLR <- LLR + log(lik_H1 / lik_H0)
    
    # Check stopping conditions
    if (LLR >= bound_B) {
      return(list(
        decision = "H1: New engine better",
        games_played = i,
        total_available = nrow(games_subset),
        final_LLR = LLR
      ))
    } else if (LLR <= bound_A) {
      return(list(
        decision = "H0: New engine NOT better",
        games_played = i,
        total_available = nrow(games_subset),
        final_LLR = LLR
      ))
    }
  }
  
  return(list(
    decision = "Undecided",
    games_played = nrow(games_subset),
    total_available = nrow(games_subset),
    final_LLR = LLR
  ))
}


# 3. Run Simulation -----------------------------------------------------------

pairings <- games_processed %>%
  select(white, black) %>%
  mutate(pair = paste(white, black, sep = " vs ")) %>%
  distinct(pair, .keep_all = TRUE)

results <- list()

cat("\n=== STARTING SPRT SIMULATION ===\n")
cat("E0 = 10 Elo | alpha = 0.05 | beta = 0.05\n\n")

for (i in 1:nrow(pairings)) {
  new_eng <- pairings$white[i]
  base_eng <- pairings$black[i]
  
  pair_games <- games_processed %>%
    filter((white == new_eng & black == base_eng) | 
             (black == new_eng & white == base_eng))
  
  if (nrow(pair_games) > 0) {
    result <- sequential_test(pair_games, new_eng, base_eng, 
                              engine_params, E0 = 10, 
                              alpha = 0.2, beta_param = 0.2)
    
    results[[i]] <- data.frame(
      Pair = paste(new_eng, "vs", base_eng),
      New_Engine = new_eng,
      Base_Engine = base_eng,
      Decision = result$decision,
      Games_Played = result$games_played,
      Total_Available = result$total_available,
      Final_LLR = round(result$final_LLR, 2)
    )
    
    cat(sprintf("%-30s: %-30s (%d/%d games)\n", 
                results[[i]]$Pair, result$decision, 
                result$games_played, result$total_available))
  }
}

# 4. Analysis -----------------------------------------------------------------

results_df <- bind_rows(results)

results_analysis <- results_df %>%
  left_join(rating_df %>% select(engine, mean_rating), 
            by = c("New_Engine" = "engine")) %>%
  rename(Rating_New = mean_rating) %>%
  left_join(rating_df %>% select(engine, mean_rating),
            by = c("Base_Engine" = "engine")) %>%
  rename(Rating_Base = mean_rating) %>%
  mutate(
    Model_Rating_Diff = Rating_New - Rating_Base,
    Test_Concluded = Decision != "Undecided",
    Conclusion_Type = case_when(
      Decision == "H1: New engine better" ~ "Better",
      Decision == "H0: New engine NOT better" ~ "Not Better",
      TRUE ~ "Undecided"
    )
  )

cat("\n=== RESULTS SUMMARY ===\n")
print(results_df)

cat("\n=== RELATIONSHIP TO RATING LIST ===\n")
cat("\nPairs where H1 accepted (New is better):\n")
print(results_analysis %>%
        filter(Conclusion_Type == "Better") %>%
        arrange(desc(Model_Rating_Diff)) %>%
        select(Pair, Model_Rating_Diff, Games_Played))

cat("\nPairs where H0 accepted (New is NOT better):\n")
print(results_analysis %>%
        filter(Conclusion_Type == "Not Better") %>%
        arrange(Model_Rating_Diff) %>%
        select(Pair, Model_Rating_Diff, Games_Played))

cat("\nPairs where test did not conclude:\n")
print(results_analysis %>%
        filter(Conclusion_Type == "Undecided") %>%
        arrange(abs(Model_Rating_Diff)) %>%
        select(Pair, Model_Rating_Diff, Games_Played, Total_Available))

