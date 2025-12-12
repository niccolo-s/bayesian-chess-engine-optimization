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
#4th task (MODEL CHECKING)
#------------------------------------------ 

# --- 1. EXTRACT FIXED PARAMETERS FROM STAN MODEL ---
# We extract the global parameters (white_advantage, draw parameters) from the 
# previously fitted model. These will be treated as known constants for the SPRT.

draws_summary <- fit$summary(
  variables = c("white_advantage", "p_draw_base", "draw_scale"),
  "mean"
)

# Store parameters in a list for easy access
global_params <- list(
  white_adv = draws_summary$mean[draws_summary$variable == "white_advantage"],
  p_draw_base = draws_summary$mean[draws_summary$variable == "p_draw_base"],
  draw_scale = draws_summary$mean[draws_summary$variable == "draw_scale"]
)

# --- 2. SEQUENTIAL TEST FUNCTION (BAYES FACTOR / SPRT) ---
# This function implements the Sequential Probability Ratio Test (SPRT).
# Instead of calculating the full posterior density (Grid Approach), we compare
# two specific point hypotheses: H0 (No improvement) vs H1 (Improvement by E0).
# This approach is computationally very efficient and adheres to standard SPRT methodology.
#
# Arguments:
#   games_df: DataFrame containing the sequence of games
#   engine_A: The 'new' engine (Challenger)
#   engine_B: The 'base' engine (Defender)
#   params:   Global chess parameters (white_adv, etc.)
#   E0:       The improvement margin in Elo to test for (default 10)
#   alpha:    Threshold for accepting H1 (default 20, corresponds to strong evidence)
#   beta:     Threshold for accepting H0 (default 1/20)

run_seq_test_bf <- function(games_df, engine_A, engine_B, params, E0 = 10, alpha = 20, beta = 1/20) {
  
  # Initialize Bayes Factor (BF).
  # BF starts at 1, implying equal prior odds (1:1) for H0 and H1.
  bayes_factor <- 1
  
  # Storage for history (to analyze trajectory later)
  history_bf <- numeric(nrow(games_df))
  decision <- "Undecided"
  stopped_at <- nrow(games_df)
  
  # Define the two hypotheses to be compared:
  # H0: The rating difference is 0 (The new engine is not better).
  # H1: The rating difference is exactly E0 (The new engine has improved).
  elo0 <- 0   # Null Hypothesis value (Delta = 0)
  elo1 <- E0  # Alternative Hypothesis value (Delta = E0)
  
  # Iterate through games sequentially
  for (i in 1:nrow(games_df)) {
    game <- games_df[i, ]
    is_A_white <- (game$white == engine_A)
    
    # --- 1. Calculate Likelihoods under H0 (Elo Diff = 0) ---
    # Determine effective rating difference from White's perspective including advantage
    diff0 <- if(is_A_white) { elo0 + params$white_adv } else { -elo0 + params$white_adv }
    
    # Calculate probabilities using the Bradley-Terry model (Sigmoid)
    exp_score0 <- 1.0 / (1.0 + 10^(-diff0 / 400.0))
    # Draw probability model (decaying exponential)
    p_draw0 <- params$p_draw_base * exp(-abs(diff0) / params$draw_scale)
    
    # Probabilities for outcomes: [1: Black Win, 2: Draw, 3: White Win]
    probs0 <- c(
      (1 - p_draw0) * (1 - exp_score0), 
      p_draw0,                          
      (1 - p_draw0) * exp_score0        
    )
    
    # --- 2. Calculate Likelihoods under H1 (Elo Diff = E0) ---
    # Repeat calculation for the alternative hypothesis
    diff1 <- if(is_A_white) { elo1 + params$white_adv } else { -elo1 + params$white_adv }
    exp_score1 <- 1.0 / (1.0 + 10^(-diff1 / 400.0))
    p_draw1 <- params$p_draw_base * exp(-abs(diff1) / params$draw_scale)
    
    probs1 <- c(
      (1 - p_draw1) * (1 - exp_score1), 
      p_draw1,                          
      (1 - p_draw1) * exp_score1        
    )
    
    # --- 3. Update Bayes Factor based on actual game outcome ---
    # game$outcome should be: 1 (Black Win), 2 (Draw), or 3 (White Win)
    outcome_idx <- game$outcome
    
    lik_H0 <- probs0[outcome_idx]
    lik_H1 <- probs1[outcome_idx]
    
    # Update Rule: BF_new = BF_old * (Likelihood(Data|H1) / Likelihood(Data|H0))
    # This represents how much more likely the observed data is under H1 compared to H0.
    bayes_factor <- bayes_factor * (lik_H1 / lik_H0)
    history_bf[i] <- bayes_factor
    
    # --- 4. Check Stopping Conditions ---
    # If BF > alpha (e.g., 20), evidence overwhelmingly supports H1
    if (bayes_factor > alpha) {
      decision <- paste(engine_A, "is better (Accept H1)")
      stopped_at <- i
      break
      # If BF < beta (e.g., 1/20), evidence overwhelmingly supports H0
    } else if (bayes_factor < beta) {
      decision <- paste(engine_A, "is NOT better (Accept H0)")
      stopped_at <- i
      break
    }
  }
  
  # Return results list
  return(list(
    pair = paste(engine_A, "vs", engine_B),
    decision = decision,
    stopped_at = stopped_at,
    total_games = nrow(games_df),
    final_bf = ifelse(i > 0, history_bf[i], 1),
    bf_history = history_bf[1:i]
  ))
}

# --- 3. RUN SIMULATION ON ALL PAIRS ---

# Generate all unique pairs from the dataset
engines <- unique_engines
pairs <- combn(engines, 2, simplify = FALSE)

results_list <- list()

cat("\n=== STARTING SPRT SIMULATION (Bayes Factor Approach) ===\n")

for (p in pairs) {
  eng1 <- p[1]
  eng2 <- p[2]
  
  # Filter games for this specific pair
  pair_games <- games_processed %>%
    filter((white == eng1 & black == eng2) | (white == eng2 & black == eng1)) %>%
    # Ensure chronological order
    arrange(row_number()) 
  
  if (nrow(pair_games) > 0) {
    # Run the SPRT test
    # using alpha=20 (strong evidence) and beta=1/20
    res <- run_seq_test_bf(pair_games, eng1, eng2, global_params, E0 = 10, alpha = 8, beta = 1/8)
    
    results_list[[length(results_list) + 1]] <- res
    
    # Print status
    cat(sprintf("Pair %-15s: %-30s (Stopped at game %d / %d)\n", 
                res$pair, res$decision, res$stopped_at, nrow(pair_games)))
  }
}

# --- 4. SUMMARY TABLE ---
# Convert results to a clean DataFrame for reporting
results_df <- do.call(rbind, lapply(results_list, function(x) {
  data.frame(
    Pair = x$pair,
    Decision = x$decision,
    Games_Played = x$stopped_at,
    Total_Games = x$total_games,
    Final_BF = round(x$final_bf, 2)
  )
}))

print(results_df)







