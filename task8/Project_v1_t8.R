# ==============================================================================
# Task 8: MCMC
# ==============================================================================

library(Rschach)
library(dplyr)
library(cmdstanr)
library(DiceKriging)
library(tidyverse)
library(metR)
library(ggplot2)

setwd("~/Desktop/Bayesian Statistics/Progetto/T8")


book_file <- "8moves_v3.epd"
book_fen <- read.csv(book_file, header = FALSE)[[1]]


#### 1. Define 4 engine versions ####

# Model 1: Baseline (Default)
params_1 <- list()

# Model 2: Step 5 (Manual Tuning -> Using Hint Values)
params_2 <- list(
  NMP_intercept = 3,
  NMP_slope     = 0,
  LMR_intercept = 1,
  RFP_intercept = -30,
  RFP_slope     = 150
)

# Model 3: Step 6 (Step 5 + Optimized LMR_slope)
params_3 <- modifyList(params_2, list(
  LMR_slope = 0.3015075  # result from Step 6
))

# Model 4: Step 7 (Step 6 + Optimized RFP parameters)
params_4 <- modifyList(params_3, list(
  RFP_intercept = 300,   # result from Step 7
  RFP_slope     = 0      # result from Step 7
))

candidates_task8 <- list(
  list(name = "1_Base",        params = params_1),
  list(name = "2_Manual",      params = params_2),
  list(name = "3_LMR_Opt",     params = params_3),
  list(name = "4_RFP_Final",   params = params_4)
)



#### 2. Run Round-Robin Tournament ####
# Time control: 10+0.1 seconds, at least 50 rounds

df <- data.frame(white = character(),
                 black = character(),
                 result = character(),
                 stringsAsFactors = FALSE)

n_rounds <- 50
set.seed(789)

for(round in 1:n_rounds) {
  cat(sprintf("\n=== ROUND %d/%d ===\n", round, n_rounds))
  
  # Loop through all pairs
  for(i in 1:(length(candidates_task8)-1)){
    for(j in (i+1):length(candidates_task8)){
      
      cand_i <- candidates_task8[[i]]
      cand_j <- candidates_task8[[j]]
      
      if(length(cand_i$params) == 0) {
        e.cand_i <- Engine(name = cand_i$name)
      } else {
        e.cand_i <- Engine(name = cand_i$name, params = cand_i$params)
      }
      
      if(length(cand_j$params) == 0) {
        e.cand_j <- Engine(name = cand_j$name)
      } else {
        e.cand_j <- Engine(name = cand_j$name, params = cand_j$params)
      }
      
      cat(sprintf("MATCH: %s vs %s \n", cand_i$name, cand_j$name))
      
      # Sample ONE opening for this pair in this round (to be fair for both colors)
      set.seed(Sys.time()) # Ensure randomness across rounds
      current_book_pos <- sample(book_fen, size=1)
      
      # Play 2 games (Swap colors)
      for(k in 1:2){
        
        if(k == 1){
          white_engine <- e.cand_i
          black_engine <- e.cand_j
        } else {
          white_engine <- e.cand_j
          black_engine <- e.cand_i
        }
        
        # Play game with Task 8 specs: 10+0.1s, resign_count=5
        res <- play.tournament(white_engine, black_engine, 
                               nr_rounds=1L, 
                               book=current_book_pos,
                               tc_base = 10, 
                               tc_inc = 0.1,
                               resign_count = 5L)
        
        for(r in res){
          df <- rbind(df, data.frame(
            white  = r$White,
            black  = r$Black,
            result = r$Result,
            stringsAsFactors=FALSE))
        }
      }
    }
  }
}

# Save results
write.csv(df, "task8_tournament_results.csv", row.names = FALSE)





#### 3. Prepare data for MCMC ####

unique_engines <- c("Base", "Manual", "LMR_Opt", "RFP_Opt")

df$outcome <- sapply(df$result, function(x){
  if (x == "1-0") return(3)       # White win
  if (x == "0-1") return(1)       # Black win
  if (x == "1/2-1/2") return(2)   # Draw
  return(NA)
})

df$white_id <- match(df$white, unique_engines)
df$black_id <- match(df$black, unique_engines)

# Check data
table(df$outcome)
table(df$white, df$black)

# Calculate expected game duration (for tc variable, though it's constant here)
tc_value <- 10 + 40 * 0.1  # = 14 seconds

mcmc_data <- list(
  N = nrow(df),
  K = length(unique_engines),
  white_id = df$white_id,
  black_id = df$black_id,
  outcome = df$outcome,
  tc = rep(tc_value, nrow(df))  # Constant time control
)

# Save data for MCMC
saveRDS(mcmc_data, "step8_mcmc_data.rds")
saveRDS(df, "step8_game_results.rds")

cat(sprintf("Games: %d | Engines: %d\n", mcmc_data$N, mcmc_data$K))




#### 4. Define MCMC ####
mcmc_elo <- function(data, n_iter = 100000, burn_in = 10000, 
                     thin = 50, n_chains = 2) {
  
  N <- nrow(data)
  K <- length(unique(c(data$white_id, data$black_id)))
  
  # Initialize parameters
  init_params <- function() {
    list(
      rating = rnorm(K, 2000, 100),
      beta_tc = rnorm(K, 0, 0.05),
      white_advantage = rnorm(1, 35, 10),
      p_draw_base = rbeta(1, 3, 7),
      draw_scale = rnorm(1, 300, 50)
    )
  }
  
  # Log posterior calculation
  log_posterior <- function(params, data) {
    rating <- params$rating
    beta_tc <- params$beta_tc
    white_advantage <- params$white_advantage
    p_draw_base <- params$p_draw_base
    draw_scale <- params$draw_scale
    
    # Priors
    log_prior <- sum(dnorm(rating, 2000, 200, log = TRUE)) +
      dnorm(mean(rating), 2000, 10, log = TRUE) +
      sum(dnorm(beta_tc, 0, 0.1, log = TRUE)) +
      dnorm(white_advantage, 35, 15, log = TRUE) +
      dbeta(p_draw_base, 3, 7, log = TRUE) +
      dnorm(draw_scale, 300, 100, log = TRUE)
    
    # Likelihood
    log_lik <- 0
    for (i in 1:N) {
      rating_white <- rating[data$white_id[i]] + 
        beta_tc[data$white_id[i]] * data$tc[i]
      rating_black <- rating[data$black_id[i]] + 
        beta_tc[data$black_id[i]] * data$tc[i]
      
      rating_diff <- (rating_white + white_advantage) - rating_black
      abs_diff <- abs(rating_diff)
      expected_score <- 1 / (1 + 10^(-rating_diff / 400))
      
      p_draw <- p_draw_base * exp(-abs_diff / draw_scale)
      
      # Probabilities: [black wins, draw, white wins]
      probs <- c(
        (1 - expected_score) * (1 - p_draw),
        p_draw,
        expected_score * (1 - p_draw)
      )
      
      log_lik <- log_lik + log(probs[data$outcome[i]])
    }
    
    return(log_prior + log_lik)
  }
  
  # Run single chain
  run_chain <- function(chain_id) {
    params <- init_params()
    n_save <- (n_iter - burn_in) %/% thin
    
    # Storage
    samples <- list(
      rating = matrix(NA, n_save, K),
      beta_tc = matrix(NA, n_save, K),
      white_advantage = numeric(n_save),
      p_draw_base = numeric(n_save),
      draw_scale = numeric(n_save)
    )
    
    # Proposal standard deviations (can/shoudl be tuned)
    prop_sd <- list(
      rating = 30,
      beta_tc = 0.02,
      white_advantage = 5,
      p_draw_base = 0.05,
      draw_scale = 20
    )
    
    accepted <- list(rating = 0, beta_tc = 0, white_advantage = 0,
                     p_draw_base = 0, draw_scale = 0)
    
    current_lp <- log_posterior(params, data)
    
    for (iter in 1:n_iter) {
      
      # Update ratings
      for (k in 1:K) {
        params_prop <- params
        params_prop$rating[k] <- params$rating[k] + 
          rnorm(1, 0, prop_sd$rating)
        
        prop_lp <- log_posterior(params_prop, data)
        if (log(runif(1)) < prop_lp - current_lp) {
          params <- params_prop
          current_lp <- prop_lp
          accepted$rating <- accepted$rating + 1
        }
      }
      
      # Update each beta_tc
      for (k in 1:K) {
        params_prop <- params
        params_prop$beta_tc[k] <- params$beta_tc[k] + 
          rnorm(1, 0, prop_sd$beta_tc)
        
        prop_lp <- log_posterior(params_prop, data)
        if (log(runif(1)) < prop_lp - current_lp) {
          params <- params_prop
          current_lp <- prop_lp
          accepted$beta_tc <- accepted$beta_tc + 1
        }
      }
      
      # Update white_advantage
      params_prop <- params
      params_prop$white_advantage <- params$white_advantage + 
        rnorm(1, 0, prop_sd$white_advantage)
      prop_lp <- log_posterior(params_prop, data)
      if (log(runif(1)) < prop_lp - current_lp) {
        params <- params_prop
        current_lp <- prop_lp
        accepted$white_advantage <- accepted$white_advantage + 1
      }
      
      # Update p_draw_base
      params_prop <- params
      prop_val <- params$p_draw_base + rnorm(1, 0, prop_sd$p_draw_base)
      # Manage boundaries [0, 1]
      while (prop_val < 0 || prop_val > 1) {
        if (prop_val < 0) prop_val <- -prop_val
        if (prop_val > 1) prop_val <- 2 - prop_val
      }
      params_prop$p_draw_base <- prop_val
      
      prop_lp <- log_posterior(params_prop, data)
      if (log(runif(1)) < prop_lp - current_lp) {
        params <- params_prop
        current_lp <- prop_lp
        accepted$p_draw_base <- accepted$p_draw_base + 1
      }
      
      # Update draw_scale (constrained > 0)
      params_prop <- params
      params_prop$draw_scale <- abs(params$draw_scale + 
                                      rnorm(1, 0, prop_sd$draw_scale))
      prop_lp <- log_posterior(params_prop, data)
      if (log(runif(1)) < prop_lp - current_lp) {
        params <- params_prop
        current_lp <- prop_lp
        accepted$draw_scale <- accepted$draw_scale + 1
      }
      
      # Save samples
      if (iter > burn_in && (iter - burn_in) %% thin == 0) {
        idx <- (iter - burn_in) %/% thin
        samples$rating[idx, ] <- params$rating
        samples$beta_tc[idx, ] <- params$beta_tc
        samples$white_advantage[idx] <- params$white_advantage
        samples$p_draw_base[idx] <- params$p_draw_base
        samples$draw_scale[idx] <- params$draw_scale
      }
      
      if (iter %% 1000 == 0) {
        cat(sprintf("Chain %d: Iteration %d/%d\n", chain_id, iter, n_iter))
      }
    }
    
    # Calculate acceptance rates
    acc_rates <- list(
      rating = accepted$rating / (n_iter * K),
      beta_tc = accepted$beta_tc / (n_iter * K),
      white_advantage = accepted$white_advantage / n_iter,
      p_draw_base = accepted$p_draw_base / n_iter,
      draw_scale = accepted$draw_scale / n_iter
    )
    
    list(samples = samples, acceptance_rates = acc_rates)
  }
  
  # Run multiple chains
  results <- lapply(1:n_chains, run_chain)
  return(results)
}



#### 5. Inference on ratings ####

# Prepare data
engine_levels <- c("1_Base", "2_Manual", "3_LMR_Opt", "4_RFP_Final")

df$white_id <- match(df$white, engine_levels)
df$black_id <- match(df$black, engine_levels)

id_lookup <- data.frame(
  ID = 1:length(engine_levels),
  Name = engine_levels
)

head(df)


tournament_data <- data.frame(
  white_id = df$white_id,  
  black_id = df$black_id,
  outcome = df$outcome,
  tc = 10 + 40 * 0.1
)

# Run MCMC
set.seed(123)
results <- mcmc_elo_custom(tournament_data, 
                           n_iter = 100000,
                           burn_in = 10000,
                           thin = 50,
                           n_chains = 2)

# Check convergence
library(coda)
chain1_rating <- mcmc(results[[1]]$samples$rating)
chain2_rating <- mcmc(results[[2]]$samples$rating)
gelman.diag(mcmc.list(chain1_rating, chain2_rating))
# Multivariate psrf 1 suggests good convergence


# Combine samples from both chains

# Combine the 'rating' matrices from your two chains
combined_ratings <- rbind(results[[1]]$samples$rating, 
                          results[[2]]$samples$rating)

colnames(combined_ratings) <- c("1_Base", "2_Manual", "3_LMR", "4_RFP")

# Calculate Summary Statistics
final_stats <- data.frame(
  Engine = colnames(combined_ratings),
  Mean_Elo = colMeans(combined_ratings),
  Lower_CI = apply(combined_ratings, 2, quantile, probs = 0.025),
  Upper_CI = apply(combined_ratings, 2, quantile, probs = 0.975)
)

# Sort by mean rating to show the ranking
final_stats <- final_stats[order(final_stats$Mean_Elo, decreasing = TRUE), ]

print(final_stats)

# Compute Probability of Superiority
prob_final_beats_base <- mean(combined_ratings[, "4_RFP"] > combined_ratings[, "1_Base"])
prob_final_beats_manual <- mean(combined_ratings[, "4_RFP"] > combined_ratings[, "2_Manual"])
prob_final_beats_t6 <- mean(combined_ratings[, "4_RFP"] > combined_ratings[, "3_LMR"])

cat(sprintf("\nProbability Final > Base:   %.4f\n", prob_final_beats_base))
cat(sprintf("Probability Final > Manual: %.4f\n", prob_final_beats_manual))
cat(sprintf("Probability Final > 3_LMR: %.4f\n", prob_final_beats_t6))

# Boxplot Visualization
boxplot(combined_ratings, 
        main = "Posterior Distribution of Engine Ratings",
        ylab = "Elo Rating",
        col = c("gray90", "lightblue", "gold", "lightgreen"),
        las = 2) # Rotate labels



#### 6. Plots ####
library(ggplot2)
library(dplyr)

# 1. Combine chains & Extract ratings
# 'results' is the list returned by your mcmc_elo function
combined_ratings <- rbind(results[[1]]$samples$rating, 
                          results[[2]]$samples$rating)

# Use the exact names from your script
colnames(combined_ratings) <- c("1_Base", "2_Manual", "3_LMR_Opt", "4_RFP_Final")

# 2. Calculate Summary Statistics for Plotting
plot_data <- data.frame(
  engine = colnames(combined_ratings),
  mean_rating = colMeans(combined_ratings),
  sd = apply(combined_ratings, 2, sd),
  lower_95 = apply(combined_ratings, 2, quantile, probs = 0.025),
  upper_95 = apply(combined_ratings, 2, quantile, probs = 0.975)
) %>%
  arrange(mean_rating) # Sort by rating for the plot order

# Set factor levels to ensure ggplot respects the sorted order (weakest at bottom, strongest at top)
plot_data$engine <- factor(plot_data$engine, levels = plot_data$engine)

# 3. Create the Forest Plot (Blue/Lightblue style)
p_forest_t8 <- ggplot(plot_data, aes(x = mean_rating, y = engine)) +
  # 95% Credible Interval (Light Blue)
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95, color = "95% CI"), 
                 height = 0.3, linewidth = 1) +
  
  # Mean +/- 1 SD Interval (Darker Blue)
  geom_errorbarh(aes(xmin = mean_rating - sd, xmax = mean_rating + sd, color = "±1 SD"), 
                 height = 0, linewidth = 2) +
  
  # Mean Point
  geom_point(size = 5, color = "darkblue") +
  
  # Manual Color Scale to match previous tasks
  scale_color_manual(name = "Uncertainty", 
                     values = c("95% CI" = "lightblue", "±1 SD" = "steelblue")) +
  
  # Labels and Theme
  labs(
    x = "Elo Rating",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.position = "top",
    legend.background = element_rect(color = "black", linewidth = 0.5, fill = "white"),
    panel.grid.major.y = element_line(color = "gray90") 
  )

# Display and Save
print(p_forest_t8)
ggsave("task8_ratings.png", p_forest_t8, width = 8, height = 5, dpi = 300)
