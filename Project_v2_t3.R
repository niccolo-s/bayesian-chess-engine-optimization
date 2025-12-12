#### TASK 3 ####
library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(patchwork)
library(posterior)

# Load posterior samples
posterior_samples <- readRDS("posterior_samples.rds")

head(posterior_samples)
str(posterior_samples)
summary(posterior_samples)

# Convert to regular data frame to avoid warnings
posterior_df <- as.data.frame(posterior_samples)

tc <- 420
engines <- c("A", "B", "C", "D", "E", "SM")

# Score matrix
observed_scores <- matrix(c(
  0,   3,   1.5, 4,   2,   2,
  1,   0,   2,   3.5, 0,   0.5,
  2.5, 2,   0,   4,   0.5, 1,
  0,   0.5, 0,   0,   0,   0,
  2,   4,   3.5, 4,   0,   2,
  2,   3.5, 3,   4,   2,   0
), nrow = 6, byrow = TRUE)

current_scores <- rowSums(observed_scores)
names(current_scores) <- engines

# Extract parameters as matrices
# posterior_samples order: A, SM, B, C, D, E
# Matrix order: A, B, C, D, E, SM

rating_all <- cbind(
  posterior_df[, "rating[1]"],   # A
  posterior_df[, "rating[3]"],   # B
  posterior_df[, "rating[4]"],   # C
  posterior_df[, "rating[5]"],   # D
  posterior_df[, "rating[6]"],   # E
  posterior_df[, "rating[2]"]    # SM
)

beta_tc_all <- cbind(
  posterior_df[, "beta_tc[1]"],  # A
  posterior_df[, "beta_tc[3]"],  # B
  posterior_df[, "beta_tc[4]"],  # C
  posterior_df[, "beta_tc[5]"],  # D
  posterior_df[, "beta_tc[6]"],  # E
  posterior_df[, "beta_tc[2]"]   # SM
)

white_adv_all <- posterior_df[, "white_advantage"]
p_draw_base_all <- posterior_df[, "p_draw_base"]
draw_scale_all <- posterior_df[, "draw_scale"]

# Time-adjusted ratings
rating_tc_all <- rating_all + beta_tc_all * tc


n_sims <- nrow(posterior_df)

# Function to calculate expected score in one game
calc_expected_score <- function(rating_i, rating_j, white_adv, p_draw_base, draw_scale) {
  # Expected score when i plays white, j plays black
  rating_diff_i_white <- (rating_i + white_adv) - rating_j
  abs_diff <- abs(rating_diff_i_white)
  expected_i_white <- 1.0 / (1.0 + 10^(-rating_diff_i_white / 400.0))
  p_draw <- p_draw_base * exp(-abs_diff / draw_scale)
  
  # Expected points for i when playing white
  exp_i_white <- (1 - p_draw) * expected_i_white + 0.5 * p_draw
  
  return(exp_i_white)
}

# Calculate log-likelihood for each posterior sample
log_weights <- numeric(n_sims)

for (sim in 1:n_sims) {
  if (sim %% 1000 == 0) cat("Sample", sim, "/", n_sims, "\n")
  
  rating_tc <- rating_tc_all[sim, ]
  white_adv <- white_adv_all[sim]
  p_draw_base <- p_draw_base_all[sim]
  draw_scale <- draw_scale_all[sim]
  
  log_lik <- 0
  
  # For each pairing
  for (i in 1:5) {
    for (j in (i+1):6) {
      # In 4 games: i plays white twice, black twice
      # Expected score for i from these 4 games
      exp_i_white <- calc_expected_score(rating_tc[i], rating_tc[j], 
                                         white_adv, p_draw_base, draw_scale)
      exp_i_black <- 1 - calc_expected_score(rating_tc[j], rating_tc[i], 
                                             white_adv, p_draw_base, draw_scale)
      
      # Total expected score for i in 4 games (2 white, 2 black)
      expected_score_i <- 2 * exp_i_white + 2 * exp_i_black
      
      # Observed score
      observed_score_i <- observed_scores[i, j]
      
      # Likelihood: normal approximation centered on expected score
      # Variance for 4 games: roughly 4 * p(1-p) where p is win probability
      # Simplified: use sd = 1.0 (can be tuned)
      sd_score <- 1.0
      log_lik <- log_lik + dnorm(observed_score_i, mean = expected_score_i, 
                                 sd = sd_score, log = TRUE)
    }
  }
  
  log_weights[sim] <- log_lik
}

# Convert to weights
max_log_weight <- max(log_weights)
weights <- exp(log_weights - max_log_weight)
weights <- weights / sum(weights)

cat("\nEffective sample size:", round(1 / sum(weights^2)), "\n")

# Resample posterior
set.seed(123)
resampled_indices <- sample(1:n_sims, size = n_sims, replace = TRUE, prob = weights)

# Create updated posterior samples
rating_tc_updated <- rating_tc_all[resampled_indices, ]
white_adv_updated <- white_adv_all[resampled_indices]
p_draw_base_updated <- p_draw_base_all[resampled_indices]
draw_scale_updated <- draw_scale_all[resampled_indices]

# Rating comparison after update
cat("Original mean ratings:\n", colMeans(rating_tc_all))
cat("\nUpdated mean ratings:\n", colMeans(rating_tc_updated))



# Game simulation function
simulate_game <- function(rating_white, rating_black, white_adv, p_draw_base, draw_scale) {
  rating_diff <- (rating_white + white_adv) - rating_black
  abs_diff <- abs(rating_diff)
  expected_score <- 1.0 / (1.0 + 10^(-rating_diff / 400.0))
  
  p_draw <- p_draw_base * exp(-abs_diff / draw_scale)
  
  probs <- c(
    (1 - p_draw) * (1 - expected_score),
    p_draw,
    (1 - p_draw) * expected_score
  )
  
  outcome <- sample(1:3, size = 1, prob = probs)
  
  if (outcome == 1) return(c(0, 1))
  if (outcome == 2) return(c(0.5, 0.5))
  return(c(1, 0))
}

# Generate pairings
get_round_pairings <- function() {
  pairings <- list()
  idx <- 1
  for (i in 1:5) {
    for (j in (i + 1):6) {
      pairings[[idx]] <- list(white = i, black = j)
      idx <- idx + 1
      pairings[[idx]] <- list(white = j, black = i)
      idx <- idx + 1
    }
  }
  return(pairings)
}

pairings <- get_round_pairings()

# Monte Carlo simulation
sm_placements <- integer(n_sims)

for (sim in 1:n_sims) {
  if (sim %% 1000 == 0) {
    cat("Simulation", sim, "/", n_sims, "\n")
  }
  
  rating_tc <- rating_tc_updated[sim, ]      # Get row sim (6 ratings)
  white_adv <- white_adv_updated[sim]        # Get element sim
  p_draw_base <- p_draw_base_updated[sim]    # Get element sim
  draw_scale <- draw_scale_updated[sim]      # Get element sim
  
  # Start with current scores
  scores <- current_scores
  
  # Simulate remaining 3 rounds
  for (round in 1:3) {
    for (pairing in pairings) {
      white_idx <- pairing$white
      black_idx <- pairing$black
      
      game_result <- simulate_game(
        rating_tc[white_idx],
        rating_tc[black_idx],
        white_adv,
        p_draw_base,
        draw_scale
      )
      
      scores[white_idx] <- scores[white_idx] + game_result[1]
      scores[black_idx] <- scores[black_idx] + game_result[2]
    }
  }
  
  # Final ranking
  final_ranking <- rank(-scores, ties.method = "random")
  sm_placements[sim] <- final_ranking[6]
}

# Results
prob_first <- mean(sm_placements == 1)
prob_second <- mean(sm_placements == 2)
prob_first_or_second <- mean(sm_placements <= 2)
prob_third_or_lower <- mean(sm_placements >= 3)


for (place in 1:6) {
  prob <- mean(sm_placements == place)
  cat("  ", place, "th place:", round(prob * 100, 2), "%\n")
}

# Betting decision
expected_value <- prob_first_or_second * 100 - prob_third_or_lower * 100
cat("Expected value:", round(expected_value, 2), "euros\n")
