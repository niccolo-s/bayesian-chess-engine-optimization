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



# Tournament setup -------
engines <- c("A", "B", "C", "D", "E", "SM")
n_engines <- 6
tc <- 300 + 40 * 3  # 420 seconds

# Results from first 2 rounds (from score table in PDF)
# Format: row vs column scores, so A vs B = 3 means A got 3 points from games against B
observed_scores <- matrix(c(
  0,   3,   1.5, 4,   2,   2,    # A's scores against each opponent
  1,   0,   2,   3.5, 0,   0.5,  # B's scores
  2.5, 2,   0,   4,   0.5, 1,    # C's scores
  0,   0.5, 0,   0,   0,   0,    # D's scores
  2,   4,   3.5, 4,   0,   2,    # E's scores
  2,   3.5, 3,   4,   2,   0     # SM's scores
), nrow = 6, byrow = TRUE)

# Calculate current total scores
current_scores <- rowSums(observed_scores)
names(current_scores) <- engines

# After 2 rounds, each pairing has had 4 games (2 per round, switching colors)
# In rounds 3-5, each pairing will have 6 more games (2 per round × 3 rounds)

# Function to simulate one game outcome
simulate_game <- function(rating_white, rating_black, white_advantage, p_draw_base, draw_scale) {
  rating_diff <- (rating_white + white_advantage) - rating_black
  abs_diff <- abs(rating_diff)
  expected_score <- 1.0 / (1.0 + 10^(-rating_diff / 400.0))
  
  p_draw <- p_draw_base * exp(-abs_diff / draw_scale)
  
  probs <- c(
    (1 - p_draw) * (1 - expected_score),  # Black wins
    p_draw,                                # Draw
    (1 - p_draw) * expected_score          # White wins
  )
  
  outcome <- sample(1:3, size = 1, prob = probs)
  
  # Return points: [points for white, points for black]
  if (outcome == 1) return(c(0, 1))      # Black wins
  if (outcome == 2) return(c(0.5, 0.5))  # Draw
  return(c(1, 0))                         # White wins
}

# Generate pairings for one round
get_round_pairings <- function() {
  pairings <- list()
  idx <- 1
  for (i in 1:(n_engines - 1)) {
    for (j in (i + 1):n_engines) {
      pairings[[idx]] <- list(white = i, black = j, pair = c(i, j))
      idx <- idx + 1
      pairings[[idx]] <- list(white = j, black = i, pair = c(i, j))
      idx <- idx + 1
    }
  }
  return(pairings)
}

pairings <- get_round_pairings()

# Monte Carlo simulation
n_sims <- nrow(posterior_samples)
sm_placements <- integer(n_sims)

cat("Running", n_sims, "tournament simulations...\n")

for (sim in 1:n_sims) {
  if (sim %% 1000 == 0) cat("Simulation", sim, "/", n_sims, "\n")
  
  # Extract parameters
  rating <- as.numeric(c(
    posterior_samples[sim, "rating[1]"],
    posterior_samples[sim, "rating[2]"],
    posterior_samples[sim, "rating[3]"],
    posterior_samples[sim, "rating[4]"],
    posterior_samples[sim, "rating[5]"],
    posterior_samples[sim, "rating[6]"]
  ))
  
  beta_tc <- as.numeric(c(
    posterior_samples[sim, "beta_tc[1]"],
    posterior_samples[sim, "beta_tc[2]"],
    posterior_samples[sim, "beta_tc[3]"],
    posterior_samples[sim, "beta_tc[4]"],
    posterior_samples[sim, "beta_tc[5]"],
    posterior_samples[sim, "beta_tc[6]"]
  ))
  
  white_adv <- as.numeric(posterior_samples[sim, "white_advantage"])
  p_draw_base <- as.numeric(posterior_samples[sim, "p_draw_base"])
  draw_scale <- as.numeric(posterior_samples[sim, "draw_scale"])
  
  # Time-adjusted ratings
  rating_tc <- rating + beta_tc * tc
  
  # Start with observed scores from rounds 1-2
  scores <- current_scores
  
  # Simulate remaining 3 rounds (rounds 3, 4, 5)
  for (round in 1:3) {
    for (pairing in pairings) {
      white_idx <- pairing$white
      black_idx <- pairing$black
      
      game_result <- simulate_game(
        rating_tc[white_idx],
        rating_tc[black_idx],
        white_advantage,
        p_draw_base,
        draw_scale
      )
      
      scores[white_idx] <- scores[white_idx] + game_result[1]
      scores[black_idx] <- scores[black_idx] + game_result[2]
    }
  }
  
  # Determine final placement for SchachMaus
  final_ranking <- rank(-scores, ties.method = "random")
  sm_placements[sim] <- final_ranking[6]  # SM is engine 6
}

# Calculate probabilities
prob_first <- mean(sm_placements == 1)
prob_second <- mean(sm_placements == 2)
prob_first_or_second <- mean(sm_placements <= 2)
prob_third_or_lower <- mean(sm_placements >= 3)

# Results
cat("\n=== Tournament Prediction Results ===\n")
cat("Current standings after 2 rounds:\n")
for (i in 1:6) {
  cat(" ", engines[i], ":", current_scores[i], "points\n")
}

cat("\nProbability SchachMaus finishes:\n")
cat("  1st place:", round(prob_first * 100, 2), "%\n")
cat("  2nd place:", round(prob_second * 100, 2), "%\n")
cat("  1st or 2nd:", round(prob_first_or_second * 100, 2), "%\n")
cat("  3rd or lower:", round(prob_third_or_lower * 100, 2), "%\n")

cat("\nFull placement distribution:\n")
for (place in 1:6) {
  prob <- mean(sm_placements == place)
  cat("  ", place, "th place:", round(prob * 100, 2), "%\n")
}

# Betting decision
expected_value <- prob_first_or_second * 100 - prob_third_or_lower * 100
cat("\n=== Betting Decision ===\n")
cat("Expected value:", round(expected_value, 2), "euros\n")

if (expected_value > 0) {
  cat("RECOMMENDATION: Take the bet (positive EV)\n")
} else {
  cat("RECOMMENDATION: Decline the bet (negative EV)\n")
}

# Additional consideration for risk
cat("\nRisk consideration:\n")
cat("Even if EV is positive, consider your risk tolerance.\n")
cat("With", n_engines, "teams, variance can be high.\n")
