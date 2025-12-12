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
tc <- 300 + 40 * 3  # 420 seconds

# Stan model engine order: A, SM, B, C, D, E (indices 1,2,3,4,5,6)
# Problem order: A, B, C, D, E, SM
# Mapping: problem_idx -> stan_idx
stan_to_problem <- c(1, 3, 4, 5, 6, 2)  # [A, B, C, D, E, SM] -> [1, 3, 4, 5, 6, 2]
problem_to_stan <- c(1, 6, 2, 3, 4, 5)  # [A, SM, B, C, D, E] position in problem order

# Engine names in problem order
engines <- c("A", "B", "C", "D", "E", "SM")

# Score matrix from problem (A, B, C, D, E, SM order)
observed_scores <- matrix(c(
  0,   3,   1.5, 4,   2,   2,    # A vs [A,B,C,D,E,SM]
  1,   0,   2,   3.5, 0,   0.5,  # B
  2.5, 2,   0,   4,   0.5, 1,    # C
  0,   0.5, 0,   0,   0,   0,    # D
  2,   4,   3.5, 4,   0,   2,    # E
  2,   3.5, 3,   4,   2,   0     # SM
), nrow = 6, byrow = TRUE)

current_scores <- rowSums(observed_scores)
names(current_scores) <- engines

cat("Current scores after 2 rounds (problem order):\n")
print(current_scores)

# Function to simulate one game
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

# Generate pairings for one round (in problem order: A=1, B=2, C=3, D=4, E=5, SM=6)
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
n_sims <- nrow(posterior_samples)
sm_placements <- integer(n_sims)

cat("\nRunning", n_sims, "tournament simulations...\n")
start_time <- Sys.time()

for (sim in 1:n_sims) {
  if (sim %% 1000 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat("Simulation", sim, "/", n_sims, "- Elapsed:", round(elapsed, 1), "sec\n")
  }
  
  # Extract parameters in STAN order (A, SM, B, C, D, E)
  rating_stan <- as.numeric(c(
    posterior_samples[sim, "rating[1]"],  # A
    posterior_samples[sim, "rating[2]"],  # SM
    posterior_samples[sim, "rating[3]"],  # B
    posterior_samples[sim, "rating[4]"],  # C
    posterior_samples[sim, "rating[5]"],  # D
    posterior_samples[sim, "rating[6]"]   # E
  ))
  
  beta_tc_stan <- as.numeric(c(
    posterior_samples[sim, "beta_tc[1]"],  # A
    posterior_samples[sim, "beta_tc[2]"],  # SM
    posterior_samples[sim, "beta_tc[3]"],  # B
    posterior_samples[sim, "beta_tc[4]"],  # C
    posterior_samples[sim, "beta_tc[5]"],  # D
    posterior_samples[sim, "beta_tc[6]"]   # E
  ))
  
  # Reorder to PROBLEM order (A, B, C, D, E, SM)
  rating <- rating_stan[problem_to_stan]
  beta_tc <- beta_tc_stan[problem_to_stan]
  
  white_adv <- as.numeric(posterior_samples[sim, "white_advantage"])
  p_draw_base <- as.numeric(posterior_samples[sim, "p_draw_base"])
  draw_scale <- as.numeric(posterior_samples[sim, "draw_scale"])
  
  # Time-adjusted ratings
  rating_tc <- rating + beta_tc * tc
  
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
  
  # Final ranking (SM is position 6)
  final_ranking <- rank(scores, ties.method = "random")
  sm_placements[sim] <- final_ranking[6]
}

# Results
prob_first <- mean(sm_placements == 1)
prob_second <- mean(sm_placements == 2)
prob_first_or_second <- mean(sm_placements <= 2)
prob_third_or_lower <- mean(sm_placements >= 3)

cat("\n=== Tournament Prediction Results ===\n")
cat("Probability SchachMaus finishes:\n")
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



# Check the posterior mean ratings for each engine
library(posterior)

posterior_samples <- readRDS("posterior_samples.rds")

rating_cols <- grep("^rating\\[", names(posterior_samples))

# Calculate posterior means
mean_ratings <- colMeans(posterior_samples[, rating_cols])
names(mean_ratings) <- c("A", "B", "C", "D", "E", "SM")

print("Posterior mean ratings:")
print(mean_ratings)

# With time control adjustment
beta_cols <- grep("^beta_tc\\[", names(posterior_samples))
mean_beta <- colMeans(posterior_samples[, beta_cols])
names(mean_beta) <- c("A", "B", "C", "D", "E", "SM")

tc <- 420
adjusted_ratings <- mean_ratings + mean_beta * tc

print("\nTime-adjusted ratings (tc=420):")
print(adjusted_ratings)

mean_ratings

print("\nCurrent tournament standings after 2 rounds:")
print(data.frame(
  Engine = c("A", "B", "C", "D", "E", "SM"),
  Points = current_scores,
  Expected_Rating = adjusted_ratings
))
