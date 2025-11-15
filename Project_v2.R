library(rstan)
library(tidyverse)
library(bayesplot)

# Load data ----------
games <- read_csv("games.csv")



# Data pre-processing ----------

# Process time control
games_processed <- games %>%
  # Split "30+1" into time and increment
  separate(timecontrol, into = c("base_time", "increment"), 
           sep = "\\+", convert = TRUE) %>%
  
  # Calculate expected game duration (in seconds)
  mutate(
    tc = base_time + 40 * increment,
  ) %>%
  
  # Create engine IDs
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
  
  # Convert outcome to categorical
  mutate(
    outcome = case_when(
      result == "0-1"     ~ 1,
      result == "1/2-1/2" ~ 2,
      result == "1-0"     ~ 3
    )
  )

# Inspect the time controls
games_processed %>%
  select(base_time, increment, tc) %>%
  distinct() %>%
  arrange(tc)

# Check distribution
summary(games_processed$tc)




# Model fitting ----------
# collect data for Stan
stan_data <- list(
  N = nrow(games_processed),
  K = length(unique_engines),
  white_id = games_processed$white_id,
  black_id = games_processed$black_id,
  outcome = games_processed$outcome,
  tc = games_processed$tc
)

# Fit the model
fit <- stan(
  file = "elo_model_v2.stan",
  data = stan_data,
  iter = 4000,
  warmup = 2000,
  chains = 4,
  cores = 4,
  seed = 123
)

# Create tier list
rating_summary <- summary(fit, pars = "rating")$summary

rating_df <- data.frame(
  engine = unique_engines,
  mean_rating = rating_summary[, "mean"],
  sd = rating_summary[, "sd"],
  lower_95 = rating_summary[, "2.5%"],
  upper_95 = rating_summary[, "97.5%"]
) %>%
  arrange(desc(mean_rating)) # order by mean_rating

rating_df


# Compute superiority probabilities
# Extract posteriors
posterior <- rstan::extract(fit)

# Define the 4 time controls you want to evaluate
time_controls <- c(15, 60, 180, 600)  # Or whatever your 4 TCs are

# For each time control
tier_lists <- list()

for (j in 1:length(time_controls)) {
  tc_value <- time_controls[j]
  
  # Calculate ratings for each engine at this time control
  # For each MCMC sample
  ratings_at_tc <- matrix(NA, nrow = nrow(posterior$rating), ncol = stan_data$K)
  
  for (i in 1:nrow(posterior$rating)) {
    ratings_at_tc[i, ] <- posterior$rating[i, ] + posterior$beta[i, ] * tc_value
  }
  
  # Recenter to mean 2000
  for (i in 1:nrow(ratings_at_tc)) {
    ratings_at_tc[i, ] <- ratings_at_tc[i, ] - mean(ratings_at_tc[i, ]) + 2000
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










