library(rstan)
library(tidyverse)
library(bayesplot)

# Load data ----------
games <- read_csv("games.csv")



# Data pre-processing ----------
unique_engines <- unique(c(games$white, games$black))
engine_ids <- data.frame(
  engine = unique_engines,
  id = 1:length(unique_engines)
)


games_processed <- games %>%
  left_join(engine_ids, by = c("white" = "engine")) %>%
  rename(white_id = id) %>%
  left_join(engine_ids, by = c("black" = "engine")) %>%
  rename(black_id = id) %>%
  # create scores column
  mutate(
    white_score = case_when(
      result == "1-0" ~ 1,
      result == "1/2-1/2" ~ 0.5,
      result == "0-1" ~ 0
    )
  )
games_processed




# Model fitting ----------
# collect data for Stan
stan_data <- list(
  N = nrow(games_processed),
  K = length(unique_engines),
  white_id = games_processed$white_id,
  black_id = games_processed$black_id,
  white_score = games_processed$white_score
)

# Fit the model
fit <- stan(
  file = "elo_model.stan",
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
posterior <- rstan::extract(fit)
n_engines <- length(unique_engines)
superiority_matrix <- matrix(0, n_engines, n_engines)

for (i in 1:n_engines) {
  for (j in 1:n_engines) {
    if (i != j) {
      # compute the mean number of higher ratings for i across all samples
      superiority_matrix[i, j] <- mean(posterior$rating[, i] > posterior$rating[, j])
    }
  }
}
superiority_matrix



