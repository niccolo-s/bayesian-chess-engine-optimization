library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(patchwork)
library(ggplot2)

## Load data ----------
games <- read.csv("games.csv", header = TRUE)


##  Data pre-processing ----------
unique_engines <- unique(c(games$white, games$black))
engine_ids <- data.frame(
  engine = unique_engines,
  id = 1:length(unique_engines))


## Process time control and outcome ----------
games_processed <- games %>%
  # Split "30+1" into time and increment
  separate(timecontrol, into = c("base_time", "increment"), 
           sep = "\\+", convert = TRUE) %>%
  
  # Calculate expected game duration (in seconds)
  mutate(tc = base_time + 40 * increment) %>%
  
  # Create engine IDs
  left_join(
    data.frame(
      engine = unique(c(games$white, games$black)),
      id = 1:length(unique(c(games$white, games$black)))),
    by = c("white" = "engine")) %>%
  rename(white_id = id) %>%
  left_join(
    data.frame(
      engine = unique(c(games$white, games$black)),
      id = 1:length(unique(c(games$white, games$black)))),
    by = c("black" = "engine")) %>%
  rename(black_id = id) %>%
  
  # Convert outcome to integers (how many successes into 2 trials)
  mutate(
    outcome = case_when(
      result == "0-1"     ~ 0,
      result == "1/2-1/2" ~ 1,
      result == "1-0"     ~ 2))

games_processed$outcome = as.numeric(games_processed$outcome)

# Inspect the time controls
games_processed %>%
  select(base_time, increment, tc) %>%
  distinct() %>%
  arrange(tc)

# Check distribution
summary(games_processed$tc)


##  Model fitting ----------
# Collect data for Stan
stan_data <- list(
  N = nrow(games_processed),
  K = length(unique_engines),
  white_id = games_processed$white_id,
  black_id = games_processed$black_id,
  outcome = games_processed$outcome,
  tc = games_processed$tc
)

# Define the model 
mod <- cmdstan_model("elo_model_v3.stan")

# Fit the model 
fit <- mod$sample(
  data = stan_data, 
  iter_warmup = 2000,
  iter_sampling = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)

# Extract the posterior for ratings 
ratings_draws <- fit$draws(variables = "rating", format = "matrix")

ratings_df <- data.frame(
  engine = unique_engines,
  mean_rating = colMeans(ratings_draws),
  sd = apply(ratings_draws, 2, sd),
  lower_95 = apply(ratings_draws, 2, quantile, probs = 0.025),
  upper_95 = apply(ratings_draws, 2, quantile, probs = 0.975)) %>%
  arrange(desc(mean_rating))

ratings_df

# Compute superiority probabilities
n_engines <- length(unique_engines)
superiority_matrix <- matrix(0, n_engines, n_engines)

for (i in 1:n_engines) {
  for (j in 1:n_engines) {
    if (i != j) {
      # compute the mean number of higher ratings for i across all samples
      superiority_matrix[i, j] <- mean(ratings_draws[, i] > ratings_draws[, j])
    }
  }
}
superiority_matrix


## Time controls ----------
# Define the 4 time controls you want to evaluate
time_controls <- c(15, 60, 260, 600)  

# Extract the posterior for betas  
beta_draws <- fit$draws(variables = "beta", format = "matrix")

# For each time control
tier_lists <- list()

for (j in 1:length(time_controls)) {
  tc_value <- time_controls[j]
  
  # Calculate ratings for each engine at this time control
  # For each MCMC sample
  ratings_at_tc <- matrix(NA, nrow = nrow(ratings_draws), ncol = stan_data$K)
  
  for (i in 1:nrow(ratings_draws)) {
    ratings_at_tc[i, ] <- ratings_draws[i, ] + beta_draws[i, ] * tc_value
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


## Plots ---------------
# Continuous range for tc
tc_range <- seq(0, max(games_processed$tc), length.out = 100)

# Initialize plot list
plots <- list()

# Uncertainty plot
for (engine_idx in 1:length(unique_engines)) {
  engine_name <- unique_engines[engine_idx]
  
  # Compute ratings for all tc
  ratings_matrix <- matrix(NA, nrow = nrow(ratings_draws), ncol = length(tc_range))
  
  for (i in 1:nrow(ratings_draws)) {
    ratings_matrix[i, ] <- ratings_draws[i, engine_idx] + 
      beta_draws[i, engine_idx] * tc_range
  }
  
  # Compute statistics
  ratings_data <- data.frame(
    tc = tc_range,
    mean = apply(ratings_matrix, 2, mean),
    sd = apply(ratings_matrix, 2, sd),
    lower_95 = apply(ratings_matrix, 2, quantile, 0.025),
    upper_95 = apply(ratings_matrix, 2, quantile, 0.975)
  )
  
  # Subplot
  plots[[engine_idx]] <- ggplot(ratings_data, aes(x = tc, y = mean)) +
    # 95% CI (light)
    geom_ribbon(
      aes(ymin = lower_95, ymax = upper_95),
      fill = "lightblue",
      alpha = 0.4) +
    # ±1 SD (dark)
    geom_ribbon(
      aes(ymin = mean - sd, ymax = mean + sd),
      fill = "steelblue",
      alpha = 0.6) +
    # Mean line
    geom_line(linewidth = 1.2, color = "darkblue") +
    # Dots for requested tcs
    geom_point(
      data = all_tiers %>% filter(engine == engine_name),
      aes(x = tc, y = mean_rating),
      size = 3,
      color = "red",
      inherit.aes = FALSE) +
    labs(
      title = engine_name,
      x = "Time Control (sec)",
      y = "Rating") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    )
}

# Combine them in a grid 2X3
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)

# Add general title
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Rating Curves for All Engines",
    subtitle = "Dark ribbon = ±1 SD, Light ribbon = 95% CI, Red points = observed ratings",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  )

print(combined_plot)
ggsave("all_engines_rating_curves.png", combined_plot, width = 16, height = 10, dpi = 300)

# Extract the raw MCMC samples for "SM"
sm_idx <- which(unique_engines == "SM")
sm_raw_samples <- ratings_draws[, sm_idx]

# Create a data frame for "SM" MCMC samples
sm_df <- data.frame(rating = sm_raw_samples)
# Rename the column from "rating.2." to "rating"
colnames(sm_df) <- "rating"
# Create histogram of MCMC samples for "SM"
sm_histogram <- ggplot(sm_df, aes(x = rating)) +
  # Histogram with default binwidth (keeping it as you prefer)
  geom_histogram(
    binwidth = 5,  # You mentioned binwidth is fine, keeping as is
    fill = "#383D78", 
    color = "grey", 
    alpha = 0.7
  ) +
  # Add kernel density estimate (KDE) with manually specified bandwidth
  geom_density(
    aes(y = after_stat(density) * nrow(sm_df) * 5),
    color = "#383D78",
    size = 2,
    alpha = 0.3,
    adjust = 2
  ) +
  geom_vline(
    xintercept = mean(sm_df$rating), # Calculate the mean of the rating column
    linetype = "dashed",            
    color = "red",              
    linewidth = 2                    
  ) +
  annotate(
    "text",
    x = mean(sm_df$rating),                                            # Anchor x-position at the mean line
    y = Inf,                                                           # Place at the top of the plot area (Inf = maximum y)
    label = paste("Mean:", round(mean(sm_df$rating), 2)),              # Formats "Mean: value"
    color = "red",
    size = 4,
    hjust = -0.1,                                                      # Nudges text slightly to the right of the x-position
    vjust = 1.5                                                        # Nudges text down from the top edge
  ) +
  labs(
    title = paste("Histogram of MCMC Samples for Engine 'SM'"),
    x = "Rating",
    y = "Number of Samples"  
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

# Print the standalone histogram with KDE
print(sm_histogram)

# Optionally, save it as an image
ggsave("SM_histogram_with_kde.png", sm_histogram, width = 8, height = 6, dpi = 300)
