library(rstan)
library(tidyverse)
library(bayesplot)
library(patchwork)

# Load data ----------
games <- read_csv("games.csv")



# Data pre-processing ----------
unique_engines <- unique(c(games$white, games$black))
engine_ids <- data.frame(
  engine = unique_engines,
  id = 1:length(unique_engines)
)

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
      result == "0-1"     ~ 0,
      result == "1/2-1/2" ~ 1,
      result == "1-0"     ~ 2
    )
  )

games_processed$outcome = as.numeric(games_processed$outcome)

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
  file = "elo_model_v3.stan",
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



# Plots ---------------

# Extract posteriors
posterior <- rstan::extract(fit)

# Continuous range for tc
tc_range <- seq(0, max(games_processed$tc), length.out = 100)

# Initialize plot list
plots <- list()

# Uncertainty plot
for (engine_idx in 1:length(unique_engines)) {
  engine_name <- unique_engines[engine_idx]
  
  # Compute ratings for all tc
  ratings_matrix <- matrix(NA, nrow = nrow(posterior$rating), ncol = length(tc_range))
  
  for (i in 1:nrow(posterior$rating)) {
    ratings_matrix[i, ] <- posterior$rating[i, engine_idx] + 
      posterior$beta[i, engine_idx] * tc_range
  }
  
  # Compute statistics
  rating_data <- data.frame(
    tc = tc_range,
    mean = apply(ratings_matrix, 2, mean),
    sd = apply(ratings_matrix, 2, sd),
    lower_95 = apply(ratings_matrix, 2, quantile, 0.025),
    upper_95 = apply(ratings_matrix, 2, quantile, 0.975)
  )
  
  # Subplot
  plots[[engine_idx]] <- ggplot(rating_data, aes(x = tc, y = mean)) +
    # 95% CI (light)
    geom_ribbon(
      aes(ymin = lower_95, ymax = upper_95),
      fill = "lightblue",
      alpha = 0.4
    ) +
    # ±1 SD (dark)
    geom_ribbon(
      aes(ymin = mean - sd, ymax = mean + sd),
      fill = "steelblue",
      alpha = 0.6
    ) +
    # Mean line
    geom_line(linewidth = 1.2, color = "darkblue") +
    # Dots for requested tcs
    geom_point(
      data = all_tiers %>% filter(engine == engine_name),
      aes(x = tc, y = mean_rating),
      size = 3,
      color = "red",
      inherit.aes = FALSE
    ) +
    labs(
      title = engine_name,
      x = "Time Control (sec)",
      y = "Rating"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    )
}

# Combina in griglia 2×3
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)

# Aggiungi titolo generale
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Rating Curves for All Engines",
    subtitle = "Dark ribbon = ±1 SD, Light ribbon = 95% CI, Red points = observed ratings",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5)
    )
  )

# Mostra
print(combined_plot)

# Salva
ggsave("all_engines_rating_curves.png", combined_plot, width = 16, height = 10, dpi = 300)




