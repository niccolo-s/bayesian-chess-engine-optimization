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

# Plots ---------------
# Continuous range for tc
tc_range <- seq(0, max(games_processed$tc), length.out = 100)

# Initialize plot list
plots <- list()

# Uncertainty plot
for (engine_idx in 1:length(unique_engines)) {
  engine_name <- unique_engines[engine_idx]
  
  # Compute ratings for all tc
  ratings_matrix <- matrix(NA, nrow = dim(posterior$rating)[1], ncol = length(tc_range))
  
  for (i in 1:dim(posterior$rating)[1]) {
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
    geom_ribbon(
      aes(ymin = lower_95, ymax = upper_95),
      fill = "lightblue",
      alpha = 0.4
    ) +
    geom_ribbon(
      aes(ymin = mean - sd, ymax = mean + sd),
      fill = "steelblue",
      alpha = 0.6
    ) +
    geom_line(linewidth = 1.2, color = "darkblue") +
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

# Combine in grid 2×3
combined_plot <- wrap_plots(plots, nrow = 2, ncol = 3)

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



# Posterior plots ------------
y_rep = fit$draws("y_rep", format="matrix")

bayesplot::ppc_dens_overlay(stan_data$outcome, y_rep[1500:2000,])




