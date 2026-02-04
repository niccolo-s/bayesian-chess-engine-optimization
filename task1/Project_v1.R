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
  left_join(engine_ids, by = c("white" = "engine")) %>%
  rename(white_id = id) %>%
  left_join(engine_ids, by = c("black" = "engine")) %>%
  rename(black_id = id) %>%
  mutate(
    outcome = case_when(
      result == "0-1"     ~ 1,
      result == "1/2-1/2" ~ 2,
      result == "1-0"     ~ 3
    )
  )

games_processed$outcome = as.numeric(games_processed$outcome)

# Model fitting ----------
stan_data <- list(
  N = nrow(games_processed),
  K = length(unique_engines),
  white_id = games_processed$white_id,
  black_id = games_processed$black_id,
  outcome = games_processed$outcome
)

# Build model
mod <- cmdstan_model("elo_model_v1.stan") 

# Run MCMC sampling
fit <- mod$sample(
  data = stan_data,
  iter_sampling = 2000,
  iter_warmup = 2000,
  chains = 4,
  parallel_chains = 4,
  seed = 123
)

# Extract only model parameters
draws_df <- as_draws_df(fit$draws(variables = c("rating", "white_advantage",
                                                "p_draw_base", "draw_scale")))
saveRDS(draws_df, "posterior_samples_task1.rds")

# Create tier list (Static Ratings)
rating_summary <- fit$summary(variables = "rating")

rating_df <- data.frame(
  engine = unique_engines,
  mean_rating = rating_summary$mean,
  sd = rating_summary$sd,
  lower_95 = rating_summary$q5,
  upper_95 = rating_summary$q95
) %>%
  arrange(desc(mean_rating))

print("=== Final Static Tier List ===")
print(rating_df)

# Plots (Interval Plot instead of Regression Curves) ---------------
# Since there is no time control, we plot intervals for each engine
# similar to the ribbon plot but just as vertical intervals.

# Extract posteriors for plotting
draws_rvars <- as_draws_rvars(fit$draws())
posterior_ratings <- draws_of(draws_rvars$rating)

# Prepare data for plotting
plot_data <- rating_df %>%
  mutate(engine = factor(engine, levels = engine[order(mean_rating)])) # Reorder for plot

# Create the plot
p_ratings <- ggplot(plot_data, aes(x = mean_rating, y = engine)) +
  # 95% CI Line (Use linewidth instead of size)
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95, color = "95% CI"), 
                 height = 0.2, linewidth = 1) + 
  
  # Mean SD Line (Use linewidth instead of size)
  geom_errorbarh(aes(xmin = mean_rating - sd, xmax = mean_rating + sd, color = "±1 SD"), 
                 height = 0, linewidth = 2) +
  
  # Mean Point (Keep size here! Points still use size)
  geom_point(size = 4, color = "darkblue") +
  
  # Custom colors
  scale_color_manual(name = "Uncertainty", 
                     values = c("95% CI" = "lightblue", "±1 SD" = "steelblue")) +
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
    # If you had a border on the legend, use linewidth there too:
    legend.background = element_rect(color = "black", linewidth = 0.5) 
  )

print(p_ratings)
ggsave("task1_ratings.png", p_ratings, width = 10, height = 6, dpi = 300)


# Posterior Predictive Checks (Model Checking) ------------
y_rep <- fit$draws("y_rep", format = "matrix")
y <- stan_data$outcome

# 1. Grouped bar plots by engine (White)
ppc_bars_grouped(y, y_rep[sample(1:4000, 500), ], 
                 group = games_processed$white, 
                 freq = FALSE, prob = 0.9) +
  labs(title = "Posterior Predictive Check by White Player")

# 2. Histogram comparison
ppc_hist(y, y_rep[1:8, ])

# 3. Density overlay
ppc_dens_overlay(y, y_rep[1:50, ])

# 4. Empirical CDF differences
ppc_ecdf_overlay_grouped(y, y_rep[1:100, ], group = games_processed$white)
