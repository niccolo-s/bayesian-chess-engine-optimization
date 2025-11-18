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

for (engine_idx in 1:length(unique_engines)) {
  engine_name <- unique_engines[engine_idx]
  
  # Compute posterior ratings
  ratings_matrix <- matrix(NA, nrow = dim(posterior$rating)[1], ncol = length(tc_range))
  for (i in 1:dim(posterior$rating)[1]) {
    ratings_matrix[i, ] <- posterior$rating[i, engine_idx] +
      posterior$beta[i, engine_idx] * tc_range
  }
  
  rating_data <- data.frame(
    tc = tc_range,
    mean = apply(ratings_matrix, 2, mean),
    sd = apply(ratings_matrix, 2, sd),
    lower_95 = apply(ratings_matrix, 2, quantile, 0.025),
    upper_95 = apply(ratings_matrix, 2, quantile, 0.975)
  )
  
  # Build ribbon data
  ribbons <- bind_rows(
    rating_data %>% transmute(tc, ymin = lower_95, ymax = upper_95, interval = "95% CI"),
    rating_data %>% transmute(tc, ymin = mean - sd, ymax = mean + sd, interval = "±1 SD")
  )
  
  # Points for predicted ratings
  pts <- all_tiers %>% 
    filter(engine == engine_name) %>% 
    mutate(type = "Mean rating at relevant time control")
  
  plots[[engine_idx]] <-
    ggplot() +
    # Ribbons
    geom_ribbon(
      data = ribbons,
      aes(x = tc, ymin = ymin, ymax = ymax, fill = interval),
      alpha = 0.5
    ) +
    # Mean line
    geom_line(
      data = rating_data,
      aes(x = tc, y = mean, color = "Mean rating"),
      size = 1.2
    ) +
    # Predicted rating points (now included in legend)
    geom_point(
      data = pts,
      aes(x = tc, y = mean_rating, color = type),
      size = 3
    ) +
    # Legend mappings
    scale_fill_manual(
      # *** MODIFICA 1: Rimuovi il titolo ***
      name = NULL,
      values = c("95% CI" = "lightblue", "±1 SD" = "steelblue")
    ) +
    scale_color_manual(
      # *** MODIFICA 2: Rimuovi il titolo ***
      name = NULL,
      values = c(
        "Mean rating" = "darkblue",
        "Mean rating at relevant time control" = "red"
      )
    ) +
    labs(tag = engine_name, x = "Time Control (sec)", y = "Rating") + 
    theme_minimal() +
    theme(
      plot.tag.position = c(0.6,1), 
      plot.tag = element_text(size = 12, face = "bold", hjust = 0.5), 
      legend.position = "none",
      # Increase text size
      axis.text = element_text(size = 14),  # Larger axis tick labels for both x and y
      axis.text.x = element_text(size = 11),  # Smaller x-axis tick labels if needed
      axis.title = element_text(size = 14), # Larger axis titles
      plot.title = element_text(size = 16, face = "bold", margin = margin(r = 10)), # Larger plot title
      legend.text = element_text(size = 12), # Larger legend text
      # Adjust Y-axis label to move it left
      axis.title.y = element_text(margin = margin(r = 10), size = 14),  # Moves Y-axis title to the left
      # Optionally adjust Y-axis tick size if desired
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 10), size = 12)
    )
}

layout_design <- c(
  area(t = 1.1, b = 1.1, l = 1, r = 3), area(t = 2.1, b = 3, l = 1, r = 3) 
)

combined_plot <- wrap_plots(
  A = guide_area(), 
  Plots = wrap_plots(plots, nrow = 2, ncol = 3), 
  design = layout_design
) +
  plot_layout(guides = "collect", heights = c(0.1, 1)) & 
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    # Custom margins
    plot.margin = margin(t = 5, r = 10, b = 15, l = 10), 
    legend.background = element_rect(color = "black", size = 0.5),
    legend.key = element_blank(),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(hjust = 0.5),
    legend.text = element_text(size = 12)
  )

# Main title
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Rating Curves for All Engines",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    )
  )

# Display the plot
print(combined_plot)

ggsave("all_engines_rating_curves.png", combined_plot, width = 16, height = 10, dpi = 300)



# Posterior plots ------------
y_rep = fit$draws("y_rep", format="matrix")

bayesplot::ppc_dens_overlay(stan_data$outcome, y_rep[1500:2000,])




#------------------------------------------
#Basic Posterior Predictive (MODEL CHECKING)
#------------------------------------------

y_rep <- fit$draws("y_rep", format = "matrix")
y <- stan_data$outcome

# 1. Grouped bar plots by engine or time control
ppc_bars_grouped(y, y_rep[500:1000, ], 
                 group = games_processed$black,  # or black, or time control
                 freq = FALSE,  # show proportions instead of counts
                 prob = 0.9)

# 2. Basic distribution comparison grouped by outcome (MOST IMPORTANT for categorical data)
ppc_bars_grouped(y, y_rep[1:1000, ], 
                 group = factor(y, labels = c("Black", "Draw", "White")))

# Distribution Comparisons
# 6. Histogram comparison (first 8 replicates - lecture style)
ppc_hist(y, y_rep[1:8, ])

# 7. Density overlay (already using, but here's the syntax)
ppc_dens_overlay(y, y_rep[1:50, ])

# 8. Empirical CDF differences
ppc_ecdf_overlay_grouped(y, y_rep[1:100, ], group = games_processed$white)

#Probability Integral Transform (PIT)
# 9. Grouped PIT
ppc_pit_ecdf_grouped(y, y_rep, 
                     group = games_processed$white)



