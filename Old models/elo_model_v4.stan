data {
  int<lower=1> N;
  int<lower=1> K;
  array[N] int<lower=1, upper=K> white_id;
  array[N] int<lower=1, upper=K> black_id;
  array[N] int<lower=1, upper=3> outcome;
  vector[N] tc;
}

parameters {
  vector[K] rating;
  vector[K] beta;
  real white_advantage;
  real alpha_black;          // Changed: explicit Black win baseline
  real alpha_draw;           // Draw baseline
  real alpha_white;          // White win baseline
  real<lower=0> draw_scale;
}

transformed parameters {
  vector[N] rating_white;
  vector[N] rating_black;
  
  for (i in 1:N) {
    rating_white[i] = rating[white_id[i]] + beta[white_id[i]] * tc[i];
    rating_black[i] = rating[black_id[i]] + beta[black_id[i]] * tc[i];
  }
}

model {
  // Priors
  rating ~ normal(2000, 200);
  mean(rating) ~ normal(2000, 10);
  beta ~ normal(0, 0.1);
  
  white_advantage ~ normal(35, 15);
  
  // Symmetric intercepts - let data determine base rates
  alpha_black ~ normal(0, 0.5);
  alpha_draw ~ normal(0, 0.5);
  alpha_white ~ normal(0, 0.5);
  
  draw_scale ~ normal(200, 100);  // Tighter - draws more sensitive to rating gap
  
  for (i in 1:N) {
    real rating_diff = (rating_white[i] + white_advantage) - rating_black[i];
    real abs_diff = abs(rating_diff);
    
    vector[3] utilities;
    utilities[1] = alpha_black - rating_diff / 400.0;      // Black wins
    utilities[2] = alpha_draw - abs_diff / draw_scale;      // Draws
    utilities[3] = alpha_white + rating_diff / 400.0;       // White wins
    
    vector[3] probs = softmax(utilities);
    
    outcome[i] ~ categorical(probs);
  }
}

generated quantities {
  vector[N] log_lik;
  array[N] int y_rep;
  
  for (i in 1:N) {
    real rating_diff = (rating_white[i] + white_advantage) - rating_black[i];
    real abs_diff = abs(rating_diff);
    
    vector[3] utilities;
    utilities[1] = alpha_black - rating_diff / 400.0;
    utilities[2] = alpha_draw - abs_diff / draw_scale;
    utilities[3] = alpha_white + rating_diff / 400.0;
    
    vector[3] probs = softmax(utilities);
    
    log_lik[i] = categorical_lpmf(outcome[i] | probs);
    y_rep[i] = categorical_rng(probs);
  }
}
