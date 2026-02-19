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
  vector[K] beta_tc;
  real white_advantage;
  real<lower=0, upper=1> p_draw_base;
  real<lower=0> draw_scale;
}

transformed parameters {
  vector[N] rating_white;
  vector[N] rating_black;
  
  for (i in 1:N) {
    rating_white[i] = rating[white_id[i]] + beta_tc[white_id[i]] * tc[i];
    rating_black[i] = rating[black_id[i]] + beta_tc[black_id[i]] * tc[i];
  }
}


model {
  rating ~ normal(2000, 200);
  // anchoring
  mean(rating) ~ normal(2000, 10);
  beta_tc ~ normal(0, 0.1);
  white_advantage ~ normal(35, 15);
  
  p_draw_base ~ beta(3, 7);
  draw_scale ~ normal(300, 100);
  
  for (i in 1:N) {
    real rating_diff = (rating_white[i] + white_advantage) - rating_black[i];
    real abs_diff = abs(rating_diff);
    real expected_score = 1.0 / (1.0 + 10^(-rating_diff / 400.0));
    
    real p_draw = p_draw_base * exp(-abs_diff / draw_scale);
    
    vector[3] probs;
    probs[1] = (1 - p_draw) * (1 - expected_score);
    probs[2] = p_draw;
    probs[3] = (1 - p_draw) * expected_score;
    
    outcome[i] ~ categorical(probs);
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  
  for (i in 1:N) {
    real rating_diff = (rating_white[i] + white_advantage) - rating_black[i];
    real abs_diff = abs(rating_diff);
    real expected_score = 1.0 / (1.0 + 10^(-rating_diff / 400.0));
    
    real p_draw = p_draw_base * exp(-abs_diff / draw_scale);
    
    vector[3] probs;
    probs[1] = (1 - expected_score) * (1 - p_draw);
    probs[2] = p_draw;
    probs[3] = expected_score * (1 - p_draw);
    
    log_lik[i] = categorical_lpmf(outcome[i] | probs);
    y_rep[i] = categorical_rng(probs);
  }
}
