//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data are the vectors of scores and IDs of length N
data {
  int<lower=1> N;
  int<lower=1> K;                           // needed to check that IDs do not exceed the number of engines
  array[N] int<lower=1, upper=K> white_id;
  array[N] int<lower=1, upper=K> black_id;
  // vector<lower=0, upper=1>[N] white_score;
  array[N] int<lower=1, upper=3> outcome;
  vector[N] tc;
}

// The parameters accepted by the model. Our model
// accepts the engines' ratings.
parameters {
  vector[K] rating;
  vector[K] beta;
  real white_advantage;
  real<lower=0, upper=1> p_draw_base;
  real<lower=0> draw_scale;
}

transformed parameters {
  // Calculate time-dependent ratings for each game
  vector[N] rating_white;
  vector[N] rating_black;
  
  for (i in 1:N) {
    rating_white[i] = rating[white_id[i]] + beta[white_id[i]] * tc[i];
    rating_black[i] = rating[black_id[i]] + beta[black_id[i]] * tc[i];
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  rating ~ normal(2000, 200);
  // anchoring
  mean(rating) ~ normal(2000, 10);
  beta ~ normal(0, 0.1);
  white_advantage ~ normal(35, 15);
  
  p_draw_base ~ beta(3, 7);
  draw_scale ~ normal(300, 100);  // centered at 300 with some flexibility
  
  for (i in 1:N) {
    real rating_diff = (rating_white[i] + white_advantage) - rating_black[i];
    real abs_diff = abs(rating_diff);
    real p_white = 1.0 / (1.0 + 10^(-rating_diff / 400.0));
    
    // Draw probability decreases with rating gap
    real p_draw = p_draw_base * exp(-abs_diff / draw_scale);
    
    vector[3] probs;
    probs[1] = (1 - p_draw) * (1 - p_white);
    probs[2] = p_draw;
    probs[3] = (1 - p_draw) * p_white;
    
    outcome[i] ~ categorical(probs);
  }
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  
  for (i in 1:N) {
    real rating_diff = (rating_white[i] + white_advantage) - rating_black[i];
    real abs_diff = abs(rating_diff);
    real p_white = 1.0 / (1.0 + 10^(-rating_diff / 400.0));
    
    // Draw probability decreases with rating gap
    real p_draw = p_draw_base * exp(-abs_diff / draw_scale);
    
    vector[3] probs;
    probs[1] = (1 - p_white) * (1 - p_draw);
    probs[2] = p_draw;
    probs[3] = p_white * (1 - p_draw);
    
    log_lik[i] = categorical_lpmf(outcome[i] | probs);
    y_rep[i] = categorical_rng(probs);
  }
}
