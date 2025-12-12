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
  array[N] int<lower=0, upper=2> outcome;
  vector[N] tc;
}


// The parameters accepted by the model. Our model
// accepts the engines' ratings.
parameters {
  vector[K] rating;
  vector[K] beta;
}


transformed parameters {
  // Pre-calculate ALL ratings as vectors
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
  // parameters' prior
  rating ~ normal(2000, 200);     
  beta ~ normal(0, 0.1);
    
  // anchor raiting mean around 2000 ("hyperprior")
  mean(rating) ~ normal(2000, 10);

  for (i in 1:N) {
    real rating_diff = rating_white[i] - rating_black[i];
    real expected_score = 1.0 / (1.0 + 10^(-rating_diff / 400.0));
    
    outcome[i] ~ binomial(2, expected_score);
  }
}


generated quantities {
  vector[N] log_lik;
  
  for (i in 1:N) {
    real rating_diff = rating_white[i] - rating_black[i];
    real expected_score = 1.0 / (1.0 + 10^(-rating_diff / 400.0));
    
    log_lik[i] = binomial_lpmf(outcome[i] | 2, expected_score);
  }
}
