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
  vector<lower=0, upper=1>[N] white_score;
}

// The parameters accepted by the model. Our model
// accepts the engines' ratings.
parameters {
  vector[K] rating;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  rating ~ normal(2000, 200);     // parameters' prior

  for (i in 1:N) {
    real rating_delta = rating[white_id[i]] - rating[black_id[i]];
    real expected_score = 1.0 / (1.0 + 10^(-rating_delta / 400.0));
    white_score[i] ~ normal(expected_score, 0.25);
  }
}

generated quantities {
  vector[N] log_lik;
  
  for (i in 1:N) {
    real rating_delta = rating[white_id[i]] - rating[black_id[i]];
    real expected_score = 1.0 / (1.0 + 10^(-rating_delta / 400.0));
    log_lik[i] = normal_lpdf(white_score[i] | expected_score, 0.25); //synthax: data | mean, sd
    // This line computes how well the model predicted each individual game outcome, 
    // which is useful for model evaluation and comparison. It tells us how "surprised" 
    // the model is by this outcome - higher (less negative) values mean better predictions.
  }
}
