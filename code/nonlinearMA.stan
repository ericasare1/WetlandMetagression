
data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors
  int<lower=0> S; // number of studies
  vector[N] lwtp; // logged wtp
  matrix[N, K] x; // matrix of predictors
  vector[N] q0; // SQ levels
  vector[N] q1; // Policy levels
}

parameters {
  vector[K] beta;
  real gamma;
  real<lower=0> sigma;
}

transformed parameters {
	vector[N] v;

  v = x * beta + log((exp(gamma * q1) - exp(gamma * q0)) / gamma);
 }

model {
  beta ~ normal(0,10);
  gamma ~ normal(0,10);
  sigma ~ inv_gamma(0.5, 0.5);
  lwtp ~ normal(v, sigma);
}
