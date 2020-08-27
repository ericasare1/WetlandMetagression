
data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors
  int<lower=0> S; // number of studies
  vector[N] lwtp; // logged wtp
  matrix[N, K] x; // matrix of predictors
  vector[N] q0; // SQ levels
  vector[N] q1; // Policy levels
}

transformed data{
  vector[N] y;
  vector[N] q01;
  
  y = lwtp - log(q1- q0);
  
  q01 = (q0 + q1) / 2;
  
}

parameters {
  vector[K] beta;
  real gamma;
  real<lower=0> sigma;
}

transformed parameters {
	vector[N] v; 

  v = x * beta + gamma * q01;
}

model {
  beta ~ normal(0,10);
  gamma ~ normal(0,10);
  sigma ~ inv_gamma(0.5, 0.5);
  y ~ normal(v, sigma);
}
generated quantities {
  vector[N] y_new;
  for (n in 1:N)
    y_new[n] = normal_rng(x[n] * beta + gamma*q01[n], sigma);
}





