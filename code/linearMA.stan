
data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of predictors
  int<lower=0> S; // number of studies
  vector[N] lwtp; // logged wtp: response variable
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
  // Prior
  beta ~ normal(0,10);
  gamma ~ normal(0,10);
  sigma ~ inv_gamma(0.5, 0.5);
  //likelihood contribution
  y ~ normal(v, sigma);
}

generated quantities {
  real y_rep[N];
  vector[N] log_lik;
  
  for (n in 1:N) { 
        y_rep[n] = normal_rng(x[n] * beta + gamma * q01[n], sigma);
        
        log_lik[n] = normal_lpdf(y[n] | x[n] * beta + gamma * q01[n], sigma);
  }
  
}

