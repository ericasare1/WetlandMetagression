
data {
  int<lower=0> Nnew; // number of observations
  int<lower=0> N; // number of observations
  int<lower=0> Knew; // number of predictors
  int<lower=0> K; // number of predictors
  int<lower=0> S; // number of studies
  vector[N] lwtp; // logged wtp: response variable
  matrix[N, K] x; // matrix of predictors
  matrix[Nnew, Knew] xnew; // matrix of predictors
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
  real y_rep[Nnew];
  for (n in 1:Nnew) { 
        y_rep[n] = normal_rng(x[n] * beta + gamma * q01[n] + log(q1[n]- q0[n]), sigma);
  }
  
}

