
data {
  int<lower=0> N; // number of observations
  int<lower=0> Nnew; // number of observations
  int<lower=0> K; // number of predictors
  int<lower=0> S; // number of studies
  vector[N] lwtp; // logged wtp
  matrix[N, K] x; // matrix of predictors
  matrix[Nnew, K] xnew; // matrix of predictors
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
  target += normal_lpdf(beta | 0, 10);
  target += normal_lpdf(gamma | 0, 10);
  target += inv_gamma_lpdf(sigma | 0.5, 0.5);
  //likelihood contribution
  for (i in 1:N){
  target += normal_lpdf(y[i] | x[i]* beta + gamma * q01[i], sigma);
  }
}

generated quantities {
  real y_rep[Nnew];
  vector[N] log_lik;
  
  for (n in 1:Nnew) { 
        y_rep[n] = normal_rng(xnew[n] * beta + gamma * (1208345/2) + log(1208345), sigma);
        
        log_lik[n] = normal_lpdf(y[n] | x[n] * beta + gamma * q01[n], sigma);
  }
  
}

