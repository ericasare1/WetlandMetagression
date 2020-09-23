
data {
  int<lower=0> N; // number of observations
  int<lower=0> Nnew; // number of observations
  int<lower=0> K; // number of predictors
  int<lower=0> S; // number of studies
  vector[N] lwtp; // logged wtp
  matrix[Nnew, K] xnew; // matrix of predictors
  matrix[N, K] x; // matrix of predictors
  vector[N] q0; // SQ levels
  vector[N] q1; // Policy levels
  vector[Nnew] q0new; // SQ levels
  vector[Nnew] q1new; // Policy levels

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
  // Prior
  target += normal_lpdf(beta | 0, 10);
  target += normal_lpdf(gamma | 0, 10);
  target += inv_gamma_lpdf(sigma | 0.5, 0.5);
  //likelihood contribution
  for (i in 1:N){
  target += normal_lpdf(lwtp[i] | x[i] * beta + log((exp(gamma * q1[i]) - exp(gamma * q0[i])) / gamma), sigma);
  }
}

generated quantities {
  real y_rep[Nnew];
  vector[N] log_lik;
  
  for (n in 1:Nnew) { 
        y_rep[n] = normal_rng(xnew[n] * beta + log((exp(gamma * q1new[n]) - exp(gamma * q0new[n])) / gamma), sigma);
}
  for (n in 1:N) { 
        log_lik[n] = normal_lpdf(lwtp[n] | x[n] * beta + log((exp(gamma * q1[n]) - exp(gamma * q0[n])) / gamma), sigma);
  }
  
}
