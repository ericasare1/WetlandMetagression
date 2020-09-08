data {
  int<lower = 1> N_train;
  vector[N_train] x_train;
  int<lower = 0, upper = 1> y_train[N_train];
  int<lower = 1> N_test;
  vector[N_test] x_test;
}
parameters {
  real alpha;
  real beta;
}
model {
  y_train ~ bernoulli_logit(alpha + beta*x_train);
  alpha ~ normal(5, 10);
  beta ~ normal(5, 10);
}
generated quantities {
  vector[N_test] y_test;
  for(i in 1:N_test) {
    y_test[i] = bernoulli_rng(inv_logit(alpha + beta*x_test[i]));
  }
} 
