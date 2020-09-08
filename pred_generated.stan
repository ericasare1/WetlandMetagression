data {
	int N;
	int N_samples;
	int n_col;
	matrix[N, n_col] x;
	vector[N_samples] gamma;
	vector[N_samples] beta;
	vector[N_samples] sigma;
}
parameters{ 
}
model {
}

generated quantities {
  matrix[N_samples, N] y_rep;
  for (n in 1:N) { 
    for(i in 1:N_samples){
              y_rep[i, n] = normal_rng(x[i, n] * beta[i] + gamma[i], sigma[i]);
    }
  }
}
  







