// saved as sim_data_model.stan
data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number of predictors
}
parameters {
  real alpha; // intercept
  vector[K] beta; // coefficients for predictors
  real<lower=0> sigma; // error scale
  corr_matrix[K] omega;
}
transformed parameters{
  cov_matrix[K] cov_omega;

  cov_omega = quad_form_diag(omega, rep_vector(1, K));
}
generated quantities {
  matrix[N, K] sim_x; // simulated predictor data
  vector[N] sim_y; // simulated outcome vector

  for(i in 1:N) sim_x[i,] = multi_normal_rng(rep_vector(0, K), cov_omega)';
  for(i in 1:N) sim_y[i] = normal_rng(alpha + sim_x[i] * beta, sigma)';
}
