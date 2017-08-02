data {
int<lower=0> N;
vector[N] x;
vector[N] y;
}
parameters {
real alpha;
real beta;
real<lower=0> sigma;
}
model {
  y ~ normal(alpha + beta * x, sigma);
}
generated quantities {
  vector[N] sim_x;
  vector[N] sim_y;

  for(i in 1:N) sim_x[i] = normal_rng(0, 1);
  for(i in 1:N) sim_y[i] = normal_rng(alpha + sim_x[i] * beta, sigma);
}
