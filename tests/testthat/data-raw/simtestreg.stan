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
  vector[N] stansim_x;
  vector[N] stansim_y;

  for(i in 1:N) stansim_x[i] = normal_rng(0, 1);
  for(i in 1:N) stansim_y[i] = normal_rng(alpha + stansim_x[i] * beta, 1);
}
