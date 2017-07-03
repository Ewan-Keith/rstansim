data {
  int N; //the number of observations
  vector[N] y; //the response
  vector[N] x; //the model matrix
}
parameters {
  real beta; // gradient parameter
  real<lower=0> sigma; //the standard deviation
}
model {
  y ~ normal(x*beta,sigma);
}
