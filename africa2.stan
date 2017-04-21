data {
  int<lower=1> N;
  vector[N] l_gdp;
  vector[N] rugged;
  vector[N] africa;
}
transformed data {
  vector[N] inter;
  inter = rugged .* africa; // element wise multiplication
}
parameters {
  vector[4] beta;
  real<lower=0, upper=10> sigma;
}
model {
  beta[1] ~ normal(8, 100);
  beta[2] ~ normal(0, 1);
  beta[3] ~ normal(0, 1);
  beta[4] ~ normal(0, 1);
  sigma ~ uniform(0, 10);
  l_gdp ~ normal(beta[1] + beta[2] * africa + beta[3] * rugged + beta[4] * inter, sigma);
}
generated quantities {
  vector[N] l_gdp_pred;
  for (n in 1:N)
    l_gdp_pred[n] = normal_rng(beta[1] + beta[2] * africa[n] + beta[3] * rugged[n] + beta[4] * inter[n], sigma);
}

