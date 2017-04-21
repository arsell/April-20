data {
  int<lower=1> N;
  vector[N] l_gdp;
  vector[N] rugged;
}
parameters {
  vector[2] beta;
  real<lower=0, upper=10> sigma; // this needs to match the uniform below
}
model {
  beta[1] ~ normal(8, 100); // this indexes to the beta vector
  beta[2] ~ normal(0, 1); // dropping these priors makees an improper prior because integral is infinity
  sigma ~ uniform(0, 10); // typically a bad idea to use distributions with hard cutoffs (like the uniform)
  l_gdp ~ normal(beta[1] + beta[2] * rugged, sigma);
}

