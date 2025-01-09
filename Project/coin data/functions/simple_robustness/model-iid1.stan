#include /functions/functions.stan

data {
  int<lower=0> x; // number of same-side observations
  int<lower=0> n; // number of observation

  // prior settings
  real theta_mode;
}
parameters {
  real<lower=0> theta; // probability of same side
}
model {
  target += momnorm_lpdf(theta | theta_mode) - log(0.5);
  target += binomial_logit_lpmf(x | n, theta);
}
