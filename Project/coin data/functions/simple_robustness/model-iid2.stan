#include /functions/functions.stan

data {
  int<lower=0> x; // number of heads
  int<lower=0> n; // number of observation

  // prior settings
  real theta_mode;
}
parameters {
  real theta; // probability of heads
}
model {
  target += momnorm_lpdf(theta | theta_mode);
  target += binomial_logit_lpmf(x | n, theta);
}
