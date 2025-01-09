#include /functions/functions.stan

data {
  int<lower=0> N; // number of observations
  int<lower=0> K; // number of tossers
  int<lower=0> J; // number of coins

  int heads_heads[N];      // number of heads with starting side heads
  int tails_heads[N];      // number of heads with starting side tails
  int N_start_heads_up[N]; // number of trials with starting side heads
  int N_start_tails_up[N]; // number of trials with starting side heads

  int map_k[N];   // mapping from tossers to outcomes
  int map_j[N];   // mapping from coins to outcomes

  // prior settings
  real theta_mode;
  real alpha_mode;
  real sigma_gamma_j_mode;
  real sigma_gamma_k_mode;
}
parameters {
  real<lower=0> theta; // probability of same side
  real          alpha; // probability of heads
}
transformed parameters {
  real mu_heads_up[N];
  real mu_tails_up[N];

  for(n in 1:N){
    mu_heads_up[n] = (alpha) + (theta);
    mu_tails_up[n] = (alpha) - (theta);
  }
}
model {

  target += momnorm_lpdf(theta | theta_mode) - log(0.5);
  target += momnorm_lpdf(alpha | alpha_mode);

  target += binomial_logit_lupmf(heads_heads | N_start_heads_up, mu_heads_up);
  target += binomial_logit_lupmf(tails_heads | N_start_tails_up, mu_tails_up);
}
