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
  real<lower=0> theta;            // probability of same side
  real<lower=0> sigma_gamma_k;    // standard deviation of the distribution of tossers (on logistic scale)
  real gamma_k[K];                // difference of each individual tosser from the probability of same side (on logistic scale)
}
transformed parameters {
  real mu_heads_up[N];
  real mu_tails_up[N];

  for(n in 1:N){
    mu_heads_up[n] = + ((theta) + gamma_k[map_k[n]]);
    mu_tails_up[n] = - ((theta) + gamma_k[map_k[n]]);
  }
}
model {

  target += momnorm_lpdf(theta | theta_mode) - log(0.5);

  target += momnorm_lpdf(sigma_gamma_k | sigma_gamma_k_mode) - log(0.5);

  target += normal_lpdf(gamma_k | 0, sigma_gamma_k);

  target += binomial_logit_lupmf(heads_heads | N_start_heads_up, mu_heads_up);
  target += binomial_logit_lupmf(tails_heads | N_start_tails_up, mu_tails_up);
}
