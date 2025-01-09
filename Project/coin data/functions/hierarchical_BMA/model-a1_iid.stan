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
  int theta_alpha;
  int theta_beta;
  int alpha_alpha;
  int alpha_beta;
  int sigma_gamma_j_alpha;
  int sigma_gamma_j_beta;
  int sigma_gamma_k_alpha;
  int sigma_gamma_k_beta;
}
parameters {
  real<lower=0.5, upper=1> theta; // probability of same side
  real<lower=0,   upper=1> alpha; // probability of heads
}
transformed parameters {
  real mu_heads_up[N];
  real mu_tails_up[N];

  for(n in 1:N){
    mu_heads_up[n] = logit(alpha) + logit(theta);
    mu_tails_up[n] = logit(alpha) - logit(theta);
  }
}
model {

  target += beta_lpdf(theta | theta_alpha, theta_beta) - beta_lccdf(0.5 | theta_alpha, theta_beta);
  target += beta_lpdf(alpha | alpha_alpha, alpha_beta);

  target += binomial_logit_lupmf(heads_heads | N_start_heads_up, mu_heads_up);
  target += binomial_logit_lupmf(tails_heads | N_start_tails_up, mu_tails_up);
}
