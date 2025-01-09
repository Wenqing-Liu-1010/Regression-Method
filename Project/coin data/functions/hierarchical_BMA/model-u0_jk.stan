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
  real<lower=0> sigma_gamma_k;    // standard deviation of the distribution of tossers (on logistic scale)
  real<lower=0> sigma_gamma_j;    // standard deviation of the distribution of coins (on logistic scale)
  real gamma_k[K];                // difference of each individual tosser from the probability of same side (on logistic scale)
  real gamma_j[J];                // difference of each individual coins from the probability of heads (on logistic scale)
}
transformed parameters {
  real mu_heads_up[N];
  real mu_tails_up[N];

  for(n in 1:N){
    mu_heads_up[n] = gamma_j[map_j[n]] * sigma_gamma_j + gamma_k[map_k[n]];
    mu_tails_up[n] = gamma_j[map_j[n]] * sigma_gamma_j - gamma_k[map_k[n]];
  }
}
model {

  target += gamma_lpdf(sigma_gamma_k | sigma_gamma_k_alpha, sigma_gamma_k_beta);
  target += gamma_lpdf(sigma_gamma_j | sigma_gamma_j_alpha, sigma_gamma_j_beta);

  target += normal_lpdf(gamma_k | 0, sigma_gamma_k);
  target += normal_lpdf(gamma_j | 0, 1);

  target += binomial_logit_lupmf(heads_heads | N_start_heads_up, mu_heads_up);
  target += binomial_logit_lupmf(tails_heads | N_start_tails_up, mu_tails_up);
}
