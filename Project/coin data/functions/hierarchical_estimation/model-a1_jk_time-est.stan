#include /functions/functions.stan

data {
  int<lower=0> N; // number of time-aggregated observations
  int<lower=0> K; // number of tossers
  int<lower=0> J; // number of coins

  int heads_heads[N];      // number of heads with starting side heads
  int tails_heads[N];      // number of heads with starting side tails
  int N_start_heads_up[N]; // number of trials with starting side heads
  int N_start_tails_up[N]; // number of trials with starting side heads

  int map_k[N];   // mapping from tossers to outcomes
  int map_j[N];   // mapping from coins to outcomes

  real t[N];

  // prior settings
  int theta_alpha;
  int theta_beta;
  int alpha_alpha;
  int alpha_beta;
  real sigma_gamma_j_sigma;
  real sigma_gamma_k_sigma;
  real bpar_mu;
  real bpar_sigma;
  real cpar_mu;
  real cpar_sigma;
  real gpar_mu;
  real gpar_sigma;
  real sigma_bpar_k_sigma;
  real sigma_cpar_k_sigma;
}
parameters {
  real<lower=0, upper=1> theta;   // probability of same side
  real<lower=0, upper=1> alpha;   // probability of heads
  real<lower=0> sigma_gamma_k;    // standard deviation of the distribution of tossers (on logistic scale)
  real<lower=0> sigma_gamma_j;    // standard deviation of the distribution of coins (on logistic scale)
  real gamma_k[K];                // difference of each individual tosser from the probability of same side (on logistic scale)
  real gamma_j[J];                // difference of each individual coins from the probability of heads (on logistic scale)

  real<lower=0, upper=1> theta2;   // probability of same side
  real<lower=0> sigma_gamma2_k;    // standard deviation of the distribution of tossers (on logistic scale)
  real gamma2_k[K];                // difference of each individual tosser from the probability of same side (on logistic scale)

  // 5 parameter logistic parameters
  real bpar;
  real<lower=0> sigma_bpar_k;
  real bpar_k[K];

//  real cpar;
//  real log_sigma_cpar_k;
//  real cpar_k[K];

//  real gpar;
//  real<lower=0> sigma_gpar_k;
//  real gpar_k[K];
}
transformed parameters {
  real mu_heads_up[N];
  real mu_tails_up[N];

  real temp_alpha_j[J];
  real temp_theta_k[K];
  real temp_theta2_k[K];
  real temp_bpar_k[K];
//  real temp_cpar_k[K];
//  real temp_gpar_k[K];

  for(j in 1:J){
    temp_alpha_j[j] = logit(alpha) + gamma_j[j] * sigma_gamma_j;
  }
  for(k in 1:K){
    temp_theta_k[k]  = logit(theta)  + gamma_k[k] * sigma_gamma_k;
    temp_theta2_k[k] = logit(theta2) + gamma2_k[k] * sigma_gamma2_k;
    temp_bpar_k[k]   = bpar + bpar_k[k] * sigma_bpar_k;
//    temp_cpar_k[k]   = cpar + cpar_k[k];
//    temp_gpar_k[k]   = exp(gpar); //  gpar_k[k] * sigma_gpar_k
  }
  for(n in 1:N){
    real temp_n = temp_theta_k[map_k[n]] + temp_theta2_k[map_k[n]] * pow(t[n], temp_bpar_k[map_k[n]]);//, temp_gpar_k[map_k[n]]);
    mu_heads_up[n] = temp_alpha_j[map_j[n]] + temp_n;
    mu_tails_up[n] = temp_alpha_j[map_j[n]] - temp_n;
  }
}
model {
  target += beta_lpdf(alpha    | alpha_alpha,  alpha_beta);
  target += beta_lpdf(theta    | theta_alpha,  theta_beta);
  target += beta_lpdf(theta2   | theta_alpha,  theta_beta);
  target += normal_lpdf(bpar   | bpar_mu,      bpar_sigma);
//  target += normal_lpdf(cpar   | cpar_mu,      cpar_sigma);
//  target += normal_lpdf(gpar   | gpar_mu,      gpar_sigma);

  target += normal_lpdf(sigma_gamma_k  | 0, sigma_gamma_k_sigma)  - log(0.5);
  target += normal_lpdf(sigma_gamma2_k | 0, sigma_gamma_k_sigma)  - log(0.5);
  target += normal_lpdf(sigma_gamma_j  | 0, sigma_gamma_j_sigma)  - log(0.5);
  target += normal_lpdf(sigma_bpar_k   | 0, sigma_bpar_k_sigma);
//  target += normal_lpdf(log_sigma_cpar_k   | 0, sigma_cpar_k_sigma);
//  target += normal_lpdf(sigma_gpar_k   | 0, sigma_gpar_k_sigma)   - log(0.5);

  target += normal_lpdf(gamma_k  | 0, 1);
  target += normal_lpdf(gamma2_k | 0, 1);
  target += normal_lpdf(gamma_j  | 0, 1);
  target += normal_lpdf(bpar_k   | 0, 1);
//  target += normal_lpdf(cpar_k   | 0, exp(log_sigma_cpar_k));
//  target += normal_lpdf(gpar_k   | 0, 1);

  target += binomial_logit_lupmf(heads_heads | N_start_heads_up, mu_heads_up);
  target += binomial_logit_lupmf(tails_heads | N_start_tails_up, mu_tails_up);
}
