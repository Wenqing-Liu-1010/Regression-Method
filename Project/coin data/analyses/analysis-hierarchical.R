### this script performs hierarchical analyses (and produces corresponding figures) ----
# these analyses are computationally heavy --- many sub-results are saved in the analyses/outputs folder
# and can be loaded to skip the sampling steps

# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(rstan) # rstan version 2.26.22 (Stan version 2.26.1)
library(bridgesampling)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)
source(file = "functions/binomial-test.R")

df     <- read.csv(file = "analyses/data_long.csv")
df_agg <- read.csv(file = "analyses/data_agg.csv")

### non-preregistered analysis accounting for the both heads/tails and same-side bias and variation between coins and tossers ----
# create a stan data object
data_stan <- with(df_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(df_agg)
))

# specify prior distributions
data_stan$theta_alpha          <- 5100
data_stan$theta_beta           <- 4900
data_stan$alpha_alpha          <- 5000
data_stan$alpha_beta           <- 5000
data_stan$sigma_gamma_j_alpha  <- 4
data_stan$sigma_gamma_j_beta   <- 200
data_stan$sigma_gamma_k_alpha  <- 4
data_stan$sigma_gamma_k_beta   <- 200

# sd of random effects on logit scale:       4/200         = 0.02 -> 0.0050
# sd of sd of random effects on logit scale: sqrt(4/200^2) = 0.01 -> 0.0025

# compile stan models:
# a = accounts for overall heads/tails bias, u = does not account for overall heads/tails bias
# 1 = accounts for overall same-side bias, 0 = does not account for overall same-side bias
# iid = no coin/person effects, j = coin effects, k = person effects
# the coin effects use non-centered parameterization (the sampling works much better)

model_iid    <- rstan::stan_model("functions/hierarchical_BMA/model-u1_iid.stan")
model_j      <- rstan::stan_model("functions/hierarchical_BMA/model-u1_j.stan")
model_k      <- rstan::stan_model("functions/hierarchical_BMA/model-u1_k.stan")
model_jk     <- rstan::stan_model("functions/hierarchical_BMA/model-u1_jk.stan")
model0_j     <- rstan::stan_model("functions/hierarchical_BMA/model-u0_j.stan")
model0_k     <- rstan::stan_model("functions/hierarchical_BMA/model-u0_k.stan")
model0_jk    <- rstan::stan_model("functions/hierarchical_BMA/model-u0_jk.stan")
model_a_iid  <- rstan::stan_model("functions/hierarchical_BMA/model-a1_iid.stan")
model_a_j    <- rstan::stan_model("functions/hierarchical_BMA/model-a1_j.stan")
model_a_k    <- rstan::stan_model("functions/hierarchical_BMA/model-a1_k.stan")
model_a_jk   <- rstan::stan_model("functions/hierarchical_BMA/model-a1_jk.stan")
model0_a_iid <- rstan::stan_model("functions/hierarchical_BMA/model-a0_iid.stan")
model0_a_j   <- rstan::stan_model("functions/hierarchical_BMA/model-a0_j.stan")
model0_a_k   <- rstan::stan_model("functions/hierarchical_BMA/model-a0_k.stan")
model0_a_jk  <- rstan::stan_model("functions/hierarchical_BMA/model-a0_jk.stan")
# marglik for u0_iid can be easily computed analytically

fit_iid    <- rstan::sampling(object = model_iid   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_j      <- rstan::sampling(object = model_j     ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_k      <- rstan::sampling(object = model_k     ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_jk     <- rstan::sampling(object = model_jk    ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_j     <- rstan::sampling(object = model0_j    ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_k     <- rstan::sampling(object = model0_k    ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_jk    <- rstan::sampling(object = model0_jk   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_a_iid  <- rstan::sampling(object = model_a_iid ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_a_j    <- rstan::sampling(object = model_a_j   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_a_k    <- rstan::sampling(object = model_a_k   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit_a_jk   <- rstan::sampling(object = model_a_jk  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_a_iid <- rstan::sampling(object = model0_a_iid,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_a_j   <- rstan::sampling(object = model0_a_j  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_a_k   <- rstan::sampling(object = model0_a_k  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
fit0_a_jk  <- rstan::sampling(object = model0_a_jk ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))

set.seed(1)
marglik_iid  <- bridgesampling::bridge_sampler(fit_iid)$logml
marglik_j    <- bridgesampling::bridge_sampler(fit_j)$logml
marglik_k    <- bridgesampling::bridge_sampler(fit_k)$logml
marglik_jk   <- bridgesampling::bridge_sampler(fit_jk)$logml
marglik0_iid <- sum(dbinom(x = df$toss_end == "h", size = 1, prob = 0.50, log = TRUE))
marglik0_j   <- bridgesampling::bridge_sampler(fit0_j)$logml
marglik0_k   <- bridgesampling::bridge_sampler(fit0_k)$logml
marglik0_jk  <- bridgesampling::bridge_sampler(fit0_jk)$logml

marglik_a_iid  <- bridgesampling::bridge_sampler(fit_a_iid)$logml
marglik_a_j    <- bridgesampling::bridge_sampler(fit_a_j)$logml
marglik_a_k    <- bridgesampling::bridge_sampler(fit_a_k)$logml
marglik_a_jk   <- bridgesampling::bridge_sampler(fit_a_jk)$logml
marglik0_a_iid <- bridgesampling::bridge_sampler(fit0_a_iid)$logml
marglik0_a_j   <- bridgesampling::bridge_sampler(fit0_a_j)$logml
marglik0_a_k   <- bridgesampling::bridge_sampler(fit0_a_k)$logml
marglik0_a_jk  <- bridgesampling::bridge_sampler(fit0_a_jk)$logml


margliks <- c(
  "0_iid"   = marglik0_iid,
  "0_j"     = marglik0_j,
  "0_k"     = marglik0_k,
  "0_jk"    = marglik0_jk,
  "1_iid"   = marglik_iid,
  "1_j"     = marglik_j,
  "1_k"     = marglik_k,
  "1_jk"    = marglik_jk,
  "a_0_iid" = marglik0_a_iid,
  "a_0_j"   = marglik0_a_j,
  "a_0_k"   = marglik0_a_k,
  "a_0_jk"  = marglik0_a_jk,
  "a_1_iid" = marglik_a_iid,
  "a_1_j"   = marglik_a_j,
  "a_1_k"   = marglik_a_k,
  "a_1_jk"  = marglik_a_jk
)

saveRDS(margliks, file = "analyses/margliks.RDS")
margliks <- readRDS(file = "analyses/outputs/margliks.RDS")

# j = coins bias
# k = variation by tossers
BF_effect  <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks,
                                       is_null = rep(c(rep(T, 4), rep(F, 4)), 2))   # 2359.008
BF_tossers <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks,
                                       is_null = rep(c(T, T, F, F, T, T, F, F), 2)) # 3.101275e+24
BF_heads   <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks,
                                       is_null = c(rep(T, 8), rep(F, 8)))           # 0.18231
BF_coins   <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks,
                                       is_null = rep(c(T, F, T, F, T, F, T, F), 2)) # 0.1781013

# table with model description
exp_margliks <- exp(margliks - max(margliks))
post_prob    <- exp_margliks/sum(exp_margliks)
models_table <- cbind.data.frame(
  "same-side bias"               = ifelse(!rep(c(rep(T, 4), rep(F, 4)), 2), "Yes", "No"),
  "heads-tails bias"               = ifelse(!c(rep(T, 8), rep(F, 8)), "Yes", "No"),
  "heterogeneity same-side bias" = ifelse(!rep(c(T, T, F, F, T, T, F, F), 2), "Yes", "No"),
  "heterogeneity heads-tails bias" = ifelse(!rep(c(T, F, T, F, T, F, T, F), 2), "Yes", "No"),
  "Prior Pr."       = rep(1/16, 16),
  "log(Marg. Lik.)" = as.character(round(margliks, 2)),
  "Post. Pr."       = post_prob
)
models_table <- models_table[16:1,]
models_table <- models_table[c(1:4, 9:12, 5:8, 13:16),]
xtable::xtable(models_table, digits = 4, rownames = F)

### sensitivity analysis to outliers ----
person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
  data.frame(
    person = person,
    same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
    N      = sum(df$person == person)
  )
}))
person_flips <- person_flips[order(person_flips$N, decreasing = FALSE),]

# remove people with more than 0.53% same side bias
to_remove <- person_flips$person[person_flips$same / person_flips$N > 0.53]
s_df_agg  <- df_agg[!df_agg$person %in% to_remove,]

s_data_stan <- with(s_df_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(s_df_agg)
))

s_data_stan$theta_alpha          <- 5100
s_data_stan$theta_beta           <- 4900
s_data_stan$alpha_alpha          <- 5000
s_data_stan$alpha_beta           <- 5000
s_data_stan$sigma_gamma_j_alpha  <- 4
s_data_stan$sigma_gamma_j_beta   <- 200
s_data_stan$sigma_gamma_k_alpha  <- 4
s_data_stan$sigma_gamma_k_beta   <- 200

s_fit_iid    <- rstan::sampling(object = model_iid   ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_j      <- rstan::sampling(object = model_j     ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_k      <- rstan::sampling(object = model_k     ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_jk     <- rstan::sampling(object = model_jk    ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_j     <- rstan::sampling(object = model0_j    ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_k     <- rstan::sampling(object = model0_k    ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_jk    <- rstan::sampling(object = model0_jk   ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_a_iid  <- rstan::sampling(object = model_a_iid ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_a_j    <- rstan::sampling(object = model_a_j   ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_a_k    <- rstan::sampling(object = model_a_k   ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit_a_jk   <- rstan::sampling(object = model_a_jk  ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_a_iid <- rstan::sampling(object = model0_a_iid,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_a_j   <- rstan::sampling(object = model0_a_j  ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_a_k   <- rstan::sampling(object = model0_a_k  ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
s_fit0_a_jk  <- rstan::sampling(object = model0_a_jk ,data = s_data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))

set.seed(1)
s_marglik_iid  <- bridgesampling::bridge_sampler(s_fit_iid)$logml
s_marglik_j    <- bridgesampling::bridge_sampler(s_fit_j)$logml
s_marglik_k    <- bridgesampling::bridge_sampler(s_fit_k)$logml
s_marglik_jk   <- bridgesampling::bridge_sampler(s_fit_jk)$logml
s_marglik0_iid <- sum(dbinom(x = df$toss_end[!df$person %in% to_remove] == "h", size = 1, prob = 0.50, log = TRUE))
s_marglik0_j   <- bridgesampling::bridge_sampler(s_fit0_j)$logml
s_marglik0_k   <- bridgesampling::bridge_sampler(s_fit0_k)$logml
s_marglik0_jk  <- bridgesampling::bridge_sampler(s_fit0_jk)$logml

s_marglik_a_iid  <- bridgesampling::bridge_sampler(s_fit_a_iid)$logml
s_marglik_a_j    <- bridgesampling::bridge_sampler(s_fit_a_j)$logml
s_marglik_a_k    <- bridgesampling::bridge_sampler(s_fit_a_k)$logml
s_marglik_a_jk   <- bridgesampling::bridge_sampler(s_fit_a_jk)$logml
s_marglik0_a_iid <- bridgesampling::bridge_sampler(s_fit0_a_iid)$logml
s_marglik0_a_j   <- bridgesampling::bridge_sampler(s_fit0_a_j)$logml
s_marglik0_a_k   <- bridgesampling::bridge_sampler(s_fit0_a_k)$logml
s_marglik0_a_jk  <- bridgesampling::bridge_sampler(s_fit0_a_jk)$logml

s_margliks <- c(
  "0_iid"   = s_marglik0_iid,
  "0_j"     = s_marglik0_j,
  "0_k"     = s_marglik0_k,
  "0_jk"    = s_marglik0_jk,
  "1_iid"   = s_marglik_iid,
  "1_j"     = s_marglik_j,
  "1_k"     = s_marglik_k,
  "1_jk"    = s_marglik_jk,
  "a_0_iid" = s_marglik0_a_iid,
  "a_0_j"   = s_marglik0_a_j,
  "a_0_k"   = s_marglik0_a_k,
  "a_0_jk"  = s_marglik0_a_jk,
  "a_1_iid" = s_marglik_a_iid,
  "a_1_j"   = s_marglik_a_j,
  "a_1_k"   = s_marglik_a_k,
  "a_1_jk"  = s_marglik_a_jk
)

saveRDS(s_margliks, file = "analyses/outputs/s_margliks.RDS")
s_margliks <- readRDS(file = "analyses/outputs/s_margliks.RDS")

# j = coins bias
# k = variation by tossers
s_BF_effect  <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = s_margliks,
                                         is_null = rep(c(rep(T, 4), rep(F, 4)), 2))   # 786.8489
s_BF_heads   <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = s_margliks,
                                         is_null = c(rep(T, 8), rep(F, 8)))           # 0.2129247
s_BF_coins   <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = s_margliks,
                                         is_null = rep(c(T, F, T, F, T, F, T, F), 2)) # 0.2206385
s_BF_tossers <- BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = s_margliks,
                                         is_null = rep(c(T, T, F, F, T, T, F, F), 2)) # 28700349

# table with model description
s_exp_margliks <- exp(s_margliks - max(s_margliks))
s_post_prob    <- s_exp_margliks/sum(s_exp_margliks)
s_models_table <- cbind.data.frame(
  "same-side bias"               = ifelse(!rep(c(rep(T, 4), rep(F, 4)), 2), "Yes", "No"),
  "heads-tails bias"               = ifelse(!c(rep(T, 8), rep(F, 8)), "Yes", "No"),
  "heterogeneity same-side bias" = ifelse(!rep(c(T, T, F, F, T, T, F, F), 2), "Yes", "No"),
  "heterogeneity heads-tails bias" = ifelse(!rep(c(T, F, T, F, T, F, T, F), 2), "Yes", "No"),
  "Prior Pr."       = rep(1/16, 16),
  "log(Marg. Lik.)" = as.character(round(s_margliks, 2)),
  "Post. Pr."       = s_post_prob
)
s_models_table <- s_models_table[16:1,]
s_models_table <- s_models_table[c(1:4, 9:12, 5:8, 13:16),]
xtable::xtable(s_models_table, digits = 4, rownames = F)



### estimation only with the most complex model and manuscript figures ----
data_stan_est <- with(df_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(df_agg)
))

data_stan_est$theta_alpha          <- 312
data_stan_est$theta_beta           <- 312
data_stan_est$alpha_alpha          <- 312
data_stan_est$alpha_beta           <- 312
data_stan_est$sigma_gamma_j_sigma  <- 0.04
data_stan_est$sigma_gamma_k_sigma  <- 0.04
# sd of main effects 0.02
# sd of random effects on logit scale:  0.04 -> 0.01

model_full <- rstan::stan_model("functions/hierarchical_estimation/model-a1_jk-est.stan")
set.seed(1)
fit_full     <- rstan::sampling(object = model_full, data = data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                                seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
saveRDS(fit_full, file = "analyses/outputs/fit_full-est.RDS", compress = "xz")
fit_full <- readRDS(file = "analyses/outputs/fit_full-est.RDS")

samples_full <- rstan::extract(fit_full)

# check convergence
summary_fit_full <- summary(fit_full, pars = c("theta", "alpha", "sigma_gamma_k", "sigma_gamma_j"))
summary_fit_full$summary

# check chains
rstan::traceplot(fit_full, pars = "theta")
rstan::traceplot(fit_full, pars = "alpha")
rstan::traceplot(fit_full, pars = "sigma_gamma_k")
rstan::traceplot(fit_full, pars = "sigma_gamma_j")

rstan::stan_dens(fit_full, pars = "theta", separate_chains = TRUE)
rstan::stan_dens(fit_full, pars = "alpha", separate_chains = TRUE)
rstan::stan_dens(fit_full, pars = "sigma_gamma_k", separate_chains = TRUE)
rstan::stan_dens(fit_full, pars = "sigma_gamma_j", separate_chains = TRUE)

# report estimates
report_est(samples_full$theta) # "0.5098 [0.5050, 0.5147]"
report_est(samples_full$alpha) # "0.5005 [0.4986, 0.5026]"
report_est(inv_logit(samples_full$sigma_gamma_k) - 0.50) # "0.0156 [0.0119, 0.0200]"
report_est(inv_logit(samples_full$sigma_gamma_j) - 0.50) # "0.0018 [0.0001, 0.0047]"

# tossers estimates
toss_est <- inv_logit(matrix(logit(samples_full$theta), ncol = ncol(samples_full$gamma_k), nrow = nrow(samples_full$gamma_k)) + samples_full$gamma_k)
colnames(toss_est) <- levels(factor(df_agg$person))
person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
  data.frame(
    person = person,
    same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
    N      = sum(df$person == person)
  )
}))
person_flips <- person_flips[order(person_flips$same/person_flips$N, decreasing = FALSE),]


pdf("figures/panel1.pdf", width = 10, height = 8)

layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(1, 0.75))
par(mar = c(0, 4.5, 0, 0.5))
plot(NA, type = "n", main = "", ylab = "", xlab = "", ylim = c(0.40, 0.60), xlim = c(0, nrow(person_flips) + 1), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(2, at = seq(0.40, 0.60, 0.05), las = 1, cex.axis = 1.10)
mtext("Pr(same side)", 2, line = 3)
for(i in 1:nrow(person_flips)){

  temp_person <- person_flips$person[i]

  # descriptive
  temp_des  <- binom.test(person_flips$same[i], person_flips$N[i])

  lines(rep(i - 1, 2) - 0.20  , temp_des$conf.int, lwd = 1.5, col = "darkblue")
   points(i - 1 - 0.20        , temp_des$estimate, pch = 21, bg  = "white", cex = 1, col = "darkblue")

  # estimates
  temp_den    <- density(toss_est[,temp_person])

  temp_den.y <- temp_den$y[temp_den$x<0.60] / 150
  temp_den.x <- temp_den$x[temp_den$x<0.60]

  # polygon(c(0, temp_den.y, 0) + (i - 1)           , c(temp_den.x[1], temp_den.x, temp_den.x[length(temp_den.x)]), col = "grey", border = FALSE)
  points (i - 1                                   , mean(toss_est[,temp_person]), pch = 16, cex = 1.25, col = "black")
  lines  (rep(i - 1, 2)                           , quantile(toss_est[,temp_person], c(0.025, 0.975)), lwd = 1.5, col = "black")
}

lines(c(0, nrow(person_flips) + 1), c(0.50, 0.50), lty = 3)
lines(c(0, nrow(person_flips) + 1), c(0.51, 0.51), lty = 3)
text(nrow(person_flips)  + 1, 0.50 - 0.002, "chance",   adj = c(1, 1), cex = 1.25)
text(nrow(person_flips)  + 1, 0.51 + 0.002, "DHM", adj = c(1, 0), cex = 1.25)

legend(x = 0, y = 0.60, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"), pch = c(21, 16), col = c("darkblue", "black"), bty = "n", lty = 1, bg = c("white", "black"),
       cex = 1.25)


par(mar = c(4, 4.5, 0, 0))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0.46, 0.54), ylim = c(0, 250), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.46, 0.54, 0.02), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.10)
mtext("Density", 2, line = 3)
mtext("Pr(same side)", 1, line = 2.5)

x_prior <- seq(0.46, 0.54, length.out = 501)
d_prior <- dbeta(x = x_prior, shape1 = 312, shape2 = 312)
lines(x_prior, d_prior, lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(samples_full$theta)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

lines(c(0.50, 0.50), c(0, 250), lty = 3)
lines(c(0.51, 0.51), c(0, 250), lty = 3)

text(0.50 - 0.001, 250, "chance",   adj = c(1, 1), cex = 1.25)
text(0.51 + 0.001, 250, "DHM", adj = c(0, 1), cex = 1.25)


par(mar = c(4, 4, 0, 0.5))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0, 0.03), ylim = c(0, 250), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0, 0.03, 0.01), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.10)
mtext("Between-people heterogeneity in Pr(same side)", 1, line = 2.5)

x_prior <- seq(0, logit(0.53), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.04)*2

lines(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(inv_logit(samples_full$sigma_gamma_k) - 0.50, lbound = 0)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

legend(x = 0.02, y = 250, c("Prior", "Posterior"), lwd = 2, lty = c(2, 1), col = c("grey", "black"), bty = "n", cex = 1.25)
dev.off()


# coin estimates
toss_est <- inv_logit(matrix(logit(samples_full$alpha), ncol = ncol(samples_full$gamma_j), nrow = nrow(samples_full$gamma_j)) +
                        samples_full$gamma_j * matrix(samples_full$sigma_gamma_j, ncol = ncol(samples_full$gamma_j), nrow = nrow(samples_full$gamma_j)))
colnames(toss_est) <- levels(factor(df_agg$coin))
coin_flips <- do.call(rbind, lapply(unique(df$coin), function(coin){
  data.frame(
    coin  = coin,
    heads = sum(df$toss_end[df$coin == coin] == "h"),
    N     = sum(df$coin == coin)
  )
}))
coin_flips <- coin_flips[order(coin_flips$heads / coin_flips$N, decreasing = FALSE),]

pdf("figures/panel2.pdf", width = 10, height = 8)

layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(1, 0.75))
par(mar = c(0, 4.5, 0, 0.5))
plot(NA, type = "n", main = "", ylab = "", xlab = "", ylim = c(0.40, 0.60), xlim = c(0, nrow(coin_flips) + 1), las = 1, bty = "n", yaxt = "n", xaxt = "n")
mtext("Pr(heads)", 2, line = 3)
axis(2, at = seq(0.40, 0.60, 0.05), las = 1, cex.axis = 1.10)

for(i in 1:nrow(coin_flips)){

  temp_coin <- coin_flips$coin[i]

  # descriptive
  temp_des  <- binom.test(coin_flips$heads[i], coin_flips$N[i])

  lines(rep(i - 1, 2) - 0.20  , temp_des$conf.int, lwd = 1.5, col = "darkblue")
  points(i - 1 - 0.20         , temp_des$estimate, pch = 21, bg = "white", cex = 1, col = "darkblue")

  # estimates
  temp_den    <- density(toss_est[,temp_coin])

  temp_den.y <- temp_den$y[temp_den$x<0.60] / 250
  temp_den.x <- temp_den$x[temp_den$x<0.60]

  # polygon(c(0, temp_den.y, 0) + (i - 1)           , c(temp_den.x[1], temp_den.x, temp_den.x[length(temp_den.x)]), col = "grey", border = FALSE)
  points (i - 1                                   , mean(toss_est[,temp_coin]), pch = 16, cex = 1.25, col = "black")
  lines  (rep(i - 1, 2)                           , quantile(toss_est[,temp_coin], c(0.025, 0.975)), lwd = 1.5, col = "black")
}

lines(c(0, nrow(coin_flips) + 1), c(0.50, 0.50), lty = 3)
text(nrow(coin_flips)  + 1, 0.50 - 0.005, "chance",   adj = c(1, 1), cex = 1.25)

legend(x = 0, y = 0.60, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"), pch = c(21, 16), col = c("darkblue", "black"), bty = "n", lty = 1, bg = c("white", "black"),
       cex = 1.25)


par(mar = c(4, 4.5, 0, 0))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0.46, 0.54), ylim = c(0, 400), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.46, 0.54, 0.02), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 400, 100), las = 1, cex.axis = 1.10)
mtext("Density", 2, line = 3)
mtext("Pr(heads)", 1, line = 2.5)

x_prior <- seq(0.46, 0.54, length.out = 501)
d_prior <- dbeta(x = x_prior, shape1 = 312, shape2 = 312)
lines(x_prior, d_prior, lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(samples_full$alpha)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

lines(c(0.50, 0.50), c(0, 400), lty = 3)
text(0.50 - 0.001, 400, "chance",   adj = c(1, 1), cex = 1.25)


par(mar = c(4, 4, 0, 0.5))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0, 0.03), ylim = c(0, 400), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0, 0.03, 0.01), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 400, 100), las = 1, cex.axis = 1.10)
mtext("Between-coin heterogemeity in Pr(heads)", 1, line = 2.5)

x_prior <- seq(0, logit(0.53), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.04)*2

lines(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(inv_logit(samples_full$sigma_gamma_j) - 0.50, lbound = 0)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

legend(x = 0.02, y = 400, c("Prior", "Posterior"), lwd = 2, lty = c(2, 1), col = c("grey", "black"), bty = "n", cex = 1.25)
dev.off()

### sensitivity analysis to outliers ----
person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
  data.frame(
    person = person,
    same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
    N      = sum(df$person == person)
  )
}))
person_flips <- person_flips[order(person_flips$N, decreasing = FALSE),]

# robustness check by removing people with more than 0.53% same side bias
to_remove <- person_flips$person[person_flips$same / person_flips$N > 0.53]
s_df_agg  <- df_agg[!df_agg$person %in% to_remove,]
s_df      <- df[!df$person %in% to_remove,]

s_data_stan_est <- with(s_df_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(s_df_agg)
))


s_data_stan_est$theta_alpha          <- 312
s_data_stan_est$theta_beta           <- 312
s_data_stan_est$alpha_alpha          <- 312
s_data_stan_est$alpha_beta           <- 312
s_data_stan_est$sigma_gamma_j_sigma  <- 0.04
s_data_stan_est$sigma_gamma_k_sigma  <- 0.04
# sd of main effects 0.02
# sd of random effects on logit scale:  0.04 -> 0.01

model_full <- rstan::stan_model("functions/hierarchical_estimation/model-a1_jk-est.stan")
set.seed(1)
s_fit_full     <- rstan::sampling(object = model_full, data = s_data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                                  seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
saveRDS(s_fit_full, file = "analyses/outputs/s_fit_full-est.RDS", compress = "xz")
s_fit_full <- readRDS(file = "analyses/outputs/s_fit_full-est.RDS")

s_samples_full <- rstan::extract(s_fit_full)

# check convergence
s_summary_fit_full <- summary(s_fit_full, pars = c("theta", "alpha", "sigma_gamma_k", "sigma_gamma_j"))
s_summary_fit_full$summary

# check chains
rstan::traceplot(s_fit_full, pars = "theta")
rstan::traceplot(s_fit_full, pars = "alpha")
rstan::traceplot(s_fit_full, pars = "sigma_gamma_k")
rstan::traceplot(s_fit_full, pars = "sigma_gamma_j")

rstan::stan_dens(s_fit_full, pars = "theta", separate_chains = TRUE)
rstan::stan_dens(s_fit_full, pars = "alpha", separate_chains = TRUE)
rstan::stan_dens(s_fit_full, pars = "sigma_gamma_k", separate_chains = TRUE)
rstan::stan_dens(s_fit_full, pars = "sigma_gamma_j", separate_chains = TRUE)

# report estimates
report_est(s_samples_full$theta) # "0.5060 [0.5031, 0.5089]"
report_est(s_samples_full$alpha) # "0.5008 [0.4988, 0.5030]"
report_est(inv_logit(s_samples_full$sigma_gamma_k) - 0.50) # "0.0072 [0.0050, 0.0099]"
report_est(inv_logit(s_samples_full$sigma_gamma_j) - 0.50) # "0.0020 [0.0001, 0.0050]"

# tossers estimates
s_toss_est <- inv_logit(matrix(logit(s_samples_full$theta), ncol = ncol(s_samples_full$gamma_k), nrow = nrow(s_samples_full$gamma_k)) + s_samples_full$gamma_k)
colnames(s_toss_est) <- levels(factor(s_df_agg$person))
s_person_flips <- do.call(rbind, lapply(unique(s_df$person), function(person){
  data.frame(
    person = person,
    same   = sum(s_df$toss_start[s_df$person == person] == s_df$toss_end[s_df$person == person]),
    N      = sum(s_df$person == person)
  )
}))
s_person_flips <- s_person_flips[order(s_person_flips$same/s_person_flips$N, decreasing = FALSE),]


pdf("figures/panel1s.pdf", width = 10, height = 8)

layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(1, 0.75))
par(mar = c(0, 4.5, 0, 0.5))
plot(NA, type = "n", main = "", ylab = "", xlab = "", ylim = c(0.40, 0.60), xlim = c(0, nrow(s_person_flips) + 1), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(2, at = seq(0.40, 0.60, 0.05), las = 1, cex.axis = 1.10)
mtext("Pr(same side)", 2, line = 3)
for(i in 1:nrow(s_person_flips)){

  temp_person <- s_person_flips$person[i]

  # descriptive
  temp_des  <- binom.test(s_person_flips$same[i], s_person_flips$N[i])

  lines(rep(i - 1, 2) - 0.20  , temp_des$conf.int, lwd = 1.5, col = "darkblue")
  points(i - 1 - 0.20        , temp_des$estimate, pch = 21, bg  = "white", cex = 1, col = "darkblue")

  # estimates
  temp_den    <- density(s_toss_est[,temp_person])

  temp_den.y <- temp_den$y[temp_den$x<0.60] / 150
  temp_den.x <- temp_den$x[temp_den$x<0.60]

  # polygon(c(0, temp_den.y, 0) + (i - 1)           , c(temp_den.x[1], temp_den.x, temp_den.x[length(temp_den.x)]), col = "grey", border = FALSE)
  points (i - 1                                   , mean(s_toss_est[,temp_person]), pch = 16, cex = 1.25, col = "black")
  lines  (rep(i - 1, 2)                           , quantile(s_toss_est[,temp_person], c(0.025, 0.975)), lwd = 1.5, col = "black")
}

lines(c(0, nrow(s_person_flips) + 1), c(0.50, 0.50), lty = 3)
lines(c(0, nrow(s_person_flips) + 1), c(0.51, 0.51), lty = 3)
text(nrow(s_person_flips)  + 1, 0.50 - 0.002, "chance",   adj = c(1, 1), cex = 1.25)
text(nrow(s_person_flips)  + 1, 0.51 + 0.002, "DHM", adj = c(1, 0), cex = 1.25)

legend(x = 0, y = 0.60, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"), pch = c(21, 16), col = c("darkblue", "black"), bty = "n", lty = 1, bg = c("white", "black"),
       cex = 1.25)


par(mar = c(4, 4.5, 0, 0))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0.46, 0.54), ylim = c(0, 350), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.46, 0.54, 0.02), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 350, 50), las = 1, cex.axis = 1.10)
mtext("Density", 2, line = 3)
mtext("Pr(same side)", 1, line = 2.5)

x_prior <- seq(0.46, 0.54, length.out = 501)
d_prior <- dbeta(x = x_prior, shape1 = 312, shape2 = 312)
lines(x_prior, d_prior, lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(s_samples_full$theta)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

lines(c(0.50, 0.50), c(0, 350), lty = 3)
lines(c(0.51, 0.51), c(0, 350), lty = 3)

text(0.50 - 0.001, 350, "chance",   adj = c(1, 1), cex = 1.25)
text(0.51 + 0.001, 350, "DHM", adj = c(0, 1), cex = 1.25)


par(mar = c(4, 4, 0, 0.5))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0, 0.03), ylim = c(0, 350), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0, 0.03, 0.01), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 350, 50), las = 1, cex.axis = 1.10)
mtext("Between-people heterogeneity in Pr(same side)", 1, line = 2.5)

x_prior <- seq(0, logit(0.53), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.04)*2

lines(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(inv_logit(s_samples_full$sigma_gamma_k) - 0.50, lbound = 0)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

legend(x = 0.02, y = 300, c("Prior", "Posterior"), lwd = 2, lty = c(2, 1), col = c("grey", "black"), bty = "n", cex = 1.25)
dev.off()


# coin estimates
s_toss_est <- inv_logit(matrix(logit(s_samples_full$alpha), ncol = ncol(s_samples_full$gamma_j), nrow = nrow(s_samples_full$gamma_j)) +
                        s_samples_full$gamma_j * matrix(s_samples_full$sigma_gamma_j, ncol = ncol(s_samples_full$gamma_j), nrow = nrow(s_samples_full$gamma_j)))
colnames(s_toss_est) <- levels(factor(s_df_agg$coin))
s_coin_flips <- do.call(rbind, lapply(unique(s_df$coin), function(coin){
  data.frame(
    coin  = coin,
    heads = sum(s_df$toss_end[s_df$coin == coin] == "h"),
    N     = sum(s_df$coin == coin)
  )
}))
s_coin_flips <- s_coin_flips[order(s_coin_flips$heads / s_coin_flips$N, decreasing = FALSE),]

pdf("figures/panel2s.pdf", width = 10, height = 8)

layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(1, 0.75))
par(mar = c(0, 4.5, 0, 0.5))
plot(NA, type = "n", main = "", ylab = "", xlab = "", ylim = c(0.40, 0.60), xlim = c(0, nrow(s_coin_flips) + 1), las = 1, bty = "n", yaxt = "n", xaxt = "n")
mtext("Pr(heads)", 2, line = 3)
axis(2, at = seq(0.40, 0.60, 0.05), las = 1, cex.axis = 1.10)

for(i in 1:nrow(s_coin_flips)){

  temp_coin <- s_coin_flips$coin[i]

  # descriptive
  temp_des  <- binom.test(s_coin_flips$heads[i], s_coin_flips$N[i])

  lines(rep(i - 1, 2) - 0.20  , temp_des$conf.int, lwd = 1.5, col = "darkblue")
  points(i - 1 - 0.20         , temp_des$estimate, pch = 21, bg = "white", cex = 1, col = "darkblue")

  # estimates
  temp_den    <- density(s_toss_est[,temp_coin])

  temp_den.y <- temp_den$y[temp_den$x<0.60] / 250
  temp_den.x <- temp_den$x[temp_den$x<0.60]

  # polygon(c(0, temp_den.y, 0) + (i - 1)           , c(temp_den.x[1], temp_den.x, temp_den.x[length(temp_den.x)]), col = "grey", border = FALSE)
  points (i - 1                                   , mean(s_toss_est[,temp_coin]), pch = 16, cex = 1.25, col = "black")
  lines  (rep(i - 1, 2)                           , quantile(s_toss_est[,temp_coin], c(0.025, 0.975)), lwd = 1.5, col = "black")
}

lines(c(0, nrow(s_coin_flips) + 1), c(0.50, 0.50), lty = 3)
text(nrow(s_coin_flips)  + 1, 0.50 - 0.005, "chance",   adj = c(1, 1), cex = 1.25)

legend(x = 0, y = 0.60, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"), pch = c(21, 16), col = c("darkblue", "black"), bty = "n", lty = 1, bg = c("white", "black"),
       cex = 1.25)


par(mar = c(4, 4.5, 0, 0))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0.46, 0.54), ylim = c(0, 400), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.46, 0.54, 0.02), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 400, 100), las = 1, cex.axis = 1.10)
mtext("Density", 2, line = 3)
mtext("Pr(heads)", 1, line = 2.5)

x_prior <- seq(0.46, 0.54, length.out = 501)
d_prior <- dbeta(x = x_prior, shape1 = 312, shape2 = 312)
lines(x_prior, d_prior, lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(s_samples_full$alpha)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

lines(c(0.50, 0.50), c(0, 400), lty = 3)
text(0.50 - 0.001, 400, "chance",   adj = c(1, 1), cex = 1.25)


par(mar = c(4, 4, 0, 0.5))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0, 0.03), ylim = c(0, 400), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0, 0.03, 0.01), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 400, 100), las = 1, cex.axis = 1.10)
mtext("Between-coin heterogemeity in Pr(heads)", 1, line = 2.5)

x_prior <- seq(0, logit(0.53), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.04)*2

lines(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(inv_logit(s_samples_full$sigma_gamma_j) - 0.50, lbound = 0)
x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")

legend(x = 0.02, y = 400, c("Prior", "Posterior"), lwd = 2, lty = c(2, 1), col = c("grey", "black"), bty = "n", cex = 1.25)
dev.off()


### frequentist re-analysis ----
library(lme4)
library(lmerTest)

df <- read.csv(file = "analyses/data_long.csv")
df$same_side    <- df$toss_start == df$toss_end
df$start_effect <- ifelse(df$toss_start == "h", 1, -1)

fit         <- glmer(same_side ~ 1 + start_effect + (1 | person), family = binomial, data = df)
fit_novar   <- glm(same_side ~ 1 + start_effect, family = binomial, data = df)
fit_coinvar <- glmer(same_side ~ 1 + start_effect + (1 | person) + (1 | coin), family = binomial, data = df)

summary(fit)
summary(fit_novar)
summary(fit_coinvar)

as.data.frame(emmeans::emmeans(
  fit, ~ 1 + start_effect,
  at = list(
    start_effect = 0
  ), type = "response"))

anova(fit, fit_novar)
anova(fit, fit_coinvar)

# sensitivity to outliers
person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
  data.frame(
    person = person,
    same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
    N      = sum(df$person == person)
  )
}))
person_flips <- person_flips[order(person_flips$N, decreasing = FALSE),]
to_remove    <- person_flips$person[person_flips$same / person_flips$N > 0.53]

s_df  <- df[!df$person %in% to_remove,]

s_fit         <- glmer(same_side ~ 1 + start_effect + (1 | person), family = binomial, data = s_df)
s_fit_novar   <- glm(same_side ~ 1 + start_effect, family = binomial, data = s_df)
s_fit_coinvar <- glmer(same_side ~ 1 + start_effect + (1 | person) + (1 | coin), family = binomial, data = s_df)

summary(s_fit)
summary(s_fit_novar)
summary(s_fit_coinvar)

as.data.frame(emmeans::emmeans(
  s_fit, ~ 1 + start_effect,
  at = list(
    start_effect = 0
  ), type = "response"))

anova(s_fit, s_fit_novar)
anova(s_fit, s_fit_coinvar)
