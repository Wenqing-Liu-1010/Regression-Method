### this script performs trimmed hierarchical analyses ----
# these analyses are computationally heavy --- many sub-results are saved in the analyses/outputs folder
# and can be loaded to skip the sampling steps

# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(rstan) # rstan version 2.26.22 (Stan version 2.26.1)
library(bridgesampling)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)
source(file = "functions/binomial-test.R")

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

# hypothesis testing
for(i in 1:5){

  df     <- read.csv(file = "analyses/data_long.csv")
  df_agg <- read.csv(file = "analyses/data_agg.csv")

  person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
    data.frame(
      person = person,
      same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
      N      = sum(df$person == person)
    )
  }))
  person_flips <- person_flips[order(person_flips$same/person_flips$N, decreasing = FALSE),]

  # remove i- least and most biased people
  to_remove <- c(
    person_flips$person[1:i],
    rev(person_flips$person)[1:i]
  )
  s_df_agg  <- df_agg[!df_agg$person %in% to_remove,]

  # create a stan data object
  data_stan <- with(s_df_agg, list(
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


  # marglik for u0_iid can be easily computed analytically

  fit_iid      <- rstan::sampling(object = model_iid   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_iid  <- bridgesampling::bridge_sampler(fit_iid)$logml
  rm(fit_iid)

  fit_j        <- rstan::sampling(object = model_j     ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_j    <- bridgesampling::bridge_sampler(fit_j)$logml
  rm(fit_j)

  fit_k        <- rstan::sampling(object = model_k     ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_k    <- bridgesampling::bridge_sampler(fit_k)$logml
  rm(fit_k)

  fit_jk       <- rstan::sampling(object = model_jk    ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_jk   <- bridgesampling::bridge_sampler(fit_jk)$logml
  rm(fit_jk)

  fit0_j       <- rstan::sampling(object = model0_j   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_j   <- bridgesampling::bridge_sampler(fit0_j)$logml
  rm(fit0_j)

  fit0_k       <- rstan::sampling(object = model0_k   ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_k   <- bridgesampling::bridge_sampler(fit0_k)$logml
  rm(fit0_k)

  fit0_jk      <- rstan::sampling(object = model0_jk  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_jk  <- bridgesampling::bridge_sampler(fit0_jk)$logml
  rm(fit0_jk)

  fit_a_iid    <- rstan::sampling(object = model_a_iid,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_a_iid<- bridgesampling::bridge_sampler(fit_a_iid)$logml
  rm(fit_a_iid)

  fit_a_j      <- rstan::sampling(object = model_a_j  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_a_j  <- bridgesampling::bridge_sampler(fit_a_j)$logml
  rm(fit_a_j)

  fit_a_k      <- rstan::sampling(object = model_a_k  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_a_k  <- bridgesampling::bridge_sampler(fit_a_k)$logml
  rm(fit_a_k)

  fit_a_jk     <- rstan::sampling(object = model_a_jk ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik_a_jk <- bridgesampling::bridge_sampler(fit_a_jk)$logml
  rm(fit_a_jk)

  fit0_a_iid   <- rstan::sampling(object = model0_a_iid,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_a_iid<- bridgesampling::bridge_sampler(fit0_a_iid)$logml
  rm(fit0_a_iid)

  fit0_a_j     <- rstan::sampling(object = model0_a_j  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_a_j <- bridgesampling::bridge_sampler(fit0_a_j)$logml
  rm(fit0_a_j)

  fit0_a_k     <- rstan::sampling(object = model0_a_k  ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_a_k <- bridgesampling::bridge_sampler(fit0_a_k)$logml
  rm(fit0_a_k)

  fit0_a_jk    <- rstan::sampling(object = model0_a_jk ,data = data_stan, chains = 10, warmup = 15000, iter = 25000, seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  marglik0_a_jk<- bridgesampling::bridge_sampler(fit0_a_jk)$logml
  rm(fit0_a_jk)

  marglik0_iid <- sum(dbinom(x = df$toss_end == "h", size = 1, prob = 0.50, log = TRUE))


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

  saveRDS(margliks, file = paste0("analyses/outputs/margliks-t", i,".RDS"))
}

margliks <- do.call(rbind, c(
  list(readRDS(file = "analyses/outputs/margliks.RDS")),
  lapply(1:5, function(i) readRDS(file = paste0("analyses/outputs/margliks-t", i,".RDS")))
))

BFs <- do.call(rbind, lapply(1:nrow(margliks), function(i){
  return(list(
    BF_effect  = BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks[i,], is_null = rep(c(rep(T, 4), rep(F, 4)), 2)),
    BF_tossers = BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks[i,], is_null = rep(c(T, T, F, F, T, T, F, F), 2)),
    BF_heads   = BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks[i,], is_null = c(rep(T, 8), rep(F, 8))),
    BF_coins   = BayesTools::inclusion_BF(prior_probs = rep(1/16, 16), margliks = margliks[i,], is_null = rep(c(T, F, T, F, T, F, T, F), 2))
  ))
}))
xtable::xtable(BFs)


model_full <- rstan::stan_model("functions/hierarchical_estimation/model-a1_jk-est.stan")
# parameter estimation
for(i in 1:5){

  df     <- read.csv(file = "analyses/data_long.csv")
  df_agg <- read.csv(file = "analyses/data_agg.csv")

  person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
    data.frame(
      person = person,
      same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
      N      = sum(df$person == person)
    )
  }))
  person_flips <- person_flips[order(person_flips$same/person_flips$N, decreasing = FALSE),]

  # remove i- least and most biased people
  to_remove <- c(
    person_flips$person[1:i],
    rev(person_flips$person)[1:i]
  )
  s_df_agg  <- df_agg[!df_agg$person %in% to_remove,]

  # create a stan data object
  data_stan_est <- with(s_df_agg, list(
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

  data_stan_est$theta_alpha          <- 312
  data_stan_est$theta_beta           <- 312
  data_stan_est$alpha_alpha          <- 312
  data_stan_est$alpha_beta           <- 312
  data_stan_est$sigma_gamma_j_sigma  <- 0.04
  data_stan_est$sigma_gamma_k_sigma  <- 0.04
  # sd of main effects 0.02
  # sd of random effects on logit scale:  0.04 -> 0.01

  set.seed(1)
  fit_full     <- rstan::sampling(object = model_full, data = data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                                  seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
  samples_full <- rstan::extract(fit_full)

  ests <- list()
  ests$theta <- report_est(samples_full$theta) # "0.5098 [0.5050, 0.5147]"
  ests$alpha <- report_est(samples_full$alpha) # "0.5005 [0.4986, 0.5026]"
  ests$sigma_gamma_k <- report_est(inv_logit(samples_full$sigma_gamma_k) - 0.50) # "0.0156 [0.0119, 0.0200]"
  ests$sigma_gamma_j <- report_est(inv_logit(samples_full$sigma_gamma_j) - 0.50) # "0.0018 [0.0001, 0.0047]"

  saveRDS(ests, file = paste0("analyses/outputs/ests-t", i,".RDS"))
}

ests <- do.call(rbind, lapply(1:5, function(i) readRDS(file = paste0("analyses/outputs/ests-t", i,".RDS"))))
xtable::xtable(ests)
