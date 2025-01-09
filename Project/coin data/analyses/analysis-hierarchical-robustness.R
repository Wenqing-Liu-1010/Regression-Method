### this script performs hierarchical analyses (and produces corresponding figures) ----
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(rstan) # rstan version 2.26.22 (Stan version 2.26.1)
library(bridgesampling)
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
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

model_iid    <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u1_iid.stan")
model_j      <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u1_j.stan")
model_k      <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u1_k.stan")
model_jk     <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u1_jk.stan")
model0_j     <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u0_j.stan")
model0_k     <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u0_k.stan")
model0_jk    <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-u0_jk.stan")
model_a_iid  <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a1_iid.stan")
model_a_j    <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a1_j.stan")
model_a_k    <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a1_k.stan")
model_a_jk   <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a1_jk.stan")
model0_a_iid <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a0_iid.stan")
model0_a_j   <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a0_j.stan")
model0_a_k   <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a0_k.stan")
model0_a_jk  <- rstan::stan_model("functions/hierarchical_BMA_robustness/model-a0_jk.stan")
# marglik for u0_iid can be easily computed analytically

robustness_theta <- data.frame(expand.grid(
  "theta_mode"          = seq(0.005, 0.08, 0.005),
  "alpha_mode"          = 0.04,
  "sigma_gamma_j_mode"  = 0.02,
  "sigma_gamma_k_mode"  = 0.02
))
robustness_alpha <- data.frame(expand.grid(
  "theta_mode"          = 0.04,
  "alpha_mode"          = seq(0.005, 0.08, 0.005),
  "sigma_gamma_j_mode"  = 0.02,
  "sigma_gamma_k_mode"  = 0.02
))
robustness_gamma_j <- data.frame(expand.grid(
  "theta_mode"          = 0.04,
  "alpha_mode"          = 0.04,
  "sigma_gamma_j_mode"  = seq(0.005, 0.08, 0.005),
  "sigma_gamma_k_mode"  = 0.02
))
robustness_gamma_k <- data.frame(expand.grid(
  "theta_mode"          = 0.04,
  "alpha_mode"          = 0.04,
  "sigma_gamma_j_mode"  = 0.02,
  "sigma_gamma_k_mode"  = seq(0.005, 0.08, 0.005)
))

models <- list(
  "0_iid"    = NULL,
  "0_j"      = model0_j,
  "0_k"      = model0_k,
  "0_jk"     = model0_jk,
  "1_iid"    = model_iid,
  "1_j"      = model_j,
  "1_k"      = model_k,
  "1_jk"     = model_jk,
  "a_0_iid"  = model0_a_iid,
  "a_0_j"    = model0_a_j,
  "a_0_k"    = model0_a_k,
  "a_0_jk"   = model0_a_jk,
  "a_1_iid"  = model_a_iid,
  "a_1_j"    = model_a_j,
  "a_1_k"    = model_a_k,
  "a_1_jk"   = model_a_jk
)
models_guide <- data.frame(
  model_names = names(models),
  theta       = !rep(c(rep(T, 4), rep(F, 4)), 2),
  alpha       = !c(rep(T, 8), rep(F, 8)),
  gamma_k     = !rep(c(T, T, F, F, T, T, F, F), 2),
  gamma_j     = !rep(c(T, F, T, F, T, F, T, F), 2)
)

init_samples <- function(model, models_guide, priors, cores) {

  inits <- list()

  for(core in 1:cores){
    set.seed(core)

    temp_init <- list()
    if(models_guide[models_guide$model_names == model, "theta"]){
      temp_init$theta <- runif(1, priors$theta_mode - 0.005, priors$theta_mode + 0.005)
    }
    if(models_guide[models_guide$model_names == model, "alpha"]){
      temp_init$alpha <- runif(1, priors$alpha_mode - 0.005, priors$alpha_mode + 0.005)
    }
    if(models_guide[models_guide$model_names == model, "gamma_j"]){
      temp_init$sigma_gamma_j <- runif(1, priors$sigma_gamma_j_mode - 0.0025, priors$sigma_gamma_j_mode + 0.0025)
    }
    if(models_guide[models_guide$model_names == model, "gamma_k"]){
      temp_init$sigma_gamma_k <- runif(1, priors$sigma_gamma_k_mode - 0.0025, priors$sigma_gamma_k_mode + 0.0025)
    }

    inits[[core]] <- temp_init
  }


  return(inits)
}

fit_models <- function(models, data_stan, models_guide, robustness_type, priors, null = FALSE, parallel = FALSE){

  temp_data <- data_stan
  temp_data$theta_mode         <- priors$theta_mode
  temp_data$alpha_mode         <- priors$alpha_mode
  temp_data$sigma_gamma_j_mode <- priors$sigma_gamma_j_mode
  temp_data$sigma_gamma_k_mode <- priors$sigma_gamma_k_mode

  margliks <- list()
  for(model in models_guide$model_names[if(null) !models_guide[,robustness_type] else models_guide[,robustness_type]]){
    if(model == "0_iid"){
      temp_h <- sum(data_stan$tails_heads) + sum(data_stan$heads_heads)
      temp_n <- sum(data_stan$N_start_tails_up) + sum(data_stan$N_start_heads_up)
      temp_x <- c(rep(1, temp_h), rep(0, temp_n - temp_h))
      temp_marglik <- sum(dbinom(x = temp_x, size = 1, prob = 0.50, log = TRUE))
    }else{
      temp_fit     <- rstan::sampling(object = models[[model]], data = temp_data,
                                      chains = 4, warmup = 15000, iter = 25000, seed = 1, cores = if(parallel) 4 else 1,
                                      control = list(adapt_delta = 0.95, max_treedepth = 15),
                                      init = init_samples(model, models_guide, priors, 4))
      temp_marglik <- bridgesampling::bridge_sampler(temp_fit)$logml
    }


    margliks[[model]] <- temp_marglik
  }

  out <- cbind.data.frame(
    do.call(cbind, priors),
    do.call(cbind, margliks)
  )

  return(out)
}

cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, c("models", "data_stan", "models_guide", "robustness_theta", "robustness_alpha", "robustness_gamma_j", "robustness_gamma_k", "init_samples", "fit_models"))
parallel::clusterEvalQ(cl, library(rstan))
parallel::clusterEvalQ(cl, library(bridgesampling))

margliks_theta <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_theta), function(i){
  fit_models(models, data_stan, models_guide, "theta", robustness_theta[i,])
}))
margliks_theta.null <- fit_models(models, data_stan, models_guide, "theta", robustness_theta[1,], null = TRUE, parallel = TRUE)

margliks_alpha <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_alpha), function(i){
  fit_models(models, data_stan, models_guide, "alpha", robustness_alpha[i,])
}))
margliks_alpha.null <- fit_models(models, data_stan, models_guide, "alpha", robustness_alpha[1,], null = TRUE, parallel = TRUE)

margliks_gamma_j <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_gamma_j), function(i){
  fit_models(models, data_stan, models_guide, "gamma_j", robustness_gamma_j[i,])
}))
margliks_gamma_j.null <- fit_models(models, data_stan, models_guide, "gamma_j", robustness_gamma_j[1,], null = TRUE, parallel = TRUE)

margliks_gamma_k <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_gamma_k), function(i){
  fit_models(models, data_stan, models_guide, "gamma_k", robustness_gamma_k[i,])
}))
margliks_gamma_k.null <- fit_models(models, data_stan, models_guide, "gamma_k", robustness_gamma_k[1,], null = TRUE, parallel = TRUE)

parallel::stopCluster(cl)


margliks <- list(
  theta = list(
    robustness = margliks_theta,
    null       = margliks_theta.null
  ),
  alpha = list(
    robustness = margliks_alpha,
    null       = margliks_alpha.null
  ),
  gamma_j = list(
    robustness = margliks_gamma_j,
    null       = margliks_gamma_j.null
  ),
  gamma_k = list(
    robustness = margliks_gamma_k,
    null       = margliks_gamma_k.null
  )
)

saveRDS(margliks, file = "analyses/outputs/margliks_rob.RDS")


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


cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, c("models", "s_data_stan", "models_guide", "robustness_theta", "robustness_alpha", "robustness_gamma_j", "robustness_gamma_k", "init_samples", "fit_models"))
parallel::clusterEvalQ(cl, library(rstan))
parallel::clusterEvalQ(cl, library(bridgesampling))

s_margliks_theta <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_theta), function(i){
  fit_models(models, s_data_stan, models_guide, "theta", robustness_theta[i,])
}))
s_margliks_theta.null <- fit_models(models, s_data_stan, models_guide, "theta", robustness_theta[1,], null = TRUE, parallel = TRUE)

s_margliks_alpha <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_alpha), function(i){
  fit_models(models, s_data_stan, models_guide, "alpha", robustness_alpha[i,])
}))
s_margliks_alpha.null <- fit_models(models, s_data_stan, models_guide, "alpha", robustness_alpha[1,], null = TRUE, parallel = TRUE)

s_margliks_gamma_j <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_gamma_j), function(i){
  fit_models(models, s_data_stan, models_guide, "gamma_j", robustness_gamma_j[i,])
}))
s_margliks_gamma_j.null <- fit_models(models, s_data_stan, models_guide, "gamma_j", robustness_gamma_j[1,], null = TRUE, parallel = TRUE)

s_margliks_gamma_k <- do.call(rbind, parallel::parLapply(cl, 1:nrow(robustness_gamma_k), function(i){
  fit_models(models, s_data_stan, models_guide, "gamma_k", robustness_gamma_k[i,])
}))
s_margliks_gamma_k.null <- fit_models(models, s_data_stan, models_guide, "gamma_k", robustness_gamma_k[1,], null = TRUE, parallel = TRUE)

parallel::stopCluster(cl)


s_margliks <- list(
  theta = list(
    robustness = s_margliks_theta,
    null       = s_margliks_theta.null
  ),
  alpha = list(
    robustness = s_margliks_alpha,
    null       = s_margliks_alpha.null
  ),
  gamma_j = list(
    robustness = s_margliks_gamma_j,
    null       = s_margliks_gamma_j.null
  ),
  gamma_k = list(
    robustness = s_margliks_gamma_k,
    null       = s_margliks_gamma_k.null
  )
)

saveRDS(s_margliks, file = "analyses/outputs/s_margliks_rob.RDS")


### visualize the results ----
source(file = "functions/binomial-test.R")
margliks   <- readRDS(file = "analyses/outputs/margliks_rob.RDS")
s_margliks <- readRDS(file = "analyses/outputs/s_margliks_rob.RDS")

pdf("figures/panel_robustness2.pdf", width = 10, height = 8)
par(mfrow = c(2, 2), mar = c(4, 5, 2, 1))

# same-side bias
BF_effect    <- sapply(1:nrow(margliks$theta$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(margliks$theta$robustness[i,-c(1:4)], margliks$theta$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
s_BF_effect  <- sapply(1:nrow(s_margliks$theta$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(s_margliks$theta$robustness[i,-c(1:4)], s_margliks$theta$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
plot(
  c(0.5, inv_logit(margliks$theta$robustness$theta_mode)),
  c(0, log(BF_effect)),
  pch = 16, lwd = 2, xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", type = "l",
  xlim = c(0.5, 0.52), ylim = log(c(1, 10000))
)
lines(
  c(0.5, inv_logit(s_margliks$theta$robustness$theta_mode)),
  c(0, log(s_BF_effect)),
  lwd = 2, lty = 2
)
mtext("Prior mode of Pr(same side)", side = 1, line = 2.5)
mtext(expression("BF"["same-side bias"]), side = 2, line = 3.5)
axis(1, at = seq(.50, 0.52, 0.005), cex.axis = 1.10)
axis(2, at = log(c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000)), labels = c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000), las = 1, cex.axis = 1.10)

# heads-tails bias
BF_heads  <- sapply(1:nrow(margliks$alpha$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(margliks$alpha$robustness[i,-c(1:4)], margliks$alpha$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
s_BF_heads  <- sapply(1:nrow(s_margliks$alpha$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(s_margliks$alpha$robustness[i,-c(1:4)], s_margliks$alpha$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
plot(
  c(0.5, inv_logit(margliks$alpha$robustness$alpha_mode)),
  c(0, log(BF_heads)),
  pch = 16, lwd = 2, xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", type = "l",
  xlim = c(0.5, 0.52), ylim = log(c(1/3000, 1))
)
lines(
  c(0.5, inv_logit(s_margliks$alpha$robustness$alpha_mode)),
  c(0, log(s_BF_heads)),
  lwd = 2, lty = 2
)
mtext("Prior mode of Pr(heads)", side = 1, line = 2.5)
mtext(expression("BF"["heads-tails bias"]), side = 2, line = 3.5)
axis(1, at = seq(.50, 0.52, 0.005), cex.axis = 1.10)
axis(2, at = log(rev(1/c(1, 3, 10, 30, 100, 300, 1000, 3000))), labels = rev(c("1", paste0("1/",c(3, 10, 30, 100, 300, 1000, 3000)))), las = 1, cex.axis = 1.10)

# between tosser variation
BF_tossers <- sapply(1:nrow(margliks$gamma_k$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(margliks$gamma_k$robustness[i,-c(1:4)], margliks$gamma_k$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
s_BF_tossers <- sapply(1:nrow(s_margliks$gamma_k$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(s_margliks$gamma_k$robustness[i,-c(1:4)], s_margliks$gamma_k$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
plot(
  c(0.5, inv_logit(margliks$gamma_k$robustness$sigma_gamma_k_mode)),
  c(0, log(BF_tossers)),
  pch = 16, lwd = 2, xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", type = "l",
  xlim = c(0.5, 0.52), ylim = log(c(1, 10^27))
)
lines(
  c(0.5, inv_logit(s_margliks$gamma_k$robustness$sigma_gamma_k_mode)),
  c(0, log(s_BF_tossers)),
  lwd = 2, lty = 2
)
mtext("Prior mode of between-people heterogeneity in Pr(same side)", side = 1, line = 2.5)
mtext(expression("BF"["between-people heterogeneity"]), side = 2, line = 3.5)
axis(1, at = seq(.50, 0.52, 0.005), labels = seq(.50, 0.52, 0.005)-0.5, cex.axis = 1.10)
axis(2, at = log(10^(c(0:9)*3)), labels = c(1, sapply((c(1:9))*3, function(x) parse(text = paste0("10^",x)))), las = 1, cex.axis = 1.10)

# between coin variation
BF_coin <- sapply(1:nrow(margliks$gamma_j$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(margliks$gamma_j$robustness[i,-c(1:4)], margliks$gamma_j$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
s_BF_coin <- sapply(1:nrow(s_margliks$gamma_j$robustness), function(i) {
  BayesTools::inclusion_BF(
    prior_probs = rep(1/16, 16),
    margliks    = unlist(c(s_margliks$gamma_j$robustness[i,-c(1:4)], s_margliks$gamma_j$null[1,-c(1:4)])),
    is_null     = c(rep(F, 8), rep(T, 8))
  )
})
plot(
  c(0.5, inv_logit(margliks$gamma_j$robustness$sigma_gamma_j_mode)),
  c(0, log(BF_coin)),
  pch = 16, lwd = 2, xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", type = "l",
  xlim = c(0.5, 0.52), ylim = log(c(1e-5, 1))
)
lines(
  c(0.5, inv_logit(s_margliks$gamma_j$robustness$sigma_gamma_j_mode)),
  c(0, log(s_BF_coin)),
  lwd = 2, lty = 2
)
mtext("Prior mode of between-coin heterogeneity in Pr(heads)", side = 1, line = 2.5)
mtext(expression("BF"["between-coin heterogeneity"]), side = 2, line = 3.5)
axis(1, at = seq(.50, 0.52, 0.005), labels = seq(.50, 0.52, 0.005)-0.5, cex.axis = 1.10)
axis(2, at = log(rev(1/c(10^(0:5)))), labels = rev(c(1, sapply((1:5)*-1, function(x) parse(text = paste0("10^",x))))), las = 1, cex.axis = 1.10)

legend("topright", legend = c("Complete data set", "Robustness check"), lty = c(1, 2), lwd = 2,  bty = "n", seg.len = 1.75, cex = 1.25)
dev.off()

# summaries
min(inv_logit(margliks$theta$robustness$theta_mode)[BF_effect > 10])
min(inv_logit(s_margliks$theta$robustness$theta_mode)[s_BF_effect > 10])

inv_logit(margliks$theta$robustness$theta_mode)[which.max(BF_effect)]; max(BF_effect)
inv_logit(s_margliks$theta$robustness$theta_mode)[which.max(s_BF_effect)]; max(s_BF_effect)


min(inv_logit(margliks$alpha$robustness$alpha_mode)[s_BF_heads < 1/10])
min(inv_logit(s_margliks$alpha$robustness$alpha_mode)[s_BF_heads < 1/10])


min((inv_logit(margliks$gamma_k$robustness$sigma_gamma_k_mode)-0.50)[BF_tossers > 10])
min((inv_logit(s_margliks$gamma_k$robustness$sigma_gamma_k_mode)-0.50)[s_BF_tossers > 10])

(inv_logit(margliks$gamma_k$robustness$sigma_gamma_k_mode)[which.max(BF_tossers)]-0.50); max(BF_tossers)
(inv_logit(s_margliks$gamma_k$robustness$sigma_gamma_k_mode)[which.max(s_BF_tossers)]-0.50); max(s_BF_tossers)


min((inv_logit(margliks$gamma_j$robustness$sigma_gamma_j_mode)-0.50)[BF_coin < 1/10])
min((inv_logit(s_margliks$gamma_j$robustness$sigma_gamma_j_mode)-0.50)[s_BF_coin < 1/10])
