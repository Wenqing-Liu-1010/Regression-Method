### this script performs main analyses ----
source(file = "functions/binomial-test.R")
df <- read.csv(file = "analyses/data_long.csv")

### prepare outliers removed data set
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
s_df      <- df[!df$person %in% to_remove,]

### assess robustness of the same-side and heads-tails bias
mode_range <- seq(0.001, 0.08, 0.001)

model_iid1 <- rstan::stan_model("functions/simple_robustness/model-iid1.stan")
model_iid2 <- rstan::stan_model("functions/simple_robustness/model-iid2.stan")

data_stan_effect <- list(
  x = sum(df$toss_end == df$toss_start),
  n = nrow(df)
)
s_data_stan_effect <- list(
  x = sum(s_df$toss_end == s_df$toss_start),
  n = nrow(s_df)
)
data_stan_heads <- list(
  x = sum(df$toss_end == "h"),
  n = nrow(df)
)
s_data_stan_heads <- list(
  x = sum(s_df$toss_end == "h"),
  n = nrow(s_df)
)


init_samples <- function(theta_mode, cores = 4) {

  inits <- list()

  for(core in 1:cores){
    set.seed(core)
    temp_init <- list()
    temp_init$theta <- runif(1, theta_mode - 0.001, theta_mode + 0.001)
    inits[[core]] <- temp_init
  }

  return(inits)
}
ml_iid       <- function(model, data, theta_mode){

  data$theta_mode <- theta_mode
  temp_fit     <- rstan::sampling(object = model, data = data,
                                  chains = 4, warmup = 15000, iter = 25000, seed = 1, cores = 1,
                                  control = list(adapt_delta = 0.95, max_treedepth = 15),
                                  init = init_samples(theta_mode))
  temp_marglik <- bridgesampling::bridge_sampler(temp_fit)$logml

  return(temp_marglik)
}

cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, c("data_stan_effect", "s_data_stan_effect", "ml_iid", "init_samples", "model_iid1"))
parallel::clusterEvalQ(cl, library(rstan))
parallel::clusterEvalQ(cl, library(bridgesampling))

margliks_theta <- parallel::parSapplyLB(cl, mode_range, function(theta_mode){
  ml_iid(model_iid1, data_stan_effect, theta_mode)
})
s_margliks_theta <- parallel::parSapplyLB(cl, mode_range, function(theta_mode){
  ml_iid(model_iid1, s_data_stan_effect, theta_mode)
})

parallel::clusterExport(cl, c("data_stan_heads", "s_data_stan_heads", "ml_iid", "init_samples", "model_iid2"))
margliks_alpha <- parallel::parSapplyLB(cl, mode_range, function(theta_mode){
  ml_iid(model_iid2, data_stan_heads, theta_mode)
})
s_margliks_alpha <- parallel::parSapplyLB(cl, mode_range, function(theta_mode){
  ml_iid(model_iid2, s_data_stan_heads, theta_mode)
})
parallel::stopCluster(cl)

simple_margliks <- list(
  margliks_theta   = margliks_theta,
  s_margliks_theta = s_margliks_theta,
  margliks_alpha   = margliks_alpha,
  s_margliks_alpha = s_margliks_alpha
)
saveRDS(simple_margliks, file = "analyses/outputs/simple_margliks_rob.RDS")

simple_margliks <- readRDS(file = "analyses/outputs/simple_margliks_rob.RDS")


log_BF_effect   <- simple_margliks$margliks_theta   - dbinom(data_stan_effect$x, data_stan_effect$n, 0.5, log = TRUE)
log_s_BF_effect <- simple_margliks$s_margliks_theta - dbinom(s_data_stan_effect$x, s_data_stan_effect$n, 0.5, log = TRUE)

log_BF_heads   <- simple_margliks$margliks_alpha   - dbinom(data_stan_heads$x, data_stan_heads$n, 0.5, log = TRUE)
log_s_BF_heads <- simple_margliks$s_margliks_alpha - dbinom(s_data_stan_heads$x, s_data_stan_heads$n, 0.5, log = TRUE)


pdf("figures/panel_robustness1.pdf", width = 10, height = 4)
par(mfrow = c(1, 2), mar = c(4, 5, 2, 1))

# same-side bias
plot(
  c(0.5, inv_logit(mode_range)),
  c(0, log_BF_effect),
  pch = 16, type = "l", lwd = 2, xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n",
  xlim = c(0.5, 0.52), ylim = c(0, 40)
)
lines(
  c(0.5, inv_logit(mode_range)),
  c(0, log_s_BF_effect),
  lwd = 2, lty = 2
)
mtext("Prior mode of Pr(same side)", side = 1, line = 2.5)
mtext(expression("BF"["same-side bias"]), side = 2, line = 3.5)
axis(1, at = seq(.50, 0.52, 0.005), cex.axis = 1.10)
axis(2, at = log(10^(c(0:6)*3)), labels = c(1, sapply((c(1:6))*3, function(x) parse(text = paste0("10^",x)))), las = 1, cex.axis = 1.10)

# heads-tails bias
plot(
  c(0.5, inv_logit(mode_range)),
  c(0, log_BF_heads),
  pch = 16, type = "l", lwd = 2, xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n",
  xlim = c(0.5, 0.52), ylim = c(-9, 0)
)
lines(
  c(0.5, inv_logit(mode_range)),
  c(0, log_s_BF_heads),
  lwd = 2, lty = 2
)
mtext("Prior mode of Pr(heads)", side = 1, line = 2.5)
mtext(expression("BF"["heads-tails bias"]), side = 2, line = 3.5)
axis(1, at = seq(.50, 0.52, 0.005), cex.axis = 1.10)
axis(2, at = log(rev(1/c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000))), labels = rev(c("1", paste0("1/",c(3, 10, 30, 100, 300, 1000, 3000, 10000)))), las = 1, cex.axis = 1.10)
legend("topright", legend = c("Complete data set", "Robustness check"), lty = c(1, 2), lwd = 2,  bty = "n", seg.len = 1.75, cex = 1.25)
dev.off()


min(inv_logit(mode_range)[log_BF_effect > log(10)])
min(inv_logit(mode_range)[log_s_BF_effect > log(10)])

inv_logit(mode_range)[which.max(log_BF_effect)]; exp(max(log_BF_effect))
inv_logit(mode_range)[which.max(log_s_BF_effect)]; exp(max(log_s_BF_effect))

min(inv_logit(mode_range)[log_BF_heads < log(1/10)])
min(inv_logit(mode_range)[log_s_BF_heads < log(1/10)])
