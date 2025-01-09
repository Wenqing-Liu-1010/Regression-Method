### this script visualizes time trends of the same-side bias (and produces corresponding figures) ----
library(rstan) # rstan version 2.26.22 (Stan version 2.26.1)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)
source(file = "functions/binomial-test.R")


df          <- read.csv(file = "analyses/data_long.csv")
df_time     <- read.csv(file = "analyses/df_time.csv")
df_time_agg <- read.csv(file = "analyses/df_time_agg.csv")

data_stan_est <- with(df_time_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(df_time_agg),
  t                = 1 + mean_toss / 1000
))

data_stan_est$theta_alpha           <- 312
data_stan_est$theta_beta            <- 312
data_stan_est$alpha_alpha           <- 312
data_stan_est$alpha_beta            <- 312
data_stan_est$sigma_gamma_j_sigma   <- 0.04
data_stan_est$sigma_gamma_k_sigma   <- 0.04

data_stan_est$bpar_mu             <- 0
data_stan_est$bpar_sigma          <- 10
data_stan_est$sigma_bpar_k_sigma  <- 1

data_stan_est$cpar_mu             <- 0
data_stan_est$cpar_sigma          <- 10
data_stan_est$sigma_cpar_k_sigma  <- 2

data_stan_est$gpar_mu             <- 0
data_stan_est$gpar_sigma          <- 5
data_stan_est$sigma_gpar_k_sigma  <- 5

init_samples <- function(cores) {

  inits <- list()

  for(core in 1:cores){

    set.seed(core)
    temp_init <- list()
    temp_init$theta  <- runif(1, 0.45, 0.55)
    temp_init$theta2 <- runif(1, 0.45, 0.55)
    temp_init$alpha  <- runif(1, 0.45, 0.55)
    temp_init$bpar   <- rnorm(1, 0, 0.5)

    temp_init$sigma_gamma_k  <- runif(1, 0, 0.05)
    temp_init$sigma_gamma2_k <- runif(1, 0, 0.05)
    temp_init$sigma_gamma_j  <- runif(1, 0, 0.05)
    temp_init$sigma_bpar_k   <- runif(1, 0, 1)

    inits[[core]] <- temp_init
  }

  return(inits)
}
model_time <- rstan::stan_model("functions/hierarchical_estimation/model-a1_jk_time-est.stan")
set.seed(1)
fit_time <- rstan::sampling(object = model_time, data = data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                            init = init_samples(cores = 10),
                             seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
saveRDS(fit_time , file = "analyses/outputs/fit_time.RDS", compress = "xz")

fit_time <- readRDS(file = "analyses/outputs/fit_time.RDS")
samples_time <- extract(fit_time)

# check convergence
fit_time_summary <- summary(fit_time, pars = c("theta", "theta2", "alpha", "bpar", "sigma_gamma_k", "sigma_gamma2_k", "sigma_gamma_j", "sigma_bpar_k"))$summary
fit_time_summary

# check chains
rstan::traceplot(fit_time, pars = "theta")
rstan::traceplot(fit_time, pars = "theta2")
rstan::traceplot(fit_time, pars = "alpha")
rstan::traceplot(fit_time, pars = "bpar")
rstan::traceplot(fit_time, pars = "sigma_gamma_k")
rstan::traceplot(fit_time, pars = "sigma_gamma2_k")
rstan::traceplot(fit_time, pars = "sigma_gamma_j")
rstan::traceplot(fit_time, pars = "sigma_bpar_k")

rstan::stan_dens(fit_time, pars = "theta",  separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "theta2", separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "alpha",  separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "bpar",   separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "sigma_gamma_k",  separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "sigma_gamma2_k", separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "sigma_gamma_j",  separate_chains = TRUE)
rstan::stan_dens(fit_time, pars = "sigma_bpar_k",   separate_chains = TRUE)

# report estimates
report_est(samples_time$theta)   # "0.5014 [0.4980, 0.5046]
report_est(samples_time$theta2)  # "0.5250 [0.5129, 0.5374]"
report_est(inv_logit(logit(samples_time$theta) + logit(samples_time$theta2))) # "0.5264 [0.5151, 0.5385]"

report_est(inv_logit(samples_time$sigma_gamma_k) - 0.50)  # "0.0028 [0.0001, 0.0067]"
report_est(inv_logit(samples_time$sigma_gamma2_k) - 0.50) # "0.0296 [0.0215, 0.0386]"
report_est(inv_logit(sqrt(samples_time$sigma_gamma_k^2 + samples_time$sigma_gamma2_k^2)) - 0.50) # "0.0298 [0.0218, 0.0388]"

report_est(samples_time$alpha) # "0.5005 [0.4986, 0.5026]"
report_est(inv_logit(samples_time$sigma_gamma_j) - 0.50) # "0.0018 [0.0001, 0.0047]"

report_est(samples_time$bpar) # "-1.6173 [-2.8269, -0.8881]"
report_est(samples_time$sigma_bpar_k) # "0.8406 [0.3660, 1.6521]"


pdf("figures/panel3.pdf", width = 10, height = 8)

layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(1, 0.75))
par(mar = c(4, 4.5, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", ylim = c(0.45, 0.55), xlim = c(0, 20), las = 1, bty = "n", yaxt = "n", xaxt = "n")

axis(2, at = seq(0.45, 0.55, 0.05), las = 1, cex.axis = 1.10)
axis(1, 0:10 * 2, (0:10)*2000)
mtext("Coin flips", 1, line = 3)
mtext("Pr(same side)", 2, line = 3)


f5PL       <- function(x, a, b, c, d, g){
  d + (a - d) / (1 + (x/c)^b)^(g)
}
f4PL       <- function(x, a, b, c, d){
  a + (d - a) * inv_logit(b*(x-c))
}
fpow       <- function(x, a, b, c){
  a + b * x^c
}

get_effect <- function(t, samples){

  alpha   <- logit(samples$alpha)
  theta   <- logit(samples$theta)
  theta2  <- logit(samples$theta2)
  bpar    <- samples$bpar

  return(inv_logit(fpow(t, theta, theta2, bpar)))
}

t_seq <- seq(0, 20, 0.1)
mu <- do.call(cbind, lapply(t_seq+1, get_effect, samples = samples_time))
polygon(c(t_seq, rev(t_seq)), c(apply(mu, 2, quantile, probs = 0.025), rev(apply(mu, 2, quantile, probs = 0.975))), col = scales::alpha("black", 0.5))
lines(t_seq, apply(mu, 2, mean), lwd = 2, col = "black")


for(i in 1:20){
  temp_ids  <- df_time$toss_number > (i-1) * 1000 & df_time$toss_number <= i * 1000
  temp_same <- df_time$toss_start[temp_ids] == df_time$toss_end[temp_ids]
  temp_test <- binom.test(sum(temp_same), length(temp_same))

  points(i - 0.5, mean(temp_same), pch = 21, col = "darkblue")
  lines(rep(i - 0.5, 2), c(temp_test$conf.int[1], temp_test$conf.int[2]), col = "darkblue")
}

lines(c(0, 20), c(0.50, 0.50), lty = 3)
lines(c(0, 20), c(0.51, 0.51), lty = 3)
text(20, 0.50 - 0.002, "chance", adj = c(1, 1), cex = 1.25)
text(20, 0.51 + 0.002, "DHM",  adj = c(1, 0), cex = 1.25)

legend(x = 0, y = 0.55, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"),
       pch = c(21, NA), col = c("darkblue", "black"), bty = "n", lty = 1, cex = 1.25)



par(mar = c(4, 4.5, 0, 0))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0.45, 0.55), ylim = c(0, 250), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.45, 0.55, 0.025), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.10)
mtext("Density", 2, line = 3)
mtext("Pr(same side)", 1, line = 2.5)

x_prior <- seq(0.45, 0.55, length.out = 501)
d_prior <- dbeta(x = x_prior, shape1 = 312, shape2 = 312)
lines(x_prior, d_prior, lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(samples_time$theta)
d_post2 <- polspline::logspline(samples_time$theta2)

x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
x_post2 <- seq(polspline::qlogspline(0.0005, d_post2), polspline::qlogspline(0.9995, d_post2), length.out = 101)

lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")
lines(x_post2, polspline::dlogspline(x_post2, d_post2), lwd = 2, lty = 4, col = "black")

lines(c(0.50, 0.50), c(0, 250), lty = 3)
lines(c(0.51, 0.51), c(0, 250), lty = 3)

text(0.50 - 0.001, 250, "chance",   adj = c(1, 1), cex = 1.25)
text(0.51 + 0.001, 250, "DHM", adj = c(0, 1), cex = 1.25)


par(mar = c(4, 4, 0, 0.5))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0, 0.05), ylim = c(0, 250), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0, 0.05, 0.01), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.10)
mtext("Between-people heterogeneity in Pr(same side)", 1, line = 2.5)

x_prior <- seq(0, logit(0.55), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.04)*2

lines(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(inv_logit(samples_time$sigma_gamma_k) - 0.50, lbound = 0)
d_post2 <- polspline::logspline(inv_logit(samples_time$sigma_gamma2_k) - 0.50, lbound = 0)

x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post),  length.out = 101)
x_post2 <- seq(polspline::qlogspline(0.0005, d_post2), polspline::qlogspline(0.9995, d_post2), length.out = 101)

lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")
lines(x_post2, polspline::dlogspline(x_post2, d_post2), lwd = 2, lty = 4, col = "black")

legend(x = 0.02, y = 250, c("Prior", "Posterior Baseline", "Posterior Toss-Dep."), lwd = 2, lty = c(2, 1, 4), col = c("grey", "black", "black"), bty = "n", cex = 1.25)
dev.off()


get_starting_i <- function(i, samples){

  theta  <- logit(samples$theta) + samples$gamma_k[,i]

  return(inv_logit(theta))
}

xxx <- lapply(1:47, function(i) get_starting_i(i, samples_time))
hist(sapply(xxx, mean), breaks = 20, xlim = c(0.45, 0.65))
rug(sapply(xxx, mean))

### additional figures
get_effect <- function(t, samples){

  alpha   <- logit(samples$alpha)
  theta   <- logit(samples$theta)
  theta2  <- logit(samples$theta2)
  delta2  <- samples$delta2

  return(inv_logit((theta-theta2) * t^-exp(delta2) + theta2))
}
get_effect_i <- function(t, i, samples){

  alpha  <- logit(samples$alpha)
  theta  <- logit(samples$theta) + samples$gamma_k[,i]
  theta2 <- logit(samples$theta2) + samples$gamma2_k[,i]
  delta2 <- samples$delta2 + samples$delta2_k[,i]

  return(inv_logit((theta-theta2) * t^-exp(delta2) + theta2))
}

t_seq <- seq(0, 20, 0.1)

par(mfrow = c(1,2))
plot(NA, type = "n", las = 1, xlab = "Coin flips", xaxt = "n", ylab = "Probability (same side)", ylim = c(0.45, 0.55), xlim = c(0, 20))
axis(1, 0:10 * 2, (0:10)*2000)
mu <- do.call(cbind, lapply(t_seq, get_effect, samples = samples_time))
polygon(c(t_seq, rev(t_seq)), c(apply(mu, 2, quantile, probs = 0.025), rev(apply(mu, 2, quantile, probs = 0.975))), col = scales::alpha("blue", 0.5))
lines(t_seq, apply(mu, 2, median), lwd = 2, col = "blue")
abline(h = 0.50, lty = 2)
abline(h = 0.51, lty = 2)

for(i in 1:20){
  temp_ids  <- df_time$toss_number > (i-1) * 1000 & df_time$toss_number <= i * 1000
  temp_same <- df_time$toss_start[temp_ids] == df_time$toss_end[temp_ids]
  temp_test <- binom.test(sum(temp_same), length(temp_same))

  points(i - 0.5, mean(temp_same), pch = 16)
  arrows(x0 = i - 0.5, y0 = temp_test$conf.int[1], x1 = i - 0.5, y1 = temp_test$conf.int[2], code = 3, angle = 90, length = 0.05)
}

plot(NA, type = "n", las = 1, xlab = "Coin flips", xaxt = "n", ylab = "Probability (same side)", ylim = c(0.45, 0.55), xlim = c(0, 20))
axis(1, 0:10 * 2, (0:10)*2000)
for(i in 1:47){

  temp_df   <- df_time_agg[df_time_agg$person == levels(as.factor(df_time_agg$person))[i],]
  temp_N    <- sum(temp_df$N_start_heads_up) + sum(temp_df$N_start_tails_up)

  t_seq <- seq(0, temp_N/1000, 0.1)

  mu_i <- do.call(cbind, lapply(t_seq, get_effect_i, samples = samples_time, i = i))
  lines(t_seq, apply(mu_i, 2, median), lwd = 1)

}
abline(h = 0.50, lty = 2)
abline(h = 0.51, lty = 2)

dev.off()


### individual subfigures
for(i in 1:47){
  pdf(file = file.path("temp-output", paste0("i=",i,"-", levels(as.factor(df_time_agg$person))[i], ".pdf")), width = 7, height = 5)

  temp_df   <- df_time_agg[df_time_agg$person == levels(as.factor(df_time_agg$person))[i],]
  temp_N    <- sum(temp_df$N)
  temp_same <- sum(temp_df$same_side)

  plot(NA, type = "n", las = 1, xlab = "Coin flips", xaxt = "n", ylab = "Probability (same side)", main = paste0(
    levels(as.factor(df_time_agg$person))[i], " (", round(temp_same/temp_N, 3), ", N = ", temp_N, ")"
  ), ylim = c(0.40, 0.60), xlim = c(0, 20))
  axis(1, 0:10*2, (0:10)*2000)

  t_seq <- seq(0, temp_N/1000, 0.1)

  # model estimates
  mu_i <- do.call(cbind, lapply(t_seq, get_effect_i, samples = samples_time, i = i))
  polygon(c(t_seq, rev(t_seq)), c(apply(mu_i, 2, quantile, probs = 0.025), rev(apply(mu_i, 2, quantile, probs = 0.975))), col = scales::alpha("blue", 0.5))
  lines(t_seq, apply(mu_i, 2, mean), lwd = 2, col = "blue")

  # observed proportions
  for(ii in 1:floor(temp_N/1000)){
    temp_ids  <- df_time$toss_number > (ii-1) * 1000 & df_time$toss_number <= ii * 1000 & levels(as.factor(df_time_agg$person))[i] == df_time$person
    temp_same <- df_time$toss_start[temp_ids] == df_time$toss_end[temp_ids]
    temp_test <- binom.test(sum(temp_same), length(temp_same))

    points(ii - 0.5, mean(temp_same), pch = 16)
    arrows(x0 = ii - 0.5, y0 = temp_test$conf.int[1], x1 = ii - 0.5, y1 = temp_test$conf.int[2], code = 3, angle = 90, length = 0.05)
  }

  abline(h = 0.50, lty = 2)
  abline(h = 0.51, lty = 2)

  dev.off()
}

plot(colMeans(samples_time$gamma_k), colMeans(samples_time$gamma2_k))
plot(colMeans(inv_logit(samples_time$temp_theta2_k)), colMeans(inv_logit(samples_time$temp_theta_k)), xlab = "Final bias", ylab = "Initial bias", las = 1, asp = 1,
     xlim = c(0.45, 0.65), ylim = c(0.45, 0.65), pch = 16)
abline(a = 0, b = 1)


fx <- function(theta2, theta, t, delta2) {
  inv_logit(theta2 + theta * (t + 1)^delta2)
}
curve(fx(logit(0.50), logit(0.55), x, -0.3), from = 1, to = 100)

### sensitivity analysis without outliers ----
# removing people with more than 0.53% same side bias
df_people <- lapply(unique(df$person), function(person){
  temp_df   <- df[df$person == person,]
  temp_same <- temp_df$toss_start == temp_df$toss_end
  return(cumsum(temp_same) / seq_along(temp_same))
})
person_flips <- do.call(rbind, lapply(unique(df$person), function(person){
  data.frame(
    person = person,
    same   = sum(df$toss_start[df$person == person] == df$toss_end[df$person == person]),
    N      = sum(df$person == person)
  )
}))
person_flips <- person_flips[order(person_flips$N, decreasing = FALSE),]
to_remove    <- person_flips$person[person_flips$same / person_flips$N > 0.53]

s_df_time_agg <- df_time_agg[!df_time_agg$person %in% to_remove,]
s_df_time     <- df_time[!df_time$person %in% to_remove,]

s_data_stan_est <- with(s_df_time_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(s_df_time_agg),
  t                = 1 + mean_toss / 1000
))

s_data_stan_est$theta_alpha           <- 312
s_data_stan_est$theta_beta            <- 312
s_data_stan_est$alpha_alpha           <- 312
s_data_stan_est$alpha_beta            <- 312
s_data_stan_est$sigma_gamma_j_sigma   <- 0.04
s_data_stan_est$sigma_gamma_k_sigma   <- 0.04

s_data_stan_est$bpar_mu             <- 0
s_data_stan_est$bpar_sigma          <- 10
s_data_stan_est$sigma_bpar_k_sigma  <- 1

s_data_stan_est$cpar_mu             <- 0
s_data_stan_est$cpar_sigma          <- 10
s_data_stan_est$sigma_cpar_k_sigma  <- 2

s_data_stan_est$gpar_mu             <- 0
s_data_stan_est$gpar_sigma          <- 5
s_data_stan_est$sigma_gpar_k_sigma  <- 5

s_fit_time <- rstan::sampling(object = model_time, data = s_data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                              init = init_samples(cores = 10),
                              seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
saveRDS(s_fit_time, file = "analyses/outputs/s_fit_time.RDS")
s_fit_time <- readRDS(file = "analyses/outputs/s_fit_time.RDS")
s_samples_time <- extract(s_fit_time)

# check convergence
s_fit_time_summary <- summary(s_fit_time, pars = c("theta", "theta2", "alpha", "bpar", "sigma_gamma_k", "sigma_gamma2_k", "sigma_gamma_j", "sigma_bpar_k"))$summary
s_fit_time_summary

# check chains
rstan::traceplot(s_fit_time, pars = "theta")
rstan::traceplot(s_fit_time, pars = "theta2")
rstan::traceplot(s_fit_time, pars = "alpha")
rstan::traceplot(s_fit_time, pars = "bpar")
rstan::traceplot(s_fit_time, pars = "sigma_gamma_k")
rstan::traceplot(s_fit_time, pars = "sigma_gamma2_k")
rstan::traceplot(s_fit_time, pars = "sigma_gamma_j")
rstan::traceplot(s_fit_time, pars = "sigma_bpar_k")

rstan::stan_dens(s_fit_time, pars = "theta",  separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "theta2", separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "alpha",  separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "bpar",   separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "sigma_gamma_k",  separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "sigma_gamma2_k", separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "sigma_gamma_j",  separate_chains = TRUE)
rstan::stan_dens(s_fit_time, pars = "sigma_bpar_k",   separate_chains = TRUE)

# report estimates
report_est(s_samples_time$theta)   # "0.5011 [0.4966, 0.5045]"
report_est(s_samples_time$theta2)  # "0.5177 [0.5065, 0.5292]"
report_est(inv_logit(logit(s_samples_time$theta) + logit(s_samples_time$theta2))) #  "0.5188 [0.5089, 0.5300]"

report_est(inv_logit(s_samples_time$sigma_gamma_k) - 0.50)  # "0.0028 [0.0001, 0.0067]"
report_est(inv_logit(s_samples_time$sigma_gamma2_k) - 0.50) # "0.0218 [0.0135, 0.0312]"
report_est(inv_logit(sqrt(s_samples_time$sigma_gamma_k^2 + s_samples_time$sigma_gamma2_k^2)) - 0.50) #  "0.0221 [0.0139, 0.0313]"

report_est(s_samples_time$alpha) # "0.5008 [0.4989, 0.5030]"
report_est(inv_logit(s_samples_time$sigma_gamma_j) - 0.50) # "0.0020 [0.0001, 0.0050]"

report_est(s_samples_time$bpar) # "-1.4614 [-3.2027, -0.6241]"
report_est(s_samples_time$sigma_bpar_k) # "0.5587 [0.0309, 1.6081]"


pdf("figures/panel3s.pdf", width = 10, height = 8)

layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(1, 0.75))
par(mar = c(4, 4.5, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", ylim = c(0.45, 0.55), xlim = c(0, 20), las = 1, bty = "n", yaxt = "n", xaxt = "n")

axis(2, at = seq(0.45, 0.55, 0.05), las = 1, cex.axis = 1.10)
axis(1, 0:10 * 2, (0:10)*2000)
mtext("Coin flips", 1, line = 3)
mtext("Pr(same side)", 2, line = 3)

fpow       <- function(x, a, b, c){
  a + b * x^c
}
get_effect <- function(t, samples){

  alpha   <- logit(samples$alpha)
  theta   <- logit(samples$theta)
  theta2  <- logit(samples$theta2)
  bpar    <- samples$bpar

  return(inv_logit(fpow(t, theta, theta2, bpar)))
}


t_seq <- seq(0, 20, 0.1)
mu <- do.call(cbind, lapply(t_seq+1, get_effect, samples = s_samples_time))
polygon(c(t_seq, rev(t_seq)), c(apply(mu, 2, quantile, probs = 0.025), rev(apply(mu, 2, quantile, probs = 0.975))), col = scales::alpha("black", 0.5))
lines(t_seq, apply(mu, 2, median), lwd = 2, col = "black")

for(i in 1:20){
  temp_ids  <- s_df_time$toss_number > (i-1) * 1000 & s_df_time$toss_number <= i * 1000
  temp_same <- s_df_time$toss_start[temp_ids] == s_df_time$toss_end[temp_ids]
  temp_test <- binom.test(sum(temp_same), length(temp_same))

  points(i - 0.5, mean(temp_same), pch = 21, col = "darkblue")
  lines(rep(i - 0.5, 2), c(temp_test$conf.int[1], temp_test$conf.int[2]), col = "darkblue")
}

lines(c(0, 20), c(0.50, 0.50), lty = 3)
lines(c(0, 20), c(0.51, 0.51), lty = 3)
text(20, 0.50 - 0.002, "chance", adj = c(1, 1), cex = 1.25)
text(20, 0.51 + 0.002, "DHM",  adj = c(1, 0), cex = 1.25)

legend(x = 0, y = 0.55, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"),
       pch = c(21, NA), col = c("darkblue", "black"), bty = "n", lty = 1, cex = 1.25)



par(mar = c(4, 4.5, 0, 0))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0.45, 0.55), ylim = c(0, 250), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.45, 0.55, 0.025), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.10)
mtext("Density", 2, line = 3)
mtext("Pr(same side)", 1, line = 2.5)

x_prior <- seq(0.45, 0.55, length.out = 501)
d_prior <- dbeta(x = x_prior, shape1 = 312, shape2 = 312)
lines(x_prior, d_prior, lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(s_samples_time$theta)
d_post2 <- polspline::logspline(s_samples_time$theta2)

x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
x_post2 <- seq(polspline::qlogspline(0.0005, d_post2), polspline::qlogspline(0.9995, d_post2), length.out = 101)

lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")
lines(x_post2, polspline::dlogspline(x_post2, d_post2), lwd = 2, lty = 4, col = "black")

lines(c(0.50, 0.50), c(0, 250), lty = 3)
lines(c(0.51, 0.51), c(0, 250), lty = 3)

text(0.50 - 0.001, 250, "chance",   adj = c(1, 1), cex = 1.25)
text(0.51 + 0.001, 250, "DHM", adj = c(0, 1), cex = 1.25)


par(mar = c(4, 4, 0, 0.5))
plot(NA, type = "n", main = "", xlab = "", ylab = "", xlim = c(0, 0.05), ylim = c(0, 250), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0, 0.05, 0.01), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 250, 50), las = 1, cex.axis = 1.10)
mtext("Between-people heterogeneity in Pr(same side)", 1, line = 2.5)

x_prior <- seq(0, logit(0.55), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.04)*2

lines(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2)

d_post  <- polspline::logspline(inv_logit(s_samples_time$sigma_gamma_k) - 0.50, lbound = 0)
d_post2 <- polspline::logspline(inv_logit(s_samples_time$sigma_gamma2_k) - 0.50, lbound = 0)

x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post),  length.out = 101)
x_post2 <- seq(polspline::qlogspline(0.0005, d_post2), polspline::qlogspline(0.9995, d_post2), length.out = 101)

lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = "black")
lines(x_post2, polspline::dlogspline(x_post2, d_post2), lwd = 2, lty = 4, col = "black")

legend(x = 0.02, y = 250, c("Prior", "Posterior Baseline", "Posterior Toss-Dep."), lwd = 2, lty = c(2, 1, 4), col = c("grey", "black", "black"), bty = "n", cex = 1.25)
dev.off()

#### add no-pooling models ----

model_time_ind <- rstan::stan_model("functions/model-a1_jk_t4-est-ind.stan")
set.seed(1)
fit_time_ind <- rstan::sampling(object = model_time_ind, data = data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                             seed = 1, control = list(adapt_delta = 0.9, max_treedepth = 15))
saveRDS(fit_time_ind, file = "analyses/outputs/fit_time-ind.RDS", compress = "xz")
fit_time_ind <- readRDS(file = "analyses/outputs/fit_time-ind.RDS")

samples_time_ind <- extract(fit_time_ind)


get_effect_i <- function(t, i, samples){

  theta  <- logit(samples$theta[,i])
  theta2 <- logit(samples$theta2[,i])
  delta2 <- samples$delta2[,i]

  return(inv_logit((theta-theta2) * t^-exp(delta2) + theta2))
}

plot(NA, type = "n", las = 1, xlab = "Coin flips", xaxt = "n", ylab = "Probability (same side)", ylim = c(0.45, 0.55), xlim = c(0, 20))
axis(1, 0:10 * 2, (0:10)*2000)
for(i in 1:47){

  temp_df   <- df_time_agg[df_time_agg$person == levels(as.factor(df_time_agg$person))[i],]
  temp_N    <- sum(temp_df$N_start_heads_up) + sum(temp_df$N_start_tails_up)

  t_seq <- seq(0, temp_N/1000, 0.1)

  mu_i <- do.call(cbind, lapply(t_seq, get_effect_i, samples = samples_time_ind, i = i))
  lines(t_seq, apply(mu_i, 2, median), lwd = 1)

}
abline(h = 0.50, lty = 2)
abline(h = 0.51, lty = 2)

sapply(1:47, function(i) mean(samples_time_ind$theta[,i]))
hist(sapply(1:47, function(i) mean(samples_time_ind$theta[,i])), breaks = 50)

par(mfrow = c(1, 2))
plot(NA, type = "n", xlim = c(0.2, 0.8), ylim = c(1, 47), ylab = "", xlab = "Initial probability of same side", yaxt = "n", bty = "n")
y_order <- order(sapply(1:47, function(i) mean(samples_time_ind$theta[,i])))
for(i in seq_along(y_order)){

  points(mean(samples_time_ind$theta[,y_order[i]]), i, pch = 16)
  arrows(
    x0 = quantile(samples_time_ind$theta[,y_order[i]], probs = 0.025),
    x1 = quantile(samples_time_ind$theta[,y_order[i]], probs = 0.975),
    y0 = i, y1 = i, code = 3, angle = 90, length = 0.05)


#  temp_df   <- df_time_agg[df_time_agg$person == levels(as.factor(df_time_agg$person))[y_order[i]],]
#  temp_N    <- sum(temp_df$N)
#  temp_same <- sum(temp_df$same_side)
#  points(temp_same/temp_N, i, pch = 16, col = "blue")
}
abline(v = 0.50, lty = 2)
abline(v = 0.51, lty = 2)

plot(NA, type = "n", xlim = c(0.20, 0.80), ylim = c(1, 47), ylab = "", xlab = "Asymptotic probability of same side", yaxt = "n", bty = "n")
y_order <- order(sapply(1:47, function(i) mean(samples_time_ind$theta[,i])))
for(i in seq_along(y_order)){

  points(mean(samples_time_ind$theta2[,y_order[i]]), i, pch = 16)
  arrows(
    x0 = quantile(samples_time_ind$theta2[,y_order[i]], probs = 0.025),
    x1 = quantile(samples_time_ind$theta2[,y_order[i]], probs = 0.975),
    y0 = i, y1 = i, code = 3, angle = 90, length = 0.05)
}
abline(v = 0.50, lty = 2)
abline(v = 0.51, lty = 2)

df_est <- cbind.data.frame(
  est = sapply(1:47, function(i) mean(samples_time_ind$theta[,i])),
  se  = sapply(1:47, function(i) sd(samples_time_ind$theta[,i]))
)
metafor::rma(df_est$est, sei = df_est$se, method = "REML")



### non-informative priors
### this script visualizes time trends of the same-side bias (and produces corresponding figures) ----
library(rstan) # rstan version 2.26.22 (Stan version 2.26.1)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)
source(file = "functions/binomial-test.R")


df          <- read.csv(file = "analyses/data_long.csv")
df_time     <- read.csv(file = "analyses/df_time.csv")
df_time_agg <- read.csv(file = "analyses/df_time_agg.csv")

# removing irmaT (too few flips)
df_time_agg <- df_time_agg[!df_time_agg$person %in% c("irmaT"),]
df_time     <- df_time[!df_time$person %in% c("irmaT"),]

data_stan_est <- with(df_time_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(df_time_agg),
  t                = df_time_agg$mean_toss / 1000
))

data_stan_est$theta_alpha           <- 1
data_stan_est$theta_beta            <- 1
data_stan_est$alpha_alpha           <- 312
data_stan_est$alpha_beta            <- 312
data_stan_est$delta_mu              <- 0
data_stan_est$delta_sigma           <- 1
data_stan_est$delta2_mu             <- 0
data_stan_est$delta2_sigma          <- 1
data_stan_est$sigma_gamma_j_sigma   <- 0.04
data_stan_est$sigma_gamma_k_sigma   <- 0.04
data_stan_est$sigma_delta_k_sigma   <- 1
data_stan_est$sigma_delta2_k_sigma  <- 10

model_time_ind <- rstan::stan_model("functions/model-a1_jk_time-est-ind.stan")
set.seed(1)
fit_time_ind_broad <- rstan::sampling(object = model_time_ind, data = data_stan_est, chains = 10, warmup = 15000, iter = 25000,
                                 seed = 1, control = list(adapt_delta = 0.9, max_treedepth = 15))
saveRDS(fit_time_ind_broad, file = "analyses/outputs/fit_time-ind-broad.RDS", compress = "xz")

### frequentist re-analysis ----
library(lme4)
library(lmerTest)
library(emmeans)

df          <- read.csv(file = "analyses/data_long.csv")
df_time     <- read.csv(file = "analyses/df_time.csv")
df_time_agg <- read.csv(file = "analyses/df_time_agg.csv")

# computed variables
df_time$time         <- (df_time$toss_number  - 1) / 10000
df_time$same_side    <- df_time$toss_start == df_time$toss_end
df_time$start_effect <- ifelse(df_time$toss_start == "h", 1, -1)

# the models converges when time
fit      <- glmer(same_side ~ 1 + start_effect + (1 | person), family = binomial, data = df_time)
fit_t1   <- glmer(same_side ~ 1 + time + start_effect + (1 + time | person), family = binomial, data = df_time)
fit_t2   <- glmer(same_side ~ 1 + time + I(time^2) + start_effect + (1 + time + I(time^2) | person), family = binomial, data = df_time)

summary(fit)
summary(fit_t1)
summary(fit_t2)

anova(fit,    fit_t1)
anova(fit_t1, fit_t2)

as.data.frame(emmeans::emmeans(
  fit_t2, ~ 1 + time + I(time^2) + start_effect,
  at = list(
    time         = 0,
    start_effect = 0
  ), type = "response"))

predict_t1 <- as.data.frame(emmeans::emmeans(
  fit_t1, ~ 1 + time + start_effect,
  at = list(
    time         = (c(1:20000)-1) / 10000,
    start_effect = 0
  ), rg.limit = 1e6, type = "response"))
predict_t2 <- as.data.frame(emmeans::emmeans(
  fit_t2, ~ 1 + time + I(time^2) + start_effect,
  at = list(
    time         = (c(1:20000)-1) / 10000,
    start_effect = 0
), rg.limit = 1e6, type = "response"))


plot(NA, type = "n", las = 1, xlab = "Coin flips", xaxt = "n", ylab = "Probability (same side)", ylim = c(0.45, 0.55), xlim = c(0, 20000))
axis(1, 0:10 * 2000)

polygon(c(predict_t1$time, rev(predict_t1$time))*10000, c(predict_t1$asymp.LCL, rev(predict_t1$asymp.UCL)),
        col = scales::alpha("black", 0.5))
lines(predict_t1$time*10000, predict_t1$prob, lwd = 2, col = "black")

abline(h = 0.50, lty = 2)
abline(h = 0.51, lty = 2)



pdf("figures/panel3f.pdf", width = 10, height = 4.6)
par(mar = c(4, 4.5, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", ylim = c(0.45, 0.55), xlim = c(0, 20000), las = 1, bty = "n", yaxt = "n", xaxt = "n")

axis(2, at = seq(0.45, 0.55, 0.05), las = 1, cex.axis = 1.10)
axis(1, 0:10 * 2000, cex.axis = 1.10)
mtext("Coin flips", 1, line = 3, cex = 1.25)
mtext("Pr(landing on same side)", 2, line = 3, cex = 1.25)

polygon(c(predict_t2$time, rev(predict_t2$time))*10000, c(predict_t2$asymp.LCL, rev(predict_t2$asymp.UCL)),
        col = scales::alpha("black", 0.5))
lines(predict_t2$time*10000, predict_t2$prob, lwd = 2, col = "black")

for(i in 1:20){
  temp_ids  <- df_time$toss_number > (i-1) * 1000 & df_time$toss_number <= i * 1000
  temp_same <- df_time$toss_start[temp_ids] == df_time$toss_end[temp_ids]
  temp_test <- binom.test(sum(temp_same), length(temp_same))

  points((i - 0.5) * 1000, mean(temp_same), pch = 21, col = "darkblue")
  lines(c(i - 0.5, i - 0.5) * 1000, c(temp_test$conf.int[1], temp_test$conf.int[2]), col = "darkblue")
}

lines(c(0, 20000), c(0.50, 0.50), lty = 3)
lines(c(0, 20000), c(0.51, 0.51), lty = 3)
text(20000, 0.50 - 0.002, "chance", adj = c(1, 1), cex = 1.25)
text(20000, 0.51 + 0.002, "DHM",  adj = c(1, 0), cex = 1.25)

legend(x = 0, y = 0.465, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"),
       pch = c(21, NA), col = c("darkblue", "black"), bty = "n", lty = 1, cex = 1.25)
dev.off()

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

s_df_time  <- df_time[!df_time$person %in% to_remove,]

s_fit      <- glmer(same_side ~ 1 + start_effect + (1 | person), family = binomial, data = s_df_time)
s_fit_t1   <- glmer(same_side ~ 1 + time + start_effect + (1 + time | person), family = binomial, data = s_df_time)
s_fit_t2   <- glmer(same_side ~ 1 + time + I(time^2) + start_effect + (1 + time + I(time^2) | person), family = binomial, data = s_df_time)


summary(s_fit)
summary(s_fit_t1)
summary(s_fit_t2)

as.data.frame(emmeans::emmeans(
  s_fit_t2, ~ 1 + time + I(time^2) + start_effect,
  at = list(
    time         = 0,
    start_effect = 0
  ), type = "response"))

anova(s_fit,    s_fit_t1)
anova(s_fit_t1, s_fit_t2)

s_predict_t1 <- as.data.frame(emmeans::emmeans(
  s_fit_t1, ~ 1 + time + start_effect,
  at = list(
    time         = (c(1:20000)-1) / 10000,
    start_effect = 0
  ), rg.limit = 1e6, type = "response"))
s_predict_t2 <- as.data.frame(emmeans::emmeans(
  s_fit_t2, ~ 1 + time + I(time^2) + start_effect,
  at = list(
    time         = (c(1:20000)-1) / 10000,
    start_effect = 0
  ), rg.limit = 1e6, type = "response"))


plot(NA, type = "n", las = 1, xlab = "Coin flips", xaxt = "n", ylab = "Probability (same side)", ylim = c(0.45, 0.55), xlim = c(0, 20000))
axis(1, 0:10 * 2000)

polygon(c(s_predict_t1$time, rev(s_predict_t1$time))*10000, c(s_predict_t1$asymp.LCL, rev(s_predict_t1$asymp.UCL)),
        col = scales::alpha("black", 0.5))
lines(s_predict_t1$time*10000, s_predict_t1$prob, lwd = 2, col = "black")

abline(h = 0.50, lty = 2)
abline(h = 0.51, lty = 2)


pdf("figures/panel3fs.pdf", width = 10, height = 4.6)
par(mar = c(4, 4.5, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", ylim = c(0.45, 0.55), xlim = c(0, 20000), las = 1, bty = "n", yaxt = "n", xaxt = "n")

axis(2, at = seq(0.45, 0.55, 0.05), las = 1, cex.axis = 1.10)
axis(1, 0:10 * 2000, cex.axis = 1.10)
mtext("Coin flips", 1, line = 3, cex = 1.25)
mtext("Pr(landing on same side)", 2, line = 3, cex = 1.25)

polygon(c(s_predict_t2$time, rev(s_predict_t2$time))*10000, c(s_predict_t2$asymp.LCL, rev(s_predict_t2$asymp.UCL)),
        col = scales::alpha("black", 0.5))
lines(s_predict_t2$time*10000, s_predict_t2$prob, lwd = 2, col = "black")

for(i in 1:20){
  temp_ids  <- s_df_time$toss_number > (i-1) * 1000 & s_df_time$toss_number <= i * 1000
  temp_same <- s_df_time$toss_start[temp_ids] == s_df_time$toss_end[temp_ids]
  temp_test <- binom.test(sum(temp_same), length(temp_same))

  points((i - 0.5) * 1000, mean(temp_same), pch = 21, col = "darkblue")
  lines(c(i - 0.5, i - 0.5) * 1000, c(temp_test$conf.int[1], temp_test$conf.int[2]), col = "darkblue")
}

lines(c(0, 20000), c(0.50, 0.50), lty = 3)
lines(c(0, 20000), c(0.51, 0.51), lty = 3)
text(20000, 0.50 - 0.002, "chance", adj = c(1, 1), cex = 1.25)
text(20000, 0.51 + 0.002, "DHM",  adj = c(1, 0), cex = 1.25)

legend(x = 0, y = 0.465, legend = c("Observed proportion and 95% CI", "Hierarchical mean estimate and 95% CI"),
       pch = c(21, NA), col = c("darkblue", "black"), bty = "n", lty = 1, cex = 1.25)
dev.off()


### descriptive
rolling_average <- function(x, n){
  cx   <- c(0,cumsum(x))
  rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n
  return(data.frame(x = 1:length(rsum), y = rsum))
}

pdf("figures/panel3d.pdf", width = 10, height = 4.6)
par(mar = c(4, 4.5, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", ylim = c(0.40, 0.60), xlim = c(0, 20000), las = 1, bty = "n", yaxt = "n", xaxt = "n")

axis(2, at = seq(0.40, 0.60, 0.05), las = 1, cex.axis = 1.10)
axis(1, 0:10 * 2000, cex.axis = 1.10)
mtext("Coin flips", 1, line = 3, cex = 1.25)
mtext("Pr(landing on same side)", 2, line = 3, cex = 1.25)
for(p in unique(df_time$person)){
  temp_df         <- df_time[df_time$person == p,]
  lines(rolling_average(temp_df$toss_start == temp_df$toss_end, 1000), col = "grey")
}
temp_ma <- do.call(rbind, lapply(1:(max(df_time$toss_number) - 999), function(i){
  temp_df   <- df_time[df_time$toss_number >= i & df_time$toss_number < i + 1000,]
  return(data.frame(x = i, y = mean(temp_df$toss_start == temp_df$toss_end)))
}))
lines(temp_ma, col = "black", lwd = 2)

lines(c(0, 20000), c(0.50, 0.50), lty = 3)
lines(c(0, 20000), c(0.51, 0.51), lty = 3)
text(20000, 0.50 - 0.002, "chance", adj = c(1, 1), cex = 1.25)
text(20000, 0.51 + 0.002, "DHM",  adj = c(1, 0), cex = 1.25)
dev.off()
