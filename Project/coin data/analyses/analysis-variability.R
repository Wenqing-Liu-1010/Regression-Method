### this script performs analysis of variability by the site ----
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(rstan) # rstan version 2.26.22 (Stan version 2.26.1)
library(bridgesampling)
rstan_options(auto_write = TRUE)
options(mc.cores = 10)
source(file = "functions/binomial-test.R")

df     <- read.csv(file = "analyses/data_long.csv")
df_agg <- read.csv(file = "analyses/data_agg.csv")

# order by data collection--corresponds to the recruitment order (some people participated in multiple data-collections, but should have only one label)
df$dataset   <- factor(df$dataset, levels = c("Bc Thesis", "Marathon", "Internet", "Marathon-Manheim", "Marathon-MSc", "Marathon-PhD", "top-up"))
df           <- df[order(df$dataset),]
df$recruited <- NA
for(person in unique(df$person)){
  df$recruited[df$person == person] <- as.character(unique(df$dataset[df$person == person])[1])
}
df_rec <- df[!duplicated(df$person),c("person", "recruited")]
df_agg <- merge(df_agg, df_rec, by = "person")

library(BayesTools) # add mean diff contrast
df_agg$recruited <- factor(df_agg$recruited)
contrasts(df_agg$recruited) <- contr.meandif

data_stan <- with(df_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(df_agg),
  x                = model.matrix(~ df_agg$recruited)[,-1],
  L                = length(unique(df$recruited)) - 1
))


data_stan$theta_alpha          <- 312
data_stan$theta_beta           <- 312
data_stan$alpha_alpha          <- 312
data_stan$alpha_beta           <- 312
data_stan$sigma_gamma_j_sigma  <- 0.04
data_stan$sigma_gamma_k_sigma  <- 0.04
data_stan$sigma_beta           <- 0.20

model_reg <- rstan::stan_model("functions/hierarchical_estimation/model-a1-jk-reg-est.stan")
set.seed(1)
fit_reg      <- rstan::sampling(object = model_reg, data = data_stan, chains = 10, warmup = 15000, iter = 25000,
                                seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
saveRDS(fit_reg, file = "analyses/outputs/fit_reg.RDS", compress = "xz")

fit_reg <- readRDS(file = "analyses/outputs/fit_reg.RDS")
samples_reg  <- rstan::extract(fit_reg)

# check convergence
summary_fit_reg <- summary(fit_reg, pars = c("theta", "alpha", "beta", "sigma_gamma_k", "sigma_gamma_j"))
summary_fit_reg$summary

# check chains
rstan::traceplot(fit_reg, pars = "theta")
rstan::traceplot(fit_reg, pars = "alpha")
rstan::traceplot(fit_reg, pars = "beta")
rstan::traceplot(fit_reg, pars = "sigma_gamma_k")
rstan::traceplot(fit_reg, pars = "sigma_gamma_j")

rstan::stan_dens(fit_reg, pars = "theta", separate_chains = TRUE)
rstan::stan_dens(fit_reg, pars = "alpha", separate_chains = TRUE)
rstan::stan_dens(fit_reg, pars = "beta", separate_chains = TRUE)
rstan::stan_dens(fit_reg, pars = "sigma_gamma_k", separate_chains = TRUE)
rstan::stan_dens(fit_reg, pars = "sigma_gamma_j", separate_chains = TRUE)

# return estimates from the contrast matrix
dev_recruited     <- samples_reg$beta %*% t(contrasts(df_agg$recruited))
samples_recruited <- inv_logit(matrix(logit(samples_reg$theta), ncol = length(unique(df_agg$recruited)), nrow = length(samples_reg$theta)) + dev_recruited)

x_prior <- seq(logit(0.40), logit(0.60), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.20)

pdf("figures/panel4.pdf", width = 7.5, height = 4.6)
par(mar = c(4, 4, 0, 0.5))
cols  <- palette.colors(ncol(dev_recruited))
plot(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2,
     type = "l", ylim = c(0, 100), xlim = c(-0.10, 0.10), las = 1, bty = "n", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "")
mtext("By-recruitment side deviation from Pr(same side)", 1, line = 3, cex = 1.25)
mtext("Density", 2, line = 3, cex = 1.25)
axis(1, at = seq(-0.1, 0.1, 0.05), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 100, 20), las = 1, cex.axis = 1.10)
for(i in 1:ncol(dev_recruited)){
  d_post  <- polspline::logspline(inv_logit(dev_recruited[,i])  - 0.50)
  x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
  lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = cols[i])
}
legend("topright", c("Prior", levels(df_agg$recruited)), lwd = 2, lty = c(2, rep(1, ncol(dev_recruited))), col = c("grey", cols), bty = "n", cex = 1.25)
dev.off()

# compute difference from the grand mean
do.call(rbind, lapply(1:ncol(dev_recruited), function(i){
  data.frame(
    "recruited" = colnames(dev_recruited)[i],
    "mean"      = mean(    inv_logit(dev_recruited[,i]) - 0.50),
    "lCI"       = quantile(inv_logit(dev_recruited[,i]) - 0.50, 0.025),
    "uCI"       = quantile(inv_logit(dev_recruited[,i]) - 0.50, 0.975)
  )
}))


### omit outliers
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

# create a stan data object
s_data_stan <- with(s_df_agg, list(
  heads_heads      = heads_heads,
  tails_heads      = tails_heads,
  N_start_heads_up = N_start_heads_up,
  N_start_tails_up = N_start_tails_up,
  map_k            = as.numeric(as.factor(person)),
  map_j            = as.numeric(as.factor(coin)),
  K                = length(levels(as.factor(person))),
  J                = length(levels(as.factor(coin))),
  N                = nrow(s_df_agg),
  x                = model.matrix(~ s_df_agg$recruited)[,-1],
  L                = length(unique(df$recruited)) - 1
))


s_data_stan$theta_alpha          <- 312
s_data_stan$theta_beta           <- 312
s_data_stan$alpha_alpha          <- 312
s_data_stan$alpha_beta           <- 312
s_data_stan$sigma_gamma_j_sigma  <- 0.04
s_data_stan$sigma_gamma_k_sigma  <- 0.04
s_data_stan$sigma_beta           <- 0.20

set.seed(1)
s_fit_reg      <- rstan::sampling(object = model_reg, data = s_data_stan, chains = 10, warmup = 15000, iter = 25000,
                                  seed = 1, control = list(adapt_delta = 0.95, max_treedepth = 15))
saveRDS(s_fit_reg, file = "analyses/outputs/s_fit_reg.RDS", compress = "xz")

s_fit_reg <- readRDS(file = "analyses/outputs/s_fit_reg.RDS")
s_samples_reg  <- rstan::extract(s_fit_reg)

# check convergence
s_summary_fit_reg <- summary(s_fit_reg, pars = c("theta", "alpha", "beta", "sigma_gamma_k", "sigma_gamma_j"))
s_summary_fit_reg$summary

# check chains
rstan::traceplot(s_fit_reg, pars = "theta")
rstan::traceplot(s_fit_reg, pars = "alpha")
rstan::traceplot(s_fit_reg, pars = "beta")
rstan::traceplot(s_fit_reg, pars = "sigma_gamma_k")
rstan::traceplot(s_fit_reg, pars = "sigma_gamma_j")

rstan::stan_dens(s_fit_reg, pars = "theta", separate_chains = TRUE)
rstan::stan_dens(s_fit_reg, pars = "alpha", separate_chains = TRUE)
rstan::stan_dens(s_fit_reg, pars = "beta", separate_chains = TRUE)
rstan::stan_dens(s_fit_reg, pars = "sigma_gamma_k", separate_chains = TRUE)
rstan::stan_dens(s_fit_reg, pars = "sigma_gamma_j", separate_chains = TRUE)

# return estimates from the contrast matrix
s_dev_recruited     <- s_samples_reg$beta %*% t(contrasts(df_agg$recruited))
s_samples_recruited <- inv_logit(matrix(logit(s_samples_reg$theta), ncol = length(unique(df_agg$recruited)), nrow = length(s_samples_reg$theta)) + s_dev_recruited)

x_prior <- seq(logit(0.40), logit(0.60), length.out = 501)
d_prior <- dnorm(x = x_prior, 0, 0.20)

pdf("figures/panel4s.pdf", width = 7.5, height = 4.6)
par(mar = c(4, 4, 0, 0.5))
cols  <- palette.colors(ncol(s_dev_recruited))
plot(inv_logit(x_prior) - 0.50, d_prior / inv_logit.jac(x_prior), lwd = 2, col = "grey", lty = 2,
     type = "l", ylim = c(0, 150), xlim = c(-0.10, 0.10), las = 1, bty = "n", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "")
mtext("By-recruitment side deviation from Pr(same side)", 1, line = 3, cex = 1.25)
mtext("Density", 2, line = 3, cex = 1.25)
axis(1, at = seq(-0.1, 0.1, 0.05), las = 1, cex.axis = 1.10)
axis(2, at = seq(0, 150, 50), las = 1, cex.axis = 1.10)
for(i in 1:ncol(s_dev_recruited)){
  d_post  <- polspline::logspline(inv_logit(s_dev_recruited[,i])  - 0.50)
  x_post  <- seq(polspline::qlogspline(0.0005, d_post),  polspline::qlogspline(0.9995, d_post), length.out = 101)
  lines(x_post, polspline::dlogspline(x_post, d_post), lwd = 2, col = cols[i])
}
legend("topright", c("Prior", levels(df_agg$recruited)), lwd = 2, lty = c(2, rep(1, ncol(s_dev_recruited))), col = c("grey", cols), bty = "n", cex = 1.25)
dev.off()

# compute difference from the grand mean
do.call(rbind, lapply(1:ncol(s_dev_recruited), function(i){
  data.frame(
    "recruited" = colnames(s_dev_recruited)[i],
    "mean"      = mean(    inv_logit(s_dev_recruited[,i]) - 0.50),
    "lCI"       = quantile(inv_logit(s_dev_recruited[,i]) - 0.50, 0.025),
    "uCI"       = quantile(inv_logit(s_dev_recruited[,i]) - 0.50, 0.975)
  )
}))


### frequentist analysis ----
library(lme4)
library(lmerTest)
library(emmeans)

df$same_side    <- df$toss_start == df$toss_end
df$start_effect <- ifelse(df$toss_start == "h", 1, -1)

fit_glmer0 <- glmer(same_side ~ 1 + start_effect + (1 | person),             family = binomial, data = df)
fit_glmer1 <- glmer(same_side ~ 1 + start_effect + recruited + (1 | person), family = binomial, data = df)

summary(fit_glmer0)
summary(fit_glmer1)

anova(fit_glmer0, fit_glmer1) # not a significant difference

predict_glmer1 <- as.data.frame(emmeans::emmeans(
  fit_glmer1, ~  1 + start_effect + recruited ,
  at = list(
    recruited    = c("Bc Thesis", "Marathon", "Internet", "Marathon-Manheim", "Marathon-MSc", "Marathon-PhD"),
    start_effect = 0
  ), type = "response"))

pdf("figures/panel4f.pdf", width = 7.5, height = 4.6)
par(mar = c(4, 0, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", xlim = c(0.40, 0.60), ylim = c(0, 6.2), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.45, 0.55, 0.05), las = 1, cex.axis = 1.10)
mtext("Pr(landing on same side)", 1, line = 3, cex = 1.25)

for(i in 1:nrow(predict_glmer1)){
  text(0.40, i - 0.5, predict_glmer1$recruited[i], adj = c(0, 0.5), cex = 1.25)
  text(0.60, i - 0.5, sprintf("%1$.3f [%2$.3f, %3$.3f]", predict_glmer1$prob[i], predict_glmer1$asymp.LCL[i], predict_glmer1$asymp.UCL[i]), adj = c(1, 0.5), cex = 1.25)
  points(predict_glmer1$prob[i], i - 0.5, pch = 16, col = "black")
  lines(predict_glmer1[i, c("asymp.LCL", "asymp.UCL")], c(i - 0.5, i - 0.5), col = "black")
}

lines(c(0.50, 0.50), c(0, 6), lty = 3)
lines(c(0.51, 0.51), c(0, 6), lty = 3)
text(0.50 - 0.002, 6.2, "chance", adj = c(1, 1), cex = 1.25)
text(0.51 + 0.002, 6.2, "DHM",  adj = c(0, 1), cex = 1.25)

dev.off()


# remove people with more than 0.53% same side bias
s_df  <- df[!df$person %in% to_remove,]

s_fit_glmer0 <- glmer(same_side ~ 1 + start_effect + (1 | person),             family = binomial, data = s_df)
s_fit_glmer1 <- glmer(same_side ~ 1 + start_effect + recruited + (1 | person), family = binomial, data = s_df)

anova(s_fit_glmer0, s_fit_glmer1) # not a significant difference

s_predict_glmer1 <- as.data.frame(emmeans::emmeans(
  s_fit_glmer1, ~  1 + start_effect + recruited ,
  at = list(
    recruited    = c("Bc Thesis", "Marathon", "Internet", "Marathon-Manheim", "Marathon-MSc", "Marathon-PhD"),
    start_effect = 0
  ), type = "response"))

pdf("figures/panel4fs.pdf", width = 7.5, height = 4.6)
par(mar = c(4, 0, 0, 0.5))
plot(NA, type = "n", las = 1, xlab = "", xaxt = "n", ylab = "", xlim = c(0.40, 0.60), ylim = c(0, 6.2), las = 1, bty = "n", yaxt = "n", xaxt = "n")
axis(1, at = seq(0.45, 0.55, 0.05), las = 1, cex.axis = 1.10)
mtext("Pr(landing on same side)", 1, line = 3, cex = 1.25)

for(i in 1:nrow(s_predict_glmer1)){
  text(0.40, i - 0.5, s_predict_glmer1$recruited[i], adj = c(0, 0.5), cex = 1.25)
  text(0.60, i - 0.5, sprintf("%1$.3f [%2$.3f, %3$.3f]", s_predict_glmer1$prob[i], s_predict_glmer1$asymp.LCL[i], s_predict_glmer1$asymp.UCL[i]), adj = c(1, 0.5), cex = 1.25)
  points(s_predict_glmer1$prob[i], i - 0.5, pch = 16, col = "black")
  lines(s_predict_glmer1[i, c("asymp.LCL", "asymp.UCL")], c(i - 0.5, i - 0.5), col = "black")
}

lines(c(0.50, 0.50), c(0, 6), lty = 3)
lines(c(0.51, 0.51), c(0, 6), lty = 3)
text(0.50 - 0.002, 6.2, "chance", adj = c(1, 1), cex = 1.25)
text(0.51 + 0.002, 6.2, "DHM",  adj = c(0, 1), cex = 1.25)

dev.off()

