binomial_test <- function(x, n, theta, alpha, beta, positive = TRUE){

  marglik_h0 <- dbinom(x, n, theta, log = TRUE)

  if(positive){
    marglik_h1 <- log(integrate(
      f     = function(theta) {
        exp(dbinom(x, n, theta, log = TRUE) + dbeta(theta, alpha, beta, log = TRUE) - pbeta(0.5, alpha, beta, lower.tail = FALSE, log.p = TRUE))
      },
      lower = 0.5 ,
      upper = 1
    )$value)
  }else{
    marglik_h1 <- extraDistr::dbbinom(x, n, alpha, beta, log = TRUE)
  }

  logBF_10 <- marglik_h1 - marglik_h0

  return(exp(logBF_10))
}
binomial_est  <- function(x, n, alpha = 1, beta = 1, print = TRUE){
  est <- (x + alpha) / (n + alpha + beta)
  lCI <- qbeta(0.025, x + alpha, n - x + beta)
  uCI <- qbeta(0.975, x + alpha, n - x + beta)

  x        <- c(est, lCI, uCI)
  names(x) <- c("est", "lCI", "uCI")

  if(print){
    x <- round(x, 4)
    print(sprintf("%1$.4f [%2$.4f, %3$.4f]",x[1], x[2], x[3]))
  }else{
      return(x)
  }
}
abbreviate_names <- function(x){
  x[x=="EJ"] <- "EricJW"
  x  <- paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  x1 <- stringr::str_extract(x, "[A-Z][a-z]*")
  x2 <- substr(x, nchar(x1) + 1, nchar(x))
  x2 <- gsub("[a-z]","", x2)
  return(paste0(x1, x2))
}
inv_logit     <- function(x) 1/(1+exp(-x))
logit         <- function(x) log(x/(1-x))
inv_logit.jac <- function(x) exp(-x)/(exp(-x) + 1)^2
report_est    <- function(x) {
  sprintf("%1$.4f [%2$.4f, %3$.4f]",
          round(mean(x), 4),
          round(quantile(x, prob = 0.025), 4),
          round(quantile(x, prob = 0.975), 4))
}
momnorm_pdf   <- function(x, mode){

  tau <- mode/sqrt(2)
  lik <- (1/2) * (sqrt(2) * x^2) / (sqrt(pi) * tau^3) * exp(-x^2/(2*tau^2))

  return(lik)
}
