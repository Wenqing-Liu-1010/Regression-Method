functions {

  // log probability density function for the moment normal distribution
  real momnorm_lpdf(real x, real mode) {

    real tau = abs(mode)/sqrt(2);
    real lik = (0.5) * (sqrt(2) * pow(x, 2)) / (sqrt(pi()) * pow(tau, 3)) * exp(-pow(x, 2)/(2*pow(tau, 2)));

    return log(lik);
  }

  // 5 parameter logistic function
  real f5PL(real x, real a, real b, real c, real d, real g) {
    real y = d + (a - d) / pow(1 + pow(x/c, b), g);
    return y;
  }

  // 4 parameter logistic function
  real f4PL(real x, real a, real b, real c, real d) {
    real y = a + (d - a) * inv_logit(b*(x-c));
    return y;
  }

}

