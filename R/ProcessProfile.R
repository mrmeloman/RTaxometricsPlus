ProcessProfile <-
function(CCFIs, parameters) {
  # 
  # Calculates aggregated CCFI and base rate estimate for one CCFI profile.
  #
  # Args:
  #        CCFIs: CCFI values across base rates for a single procedure (vector).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Aggregated CCFI + base rate estimate (list).
  #
  n <- parameters$num.p
  ps <- seq(parameters$min.p, parameters$max.p, length = n)
  power <- 6
  data <- matrix(n * (power + 1), nrow = n, ncol = power + 1)
  data[, 1] <- CCFIs
  for (i in 2:(power + 1)) {
    data[, i] <- ps ^ (i - 1)
  }
  lin.model = lm(data[, 1] ~ data[, 2:(power + 1)])
  xs <- seq(min(ps), max(ps), .001)
  ys <- rep(lin.model$coefficients[1], length(xs))
  for (i in 1:power) {
    ys <- ys + xs ^ i * lin.model$coefficients[i + 1]
  }
  p.est <- mean(xs[ys == max(ys)])
  weights <- (ps / p.est) * (p.est > ps) + 
             (1 - ps) / (1 - p.est) * (p.est <= ps)
  CCFI <- sum(weights * CCFIs) / sum(weights)
  return(list(CCFI = CCFI, p.est = p.est))
}
