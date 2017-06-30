EstimateMAXEIG <-
function(curve) {
  # 
  # Estimates taxon base rate for a MAXEIG curve.
  #
  # Args:
  #   curve: MAXEIG curve (vector).
  #
  # Returns:
  #   Base rate estimate (scalar).
  # 
  win <- length(curve)
  hitmax.value <- max(curve)
  hitmax.locs <- (1:win)[curve == hitmax.value]
  hitmax.loc <- round(median(hitmax.locs))
  p <- rep(0, win)
  n <- rep(0, win)
  k <- 4 * hitmax.value
  for (i in 1:win) {
    if ((k ^ 2 - 4 * k * curve[i]) >= 0) {
      quad1 <- (k + sqrt(k ^ 2 - 4 * k * curve[i])) / (2 * k)
    } else {
      quad1 <- 1
    }
    if ((k ^ 2 - 4 * k * curve[i]) >= 0) {
      quad2 <- (k - sqrt(k ^ 2 - 4 * k * curve[i])) / (2 * k)
    } else {
      quad2 <- 0
    }
    if (i < hitmax.loc) {
      p[i] <- min(1, quad2)
    }
    if (i == hitmax.loc) {
      p[i] <- .5
    }
    if (i > hitmax.loc) {
    p[i] <- max(quad1, 0)
    }
    if ((curve[i] < 0) & (i < hitmax.loc)) {
    p[i] <- 0
    }
    if ((curve[i] < 0) & (i > hitmax.loc)) {
      p[i] <- 1
    }
    n[i] <- curve[i]
  }
  p.estimate <- sum(p * n) / sum(n)
  return(p.estimate)
}
