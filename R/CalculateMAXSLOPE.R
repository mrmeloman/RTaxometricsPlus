CalculateMAXSLOPE <-
function(x, curve) {
  # 
  # Calculates one MAXSLOPE curve.
  #
  # Args:
  #       x: Data (matrix).
  #   curve: Curve number (scalar).
  #
  # Returns:
  #   One MAXSLOPE curve (list, vectors of x and y values).
  # 
  n <- dim(x)[1]
  if (curve == 2) {
    t <- x[, 1]
    x[, 1] <- x[, 2]
    x[, 2] <- t
  }
  x <- x[sort.list(x[, 1]), ]
  smooth <- lowess(x[, 2] ~ x[, 1])
  slopes <- (smooth$y[2:n] - smooth$y[1:(n - 1)]) /
    (smooth$x[2:n] - smooth$x[1:(n - 1)])
  while(sum(is.nan(slopes)) > 0) {
    for (i in 2:(n - 1)) {
      if (is.nan(slopes[i]) & !is.nan(slopes[i - 1])) {
        slopes[i] <- slopes[i - 1]
      }
    }
    for (i in 1:(n - 2)) {
      if (is.nan(slopes[i]) & !is.nan(slopes[i + 1])) {
        slopes[i] <- slopes[i + 1]
      }
    }
  }
  means <- (x[2:n, 1] + x[1:(n - 1), 1]) / 2
  return(list(curve.x = means, curve.y = slopes))
}
