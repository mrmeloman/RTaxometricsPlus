CalculateFitDensities <-
function(shift, data) {
  # 
  # Calculate fit for L-Mode curves.
  #
  # Args:
  #   shift: Horizontal shift (scalar).
  #    data: Curves for empirical and comparison data (list).
  #
  # Returns:
  #    Fit value (scalar).
  # 
  n <- length(data$curve.x)
  w <- (max(data$curve.y) - min(data$curve.y)) / (max(data$curve.x) - 
       min(data$curve.x))
  data$curve.x <- data$curve.x * w
  data$curve.comp.x <- data$curve.comp.x * w
  distance <- rep(0, n)
  for (i in 1:n)
    distance[i] <- min(sqrt((shift + data$curve.x[i] - data$curve.comp.x) ^ 2 + 
               (data$curve.y[i] - data$curve.comp.y) ^ 2))
  fit <- sqrt(sum(distance ^ 2) / n)
  return(fit)
}
