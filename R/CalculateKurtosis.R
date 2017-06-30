CalculateKurtosis <-
function(x) {
  # 
  # Computes the sample kurtosis of a distribution.
  #
  # Args:
  #   x: Data (vector).
  #
  # Returns:
  #   The sample kurtosis of x (scalar).
  # 
  n <- length(x)
  m1 <- mean(x)
  m2 <- sum((x - m1) ^ 2) / n
  m3 <- sum((x - m1) ^ 3) / n
  m4 <- sum((x - m1) ^ 4) / n
  g2 <- (m4 / (m2 ^ 2)) - 3
  g2 <- ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * g2 + 6)
  return(g2)
}
