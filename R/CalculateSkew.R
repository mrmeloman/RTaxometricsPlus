CalculateSkew <-
function(x) {
  # 
  # Computes the sample skewness of a distribution.
  #
  # Args:
  #   x: Data (vector).
  #
  # Returns:
  #   The sample skewness of x (scalar).
  # 
  n <- length(x)
  m1 <- mean(x)
  m2 <- sum((x - m1) ^ 2) / n
  m3 <- sum((x - m1) ^ 3) / n
  g1 <- m3 / (m2 ^ (3/2))
  g1 <- (sqrt(n * (n - 1)) / (n - 2)) * g1
  return(g1)
}
