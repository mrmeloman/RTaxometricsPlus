SummarizeDist <-
function(x) {
  # 
  # Computes the sample mean, standard deviation, skewness, and kurtosis.
  #
  # Args:
  #   x: Data (vector).
  #
  # Returns:
  #   The sample mean, standard deviation, skewness, and kurtosis of x (vector).
  # 
  mean = mean(x)
  sd = sd(x)
  skewness = CalculateSkew(x)
  kurtosis = CalculateKurtosis(x)
  return(c(mean, sd, skewness, kurtosis))
}
