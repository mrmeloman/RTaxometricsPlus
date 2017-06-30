CalculateValidity <-
function(x.1, x.2) {
  # 
  # Computes the standardized mean difference between two groups (Cohen's d).
  #
  # Args:
  #   x.1: Data for the first group (vector).
  #   x.2: Data for the second group (vector).
  #
  # Returns:
  #   The standardized mean difference between groups (scalar).
  # 
  df.1 <- length(x.1) - 1
  df.2 <- length(x.2) - 1
  d <- (mean(x.1) - mean(x.2)) / sqrt((var(x.1) * df.1 + var(x.2) * df.2) 
       / (df.1 + df.2))
  return(d)
}
