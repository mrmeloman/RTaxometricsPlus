AddVariance <-
function(x, k, parameters) {
  # 
  # Adds variance if necessary.
  #
  # Args:
  #            x: Data (matrix).
  #            k: Number of variables (scalar).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Data with ncessary variance added (matrix).
  # 
  for (i in 1:k) {
    if (var(x[, i]) == 0) {
    	  x[, i] <- x[, i] + rnorm(parameters$n, 0, .0001)
    	  cat("  * variance added to variable", i, "\n")
    }
  }
  return(x)
}
