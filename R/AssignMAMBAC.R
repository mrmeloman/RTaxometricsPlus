AssignMAMBAC <-
function(parameters) {
  # 
  # Assigns variables to input/output configurations for MAMBAC analysis.
  #
  # Args:
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Input/output variables per curve (matrix).
  # 
  k <- parameters$k
  if (parameters$assign.MAMBAC == 1) {
    n.curves <- k * (k - 1)
    curve.info <- matrix(nrow = n.curves, ncol = 2)
    counter <- 0
    for (i in 1:(k - 1))
      for (j in (i + 1):k) {
        curve.info[counter + 1, 1:2] <- c(i, j)
        curve.info[counter + 2, 1:2] <- c(j, i)
        counter <- counter + 2
      }
  } else {
    curve.info <- matrix(0, nrow = k, ncol = 2)
    curve.info[, 1] <- 1:k
    curve.info[, 2] <- 1:k
  }
  return(curve.info)
}
