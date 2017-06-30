RunMAMBAC <-
function(x, parameters) {
  # 
  # Performs MAMBAC analysis.
  #
  # Args:
  #            x: Data (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Panel of MAMBAC curves (matrix).
  # 
  k <- parameters$k
  curve.info <- AssignMAMBAC(parameters)
  n.curves <- dim(curve.info)[1]
  if (parameters$assign.MAMBAC == 1) {
    n.curves <- k * (k - 1)
  } else {
    n.curves <- k
  }
  curves <- matrix(0, nrow = n.curves, ncol = parameters$n.cuts)
  for (i in 1:n.curves)
  {
    if (parameters$assign.MAMBAC == 1) {
    	  input <- x[, curve.info[i, 1]]
    	  output <- x[, curve.info[i, 2]]
    } else {
    	  output <- x[, curve.info[i, 2]]
    	  input <- apply(x, 1, sum) - output
    }
    curves[i, ] <- CalculateMAMBAC(input, output, parameters)
  }
  return(curves)
}
