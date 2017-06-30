RunMAXEIG <-
function(x, parameters) {
  # 
  # Performs MAXEIG analysis.
  #
  # Args:
  #            x: Data (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Panel of MAXEIG curves (list, matrices of x and y values).
  # 
  k <- parameters$k
  curve.info <- AssignMAXEIG(parameters)
  n.curves <- dim(curve.info)[1]
  if (parameters$assign.MAXEIG == 1) {
    n.curves <- (k * (k - 1) * (k - 2)) / 2
  }
  if (parameters$assign.MAXEIG == 2) {
    n.curves <- k
  }
  if (parameters$assign.MAXEIG == 3) {
    n.curves <- k * (k - 1) / 2
  }
  curve.x <- matrix(0, nrow = n.curves, ncol = parameters$windows)
  curve.y <- matrix(0, nrow = n.curves, ncol = parameters$windows)
  for (i in 1:n.curves) {
    if (parameters$assign.MAXEIG == 1) {
      input <- x[, curve.info[i, 1]]
      outputs <- x[, curve.info[i, 2:3]]
    } 
    if (parameters$assign.MAXEIG == 2) {
      input <- x[, curve.info[i, 1]]
      outputs <- x[, -curve.info[i, 1]]
    }
    if (parameters$assign.MAXEIG == 3) {
      outputs <- x[, curve.info[i, 1:2]]
      input <- apply(x, 1, sum) - apply(outputs, 1, sum)
    }
    curve <- CalculateMAXEIG(input, outputs, parameters)
    curve.x[i, ] <- curve$curve.x
    curve.y[i, ] <- curve$curve.y
  }
  return(list(curve.x = curve.x, curve.y = curve.y))
}
