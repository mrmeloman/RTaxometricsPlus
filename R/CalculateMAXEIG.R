CalculateMAXEIG <-
function(input, outputs, parameters) {
  # 
  # Calculates one MAXEIG curve.
  #
  # Args:
  #        input: Input indicator (vector).
  #      outputs: Output indicators (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   One MAXEIG curve (list, vectors of x and y values).
  # 
  n <- length(input)
  k <- dim(outputs)[2]  # number of outputs
  x <- cbind(input, outputs)
  x <- x[sort.list(x[, 1]), ]
  n.win <- n / (parameters$windows * (1 - parameters$overlap) + 
                parameters$overlap)
  n.overlap <- n.win * parameters$overlap
  cuts <- matrix(nrow = 2, ncol = parameters$windows)
  cuts[1, ] <- round(seq(from = 1, by = (n.win - n.overlap), length = 
                     parameters$windows))
  cuts[2, ] <- round(seq(from = n.win, to = n, by = (n.win - n.overlap)))
  curve.x <- rep(0, parameters$windows)
  curve.y <- rep(0, parameters$windows)
  for (i in 1:parameters$reps) {
    if (i > 1) {
      x <- x[sample(1:n, n, replace = FALSE), ]
      x <- x[sort.list(x[, 1]), ]
    }
    for (j in 1:parameters$windows) {
      curve.x[j] <- curve.x[j] + mean(x[cuts[1, j]:cuts[2, j], 1])
      cov.matrix <- var(x[cuts[1, j]:cuts[2, j], 2:(k + 1)])
      data.no.diag <- cov.matrix - diag(diag(cov.matrix))
      curve.y[j] <- curve.y[j] + eigen(data.no.diag)$values[1]
    }
  }
  curve.x <- curve.x / parameters$reps
  curve.y <- curve.y / parameters$reps
  return(list(curve.x = curve.x, curve.y = curve.y))
}
