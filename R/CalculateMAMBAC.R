CalculateMAMBAC <-
function(input, output, parameters) {
  # 
  # Calculates one MAMBAC curve.
  #
  # Args:
  #        input: Input indicator (scalar).
  #       output: Output indicator (scalar).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   One MAMBAC curve (vector).
  # 
  n <- length(input)
  x <- cbind(input, output)
  x <- x[sort.list(x[, 1]), ]
  cuts <- seq(from = parameters$n.end, to = (n - parameters$n.end), 
              length = parameters$n.cuts)
  curve <- rep(0, parameters$n.cuts)
  for (i in 1:parameters$reps) {
    if (i > 1) {
      x <- x[sample(1:n, n, replace = FALSE), ]
      x <- x[sort.list(x[, 1]), ]
    }
    for (j in 1:parameters$n.cuts)
      curve[j] <- curve[j] + mean(x[(cuts[j]+1):n, 2]) - mean(x[1:cuts[j], 2])
  }
  curve <- curve / parameters$reps
  return(curve)
}
