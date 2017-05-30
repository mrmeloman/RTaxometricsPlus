AssignMAXEIG <-
function(parameters) {
  # 
  # Assigns variables to input/output configurations for MAXEIG analysis.
  #
  # Args:
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Input/output variables per curve (matrix).
  # 
  k <- parameters$k
  if (parameters$assign.MAXEIG == 1) {  # input-output triplets
    if (k == 3) {
      n.curves <- 3
    } else {
      n.curves <- (k * (k - 1) * (k - 2)) / 2
    }
    curve.info <- matrix(nrow = n.curves, ncol = 3)
    curve.info[, 1] <- 1:n.curves
    counter <- 1
    for (triplet.a in 1:k)
    for (triplet.b in 1:k)
    for (triplet.c in 1:k)
    if ((triplet.a != triplet.b) & (triplet.a != triplet.c) 
       & (triplet.b < triplet.c)) {
       curve.info[counter, 1] <- triplet.a
       curve.info[counter, 2] <- triplet.b
       curve.info[counter, 3] <- triplet.c
       counter <- counter + 1
    }
  }
  if (parameters$assign.MAXEIG == 2) {  # each serves as input once
    n.curves <- k
    curve.info <- matrix(nrow = n.curves, ncol = 1)
    curve.info[, 1] <- 1:n.curves
  }    
  if (parameters$assign.MAXEIG == 3) {  # summed inputs
    n.curves <- k * (k - 1) / 2
    curve.info <- matrix(nrow = n.curves, ncol = 2)
    counter <- 0
    for (i in 1:(k - 1))
      for (j in (i + 1):k) {
        curve.info[counter + 1, 1:2] <- c(i, j)
        counter <- counter + 1
      }
  }    
  return(curve.info)
}
