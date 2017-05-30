ClassifyCases <-
function(x, p, cols = 0) {
  # 
  # Assigns cases to groups using a specified base rate.
  #
  # Args:
  #      x: Data (matrix).
  #      p: Base rate; proportion of cases in higher-scoring group (scalar).
  #   cols: Column numbers that contain variables to sum (vector).
  #
  # Returns:
  #   Sample of data plus new classification variable (matrix).
  # 
  n <- dim(x)[1]
  if (cols[1] == 0) cols <- 1:dim(x)[2]
  sums <- apply(x[, cols], 1, sum)
  y <- cbind(x, rep(0, n), 1:n, sums)
  n.t <- round(p * n)
  n.c <- n - n.t
  class <- c(rep(1, n.c), rep(2, n.t))
  last.col <- dim(y)[2]
  y <- y[sort.list(y[, last.col]), ]
  y[, last.col - 2] <- class
  if (max(y[(y[, last.col - 2] == 1), last.col]) == 
      min(y[(y[,last.col - 2] == 2), last.col])) {
    tied.score <- max(y[(y[, last.col - 2] == 1), last.col])
    tied.cases <- (y[, last.col] == tied.score)
    n.tied.1 <- sum((y[, last.col - 2] == 1) & (tied.cases))
    n.tied.2 <- sum((y[, last.col - 2] == 2) & (tied.cases))
    tied.min <- tied.score == min(y[, last.col])
    tied.max <- tied.score == max(y[, last.col])
    if (n.tied.1 > n.tied.2) {
      if (!tied.max) {
        y[tied.cases, last.col - 2] <- 1
      } else {
        y[tied.cases, last.col - 2] <- 2
      }
    } else {
      if (!tied.min) {
        y[tied.cases, last.col - 2] <- 2
      } else {
        y[tied.cases, last.col - 2] <- 1
      }
    }
  }
  y <- y[sort.list(y[, last.col - 1]), ]
  return(cbind(x, y[, last.col - 2]))
}
