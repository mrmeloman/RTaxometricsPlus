RemoveMissingData <-
function(x) {
  # 
  # Listwise deletion of missing data.
  #
  # Args:
  #   x: Data (matrix).
  #
  # Returns:
  #   Data after listwise deletion of missing data (matrix).
  # 
  n <- dim(x)[1]
  k <- dim(x)[2]
  complete <- rep(T, n)
  for (i in 1:n)
    for (j in 1:k)
      if (is.na(x[i, j])) {
        complete[i] <- F
      }
  x.k <- x[complete, ]
  n.k <- dim(x.k)[1]
  if (n.k < n) {
    cat("  * ", n - n.k, " cases (", round(100 * (n - n.k) / n, 1), 
        "%) contained missing data;\n    ", n.k, " out of ", n, " cases (", 
        round(100 * n.k / n, 1), "%) retained for analysis\n", sep = "")
  }
  return(x.k)
}
