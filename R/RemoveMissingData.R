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
  y <- na.omit(x)
  n.y <- dim(y)[1]
  if (n.y < n) {
    cat("  * ", n - n.y, " cases (", round(100 * (n - n.y) / n, 1), 
        "%) contained missing data;\n    ", n.y, " out of ", n, " cases (", 
        round(100 * n.y / n, 1), "%) retained for analysis\n", sep = "")
  }
  return(y)
}
