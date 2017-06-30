RunLMode <-
function(x) {
  # 
  # Performs L-Mode analysis.
  #
  # Args:
  #   x: Data (matrix).
  #
  # Returns:
  #   LMode curve (factor score density; list, vectors of x and y values).
  # 
  n <- dim(x)[1]
  factoring <- RunFactorAnalysis(x, n.factors = 1)
  scores <- apply(x %*% factoring$loadings %*% solve(t(factoring$loadings) %*% 
    factoring$loadings), 2, scale)
  density.score <- density(scores)
  curve.x <- density.score$x
  curve.y <- density.score$y
  return(list(curve.x = curve.x, curve.y = curve.y))
}
