EstimateMAXSLOPE <-
function(curve.x, curve.y) {
  # 
  # Estimates taxon base rate for a MAXSLOPE curve.
  #
  # Args:
  #   curve.x: X values of curve (vector).
  #   curve.y: Y values of curve (vector).  
  #
  # Returns:
  #   Base rate estimate (scalar).
  # 
  n <- length(curve.y)
  my <- max(curve.y)
  my.x <- max(curve.x[curve.y == my])
  my.n <- sum(curve.x >= my.x) - .5 * (sum(curve.y == my) - 1)
  p.estimate <- my.n / n
  return(p.estimate)
}
