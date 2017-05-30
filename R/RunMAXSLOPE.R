RunMAXSLOPE <-
function(x) {
  # 
  # Performs MAXSLOPE analysis.
  #
  # Args:
  #   x: Data (matrix).
  #
  # Returns:
  #   Panel of MAXSLOPE curves (list, matrices of x and y values).
  # 
  curve.1 <- CalculateMAXSLOPE(x, curve = 1)
  curve.2 <- CalculateMAXSLOPE(x, curve = 2)
  curve.x <- rbind(curve.1$curve.x, curve.2$curve.x)
  curve.y <- rbind(curve.1$curve.y, curve.2$curve.y)
  return(list(curve.x = curve.x, curve.y = curve.y))
}
