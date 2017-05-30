CalculateCCFI <-
function(curve, curve.dim, curve.cat) {
  # 
  # Calculates CCFI for a MAMBAC, MAXEIG, or MAXSLOPE curve.
  #
  # Args:
  #      curve: Empirical data curve (vector).
  #  curve.dim: Average curve for dimensional comparison data (vector).
  #  curve.cat: Average curve for categorical comparison data (vector).
  #
  # Returns:
  #   CCFI value (scalar).
  # 
  points <- length(curve)
  RMSR.d <- sqrt(sum((curve - curve.dim) ^ 2) / points)
  RMSR.c <- sqrt(sum((curve - curve.cat) ^ 2) / points)
  CCFI <- RMSR.d / (RMSR.d + RMSR.c)
  return(CCFI)
}
