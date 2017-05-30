CalculateLModeCCFI <-
function(curve.x, curve.y, curve.dim.x, curve.dim.y,
                               curve.cat.x, curve.cat.y) {
  # 
  # Calculates CCFI for an LMode curve.
  #
  # Args:
  #      curve.x: Empirical data curve, x (vector).
  #      curve.y: Empirical data curve, y (vector).
  #  curve.dim.x: Average curve for dimensional comparison data, x (vector).
  #  curve.dim.y: Average curve for dimensional comparison data, y (vector).
  #  curve.cat.x: Average curve for categorical comparison data, x (vector).
  #  curve.cat.y: Average curve for categorical comparison data, y (vector).
  #
  # Returns:
  #   CCFI value (scalar).
  # 
  data <- list(curve.x = curve.x, curve.y = curve.y, 
               curve.comp.x = curve.dim.x, curve.comp.y = curve.dim.y)
  RMSR.d <- optimize(f = CalculateFitDensities, interval = c(-1, 1), 
                         data)$objective
  data <- list(curve.x = curve.x, curve.y = curve.y, 
               curve.comp.x = curve.cat.x, curve.comp.y = curve.cat.y)
  RMSR.c <- optimize(f = CalculateFitDensities, interval = c(-1, 1), 
                         data)$objective
  CCFI <- RMSR.d / (RMSR.d + RMSR.c)
  return(CCFI)
}
