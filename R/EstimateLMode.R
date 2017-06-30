EstimateLMode <-
function(curve.x, curve.y, parameters) {
  # 
  # Estimates taxon base rate for L-Mode curve.
  #
  # Args:
  #      curve.x: X values of density (vector).
  #      curve.y: Y values of density (vector).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Base rate estimates (list of three estimates).
  # 
  max.l <- max(smooth(curve.y)[curve.x <= parameters$mode.l])
  loc.l <- (1:length(curve.x))[smooth(curve.y) == max.l]
  if (length(loc.l) > 1) {
    loc.l <- loc.l[1]
  }
  max.r <- max(smooth(curve.y)[curve.x >= parameters$mode.r])
  loc.r <- (1:length(curve.x))[smooth(curve.y) == max.r]
  if (length(loc.r) > 1) {
    loc.r <- loc.r[1]
  }
  p.r <- 1 / (1 + curve.x[loc.r] ^ 2)
  p.l <- 1 - (1 / (1 + curve.x[loc.l] ^ 2))
  p.estimate <- mean(c(p.l, p.r))
  return(list(p.r = p.r, p.l = p.l, p.estimate = p.estimate))
}
