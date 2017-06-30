EstimateMAMBAC <-
function(curve) {
  # 
  # Estimates taxon base rate for a MAMBAC curve.
  #
  # Args:
  #   curve: MAMBAC curve (vector).
  #
  # Returns:
  #   Base rate estimate (scalar).
  # 
  p.est.valid <- F
  loc.l <- 1
  loc.r <- length(curve)
  while (!p.est.valid) {
    if (curve[loc.l] == 0) {
      loc.l <- loc.l + 1
      loc.r <- loc.r - 1
    } else { 
      p.est.valid <- T
    }
  }
  p.estimate <- 1 / (curve[loc.r] / curve[loc.l] + 1)
  if (p.estimate < 0) {
    p.estimate <- 0
  }
  if (p.estimate > 1) {
    p.estimate <- 1
  }
  return(p.estimate)
}
