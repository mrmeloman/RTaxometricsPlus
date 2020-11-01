CalculateProfileOutput <-
function(CCFIs, parameters) {
  # 
  # Provides aggregated CCFIs and base rate estimates for CCFI profile.
  #
  # Args:
  #        CCFIs: CCFI values across base rates and procedures (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns list object containing CCFIs and base rate estimates.
  #
  CCFI.aggregates <- rep(0, 5)
  p.ests <- rep(0, 5)
  for (i in 1:5) {
    profile.results <- ProcessProfile(CCFIs[i, ], parameters)
    CCFI.aggregates[i] <- profile.results$CCFI
    p.ests[i] <- profile.results$p.est
  }
  output <- list(CCFI.mean = CCFI.aggregates[5])
  if (parameters$MAMBAC) {
    output <- c(output, CCFI.MAMBAC = CCFI.aggregates[1])
  }
  if (parameters$MAXEIG) {
    output <- c(output, CCFI.MAXEIG = CCFI.aggregates[2])
  }
  if (parameters$LMode) {
    output <- c(output, CCFI.LMode = CCFI.aggregates[3])
  }
  if (parameters$MAXSLOPE) {
    output <- c(output, CCFI.MAXSLOPE = CCFI.aggregates[4])
  }
  output <- c(output, p.mean = p.ests[5])
  if (parameters$MAMBAC) {
    output <- c(output, p.MAMBAC = p.ests[1])
  }
  if (parameters$MAXEIG) {
    output <- c(output, p.MAXEIG = p.ests[2])
  }
  if (parameters$LMode) {
    output <- c(output, p.LMode = p.ests[3])
  }
  if (parameters$MAXSLOPE) {
    output <- c(output, p.MAXSLOPE = p.ests[4])
  }
  return(output)
}
