DisplayTextOutput <-
function(CCFIs, parameters) {
  # 
  # Displays aggregated CCFIs and base rate estimates for CCFI profile.
  #
  # Args:
  #        CCFIs: CCFI values across base rates and procedures (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Aggregated CCFI values.
  #
  CCFI.aggregates <- rep(0, 5)
  p.ests <- rep(0, 5)
  for (i in 1:5) {
    profile.results <- ProcessProfile(CCFIs[i, ], parameters)
    CCFI.aggregates[i] <- profile.results$CCFI
    p.ests[i] <- profile.results$p.est
  }
  cat("Aggregate Comparison Curve Fit Index (CCFI)\n")
  cat("  mean profile: ", round(CCFI.aggregates[5], 3), "\n")
  if (parameters$MAMBAC) {
    cat("  MAMBAC profile: ", round(CCFI.aggregates[1], 3), "\n")
  }
  if (parameters$MAXEIG) {
    cat("  MAXEIG profile: ", round(CCFI.aggregates[2], 3), "\n")
  }
  if (parameters$LMode) {
    cat("  L-Mode profile: ", round(CCFI.aggregates[3], 3), "\n")
  }
  if (parameters$MAXSLOPE) {
    cat("  MAXSLOPE profile: ", round(CCFI.aggregates[4], 3), "\n")
  }
  cat("\n")
  cat("  Note: CCFI values can range from 0 (dimensional) to 1 (categorical).",
      "\n        The further a CCFI is from .50, the stronger the result.",
      "\n        Aggregate CCFI values are a weighted mean of all CCFI values",
      "\n        in the profile.\n\n")
  cat("Base Rate Estimates\n")
  cat("  mean profile: ", round(p.ests[5], 3), "\n")
  if (parameters$MAMBAC) {
    cat("  MAMBAC profile: ", round(p.ests[1], 3), "\n")
  }
  if (parameters$MAXEIG) {
    cat("  MAXEIG profile: ", round(p.ests[2], 3), "\n")
  }
  if (parameters$LMode) {
    cat("  L-Mode profile: ", round(p.ests[3], 3), "\n")
  }
  if (parameters$MAXSLOPE) {
    cat("  MAXSLOPE profile: ", round(p.ests[4], 3), "\n")
  }
  cat("\n")
  cat("  Note: There is no evidence-based way to use base rate estimates to", 
      "\n        differentiate categorical and dimensional data. They should", 
      "\n        only be used if evidence supports categorical structure.\n")
  
  aggCCFI <- data.frame(t(round(CCFI.aggregates, 3)))
  colnames(aggCCFI) <- c("aggCCFI_MAMBAC", "aggCCFI_MAXEIG", "aggCCFI_LMode", "aggCCFI_MAXSLOPE", "aggCCFI_Mean")
  
  return(aggCCFI)
}
