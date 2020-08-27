DisplayCCFIs <-
function(x.results, x.dim.results, x.cat.results, parameters) {
  # 
  # Calculates and reports CCFI results for taxometric analysis.
  #
  # Args:
  #       x.results: Empirical data results (list).
  #   x.dim.results: Dimensional comparison data results (list).
  #   x.cat.results: Categorical comparison data results (list).
  #      parameters: Data and program parameters (list).
  #
  # Returns:
  #   CCFIs.
  #
  CCFIs <- CalculateCCFIs(x.results, x.dim.results, x.cat.results, parameters)
  cat("Comparison Curve Fit Index (CCFI)\n")
  if (parameters$MAMBAC) {
    cat("  MAMBAC: ", round((CCFIs)[1], 3), "\n")
  } 
  if (parameters$MAXEIG) {
    cat("  MAXEIG: ", round((CCFIs)[2], 3), "\n")
  } 
  if (parameters$LMode) {
    cat("  L-Mode: ", round((CCFIs)[3], 3), "\n")
  }
  if (parameters$MAXSLOPE) {
    cat("  MAXSLOPE: ", round((CCFIs)[4], 3), "\n")
  }
  if ((CCFIs)[5] > 0) {
    cat("  mean: ", round((CCFIs)[5],3), "\n\n")
  } else {
    cat("\n")
  }
  cat("  Note: CCFI values can range from 0 (dimensional) to 1 (categorical).",
      "\n        The further a CCFI is from .50, the stronger the result.\n\n")
  
  CCFIdf <- data.frame(t(round(CCFIs, 3)))
  colnames(CCFIdf) <- c("CCFI_MAMBAC", "CCFI_MAXEIG", "CCFI_LMode", "CCFI_MAXSLOPE", "CCFI_Mean")
  
  return(CCFIdf)
}
