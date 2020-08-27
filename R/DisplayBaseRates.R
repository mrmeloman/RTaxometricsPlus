DisplayBaseRates <-
function(x.results, parameters) {
  # 
  # Calculates and reports base rate estimates for taxometric analysis.
  #
  # Args:
  #    x.results: Empirical data results (list).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Nothing; text output only.
  #
  sum.p <- 0
  n.p <- 0
  MAMBAC.p <- 0
  MAXEIG.p <- 0
  LMode.p <- 0
  MAXSLOPE.p <- 0
  if (parameters$MAMBAC) {
    MAMBAC.p <- EstimateMAMBAC(x.results$MAMBAC)
    sum.p <- sum.p + MAMBAC.p
    n.p <- n.p + 1  
  }
  if (parameters$MAXEIG) {
    MAXEIG.p <- EstimateMAXEIG(x.results$MAXEIG.y)
    sum.p <- sum.p + MAXEIG.p
    n.p <- n.p + 1 
  }
  if (parameters$LMode) {
    LMode.p <- EstimateLMode(x.results$LMode.x, x.results$LMode.y, parameters)
    sum.p <- sum.p + LMode.p$p.estimate
    n.p <- n.p + 1 
  }
  if (parameters$MAXSLOPE) {
    MAXSLOPE.p <- EstimateMAXSLOPE(x.results$MAXSLOPE.x, x.results$MAXSLOPE.y)
    sum.p <- sum.p + MAXSLOPE.p
    n.p <- n.p + 1 
  }
  mean.p <- sum.p / n.p
  cat("Base Rate Estimates:\n")
  if (parameters$MAMBAC) {
    cat("  MAMBAC: ", round(MAMBAC.p, 3), "\n")  
  }
  if (parameters$MAXEIG) {
    cat("  MAXEIG: ", round(MAXEIG.p, 3), "\n")
  }
  if (parameters$LMode) {
    cat("  L-Mode:\n")
    cat("    based on location of left mode: ", round(LMode.p$p.l, 3), "\n")
    cat("    based on location of right mode: ", round(LMode.p$p.r, 3), "\n")
    cat("    mean: ", round(LMode.p$p.estimate, 3), "\n")
  }
  if (parameters$MAXSLOPE) {
    cat("  MAXSLOPE: ", round(MAXSLOPE.p, 3), "\n")
  }
  if (n.p > 1) {
   cat("  mean: ", round(mean.p,3), "\n\n")
  } else {
    cat("\n")
  }
  cat("  Note: There is no evidence-based way to use base rate estimates to", 
      "\n        differentiate categorical and dimensional data. They should", 
      "\n        only be used if evidence supports categorical structure.\n")
}
