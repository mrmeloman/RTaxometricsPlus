print.profile <-
function(x) {
  # 
  # Prints an object of class "profile".
  # 
  cat("TAXOMETRIC ANALYSIS RESULTS\n\n")
  cat("Summary of shared analytic specifications\n")
  cat("  sample size: ", x$n, "\n")
  cat("  number of variables: ", x$n.variables, "\n")
  cat("  comparison data population size: ", x$comp.data.n.pop, "\n")
  cat("  comparison data samples: ", x$comp.data.n.samples, "\n")
  cat("  replications: ", x$replications, "\n\n")
  if (!is.null(x$MAMBAC.indicators)) {
    cat("Summary of MAMBAC analytic specifications\n")
    cat("  cuts: ", x$MAMBAC.n.cuts, "evenly-spaced cuts beginning", 
        x$MAMBAC.n.end, "cases from either extreme\n")
    cat("  indicators: ", x$MAMBAC.indicators, "\n")
    cat("  number of curves: ", x$MAMBAC.n.curves, "\n\n")
  }
  if (!is.null(x$MAXEIG.indicators)) {
    cat("Summary of MAXEIG analytic specifications\n")
    cat("  subsamples: ", x$MAXEIG.n.windows, "windows that overlap", 
        x$MAXEIG.overlap, "\n")
    cat("  indicators: ", x$MAXEIG.indicators, "\n")
    cat("  number of curves: ", x$MAXEIG.n.curves, "\n\n")
  }
  if (!is.null(x$LMode.search.left)) {
    cat("Summary of L-Mode analytic specifications\n")
    cat("  position beyond which to search for left mode: ", 
        x$Lmode.search.left, "\n")
    cat("  position beyond which to search for right mode: ", 
        x$Lmode.search.right, "\n\n")
  }
  cat("  Note: Users should run the CheckData() function to evaluate whether", 
      "\n        data appear to be adequate for taxometric analysis.\n\n")
  cat("Aggregate Comparison Curve Fit Index (CCFI)\n")
  if (!is.null(x$CCFI.mean)) {
      cat("  mean profile: ", x$CCFI.mean, "\n")
  }
  if (!is.null(x$CCFI.MAMBAC)) {
      cat("  MAMBAC profile: ", x$CCFI.MAMBAC, "\n")
  }
  if (!is.null(x$CCFI.MAXEIG)) {
      cat("  MAXEIG profile: ", x$CCFI.MAXEIG, "\n")
  }
  if (!is.null(x$CCFI.LMode)) {
      cat("  LMode profile: ", x$CCFI.LMode, "\n")
  }
  if (!is.null(x$CCFI.MAXSLOPE)) {
      cat("  MAXSLOPE profile: ", x$CCFI.MAXSLOPE, "\n")
  }
  cat("\n  Note: CCFI values can range from 0 (dimensional) to 1 (categorical).", 
      "\n        The further a CCFI is from .50, the stronger the result.",
      "\n        Aggregate CCFI values are a weighted mean of all CCFI values",
      "\n        in the profile.\n\n")
  cat("Base Rate Estimates\n")
  if (!is.null(x$p.mean)) {
      cat("  mean profile: ", x$p.mean, "\n")
  }
  if (!is.null(x$p.MAMBAC)) {
      cat("  MAMBAC profile: ", x$p.MAMBAC, "\n")
  }
  if (!is.null(x$p.MAXEIG)) {
      cat("  MAXEIG profile: ", x$p.MAXEIG, "\n")
  }
  if (!is.null(x$p.LMode)) {
      cat("  LMode profile: ", x$p.LMode, "\n")
  }
  if (!is.null(x$p.MAXSLOPE)) {
      cat("  MAXSLOPE profile: ", x$p.MAXSLOPE, "\n")
  }
  cat("\n  Note: There is no evidence-based way to use base rate estimates to", 
      "\n        differentiate categorical and dimensional data. They should", 
      "\n        only be used if evidence supports categorical structure.\n")
}
