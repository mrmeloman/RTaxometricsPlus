DisplaySpecifications <-
function(parameters) {
  # 
  # Provides text output for analytic specifications.
  #
  # Args:
  #    parameters: Data and program parameters (list).
  #
  # Returns:
  #   Nothing; text output only.
  # 
  cat("\n  Note: Users should run the CheckData() function to evaluate whether", 
      "\n        data appear to be adequate for taxometric analysis.\n\n")
  cat("TAXOMETRIC ANALYSIS RESULTS\n\n")
  cat("Summary of shared analytic specifications\n")
  cat("  sample size: ", parameters$n, "\n")
  cat("  number of variables: ", parameters$k, "\n")
  cat("  comparison data population size: ", parameters$n.pop, "\n")
  cat("  comparison data samples: ", parameters$n.samples, "\n")
  if (!parameters$profile) {
    cat("  comparison data taxon base rate: ", round(parameters$p, 3), "\n")
  }
  cat("  replications: ", parameters$reps, "\n\n")
  if (parameters$MAMBAC) {
    cat("Summary of MAMBAC analytic specifications\n")
    cat("  cuts: ", parameters$n.cuts, "evenly-spaced cuts beginning", 
      parameters$n.end, "cases from either extreme\n")
    if (parameters$assign.MAMBAC == 1) {
      ind.label <- "all possible input-output pairs"
    } else { 
      ind.label <- "output = one variable, input = sum of remaining variables"
    }
    cat("  indicators: ", ind.label, "\n")
    curve.info <- AssignMAMBAC(parameters)
    n.curves <- dim(curve.info)[1]
    cat("  number of curves: ", n.curves, "\n\n")
  }
  if (parameters$MAXEIG) {
    cat("Summary of MAXEIG analytic specifications\n")
    cat("  subsamples: ", parameters$windows, "windows that overlap", 
        parameters$overlap, "\n")
    if (parameters$assign.MAXEIG == 1) {
      ind.label <- "input = one variable, output = all other variables"
    }
    if (parameters$assign.MAXEIG == 2) {
      ind.label <- "all possible input-output-output triplets"
    }
    if (parameters$assign.MAXEIG == 3) {
      ind.label <- "output = two variables, input = sum of remaining variables"
    }
    cat("  indicators: ", ind.label, "\n")
    curve.info <- AssignMAXEIG(parameters)
    n.curves <- dim(curve.info)[1]
    cat("  number of curves: ", n.curves, "\n\n")
  }
  if (parameters$LMode) {
    cat("Summary of L-Mode analytic specifications\n")
    cat("  position beyond which to search for left mode: ", 
        parameters$mode.l, "\n")
    cat("  position beyond which to search for right mode: ", 
        parameters$mode.r, "\n\n")
  }
}
