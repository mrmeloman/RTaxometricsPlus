CalculateBaseRates <-
function(x.results, parameters) {
  # 
  # Calculates and returns base rate estimates for taxometric analysis.
  #
  # Args:
  #    x.results: Empirical data results (list).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   List object containing all base rate estimates.
  #
  sum.p <- 0
  n.p <- 0
  MAMBAC.p <- 0
  MAXEIG.p <- 0
  LMode.p <- 0
  MAXSLOPE.p <- 0
  output <- list(0)
  if (parameters$MAMBAC) {
    MAMBAC.p <- EstimateMAMBAC(x.results$MAMBAC)
    sum.p <- sum.p + MAMBAC.p
    n.p <- n.p + 1  
    output <- c(output, p.MAMBAC = MAMBAC.p)
  }
  if (parameters$MAXEIG) {
    MAXEIG.p <- EstimateMAXEIG(x.results$MAXEIG.y)
    sum.p <- sum.p + MAXEIG.p
    n.p <- n.p + 1 
    output <- c(output, p.MAXEIG = MAXEIG.p)
  }
  if (parameters$LMode) {
    LMode.p <- EstimateLMode(x.results$LMode.x, x.results$LMode.y, parameters)
    sum.p <- sum.p + LMode.p$p.estimate
    n.p <- n.p + 1 
    output <- c(output, p.LMode = LMode.p$p.estimate)
  }
  if (parameters$MAXSLOPE) {
    MAXSLOPE.p <- EstimateMAXSLOPE(x.results$MAXSLOPE.x, x.results$MAXSLOPE.y)
    sum.p <- sum.p + MAXSLOPE.p
    n.p <- n.p + 1 
    output <- c(output, p.MAXSLOPE = MAXSLOPE.p)
  }
  mean.p <- sum.p / n.p
  output <- c(output, p.mean = mean.p)
  output <- output[2:length(output)]
  return(output)
}
