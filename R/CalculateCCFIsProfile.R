CalculateCCFIsProfile <-
function(x.results, x.dim.results, x.cat.results, parameters) 
  {
  # 
  # Calculates CCFI results for CCFI profile.
  #
  # Args:
  #       x.results: Empirical data results (list).
  #   x.dim.results: Dimensional comparison data results (list).
  #   x.cat.results: Categorical comparison data results (list).
  #      parameters: Data and program parameters (list).
  #
  # Returns:
  #   Vector containing all CCFI values.
  #
  MAMBAC.CCFI <- 0
  MAXEIG.CCFI <- 0
  LMode.CCFI <- 0
  MAXSLOPE.CCFI <- 0
  sum.CCFI <- 0
  n.CCFI <- 0
  if (parameters$MAMBAC) {
    MAMBAC.CCFI <- CalculateCCFI(apply(x.results$MAMBAC, 2, mean), 
                                 apply(x.dim.results$MAMBAC, 2, mean),
                                 apply(x.cat.results$MAMBAC, 2, mean))
    sum.CCFI <- sum.CCFI + MAMBAC.CCFI
    n.CCFI <- n.CCFI + 1
  }
  if (parameters$MAXEIG) {
    MAXEIG.CCFI <- CalculateCCFI(apply(x.results$MAXEIG.y, 2, mean), 
                                 apply(x.dim.results$MAXEIG.y, 2, mean),
                                 apply(x.cat.results$MAXEIG.y, 2, mean))
    sum.CCFI <- sum.CCFI + MAXEIG.CCFI
    n.CCFI <- n.CCFI + 1
  }
  if (parameters$LMode) {
    LMode.CCFI <- CalculateLModeCCFI(x.results$LMode.x, x.results$LMode.y, 
                                     apply(x.dim.results$LMode.x, 2, mean),
                                     apply(x.dim.results$LMode.y, 2, mean),
                                     apply(x.cat.results$LMode.x, 2, mean), 
                                     apply(x.cat.results$LMode.y, 2, mean))                                
    sum.CCFI <- sum.CCFI + LMode.CCFI
    n.CCFI <- n.CCFI + 1
  }
  if (parameters$MAXSLOPE) {
    MAXSLOPE.CCFI <- CalculateCCFI(apply(x.results$MAXSLOPE.y, 2, mean),
                                   apply(x.dim.results$MAXSLOPE.y, 2, mean),
                                   apply(x.cat.results$MAXSLOPE.y, 2, mean))
    sum.CCFI <- sum.CCFI + MAXSLOPE.CCFI
    n.CCFI <- n.CCFI + 1
  }
  mean.CCFI <- sum.CCFI / n.CCFI
  output <- c(MAMBAC.CCFI, MAXEIG.CCFI, LMode.CCFI, MAXSLOPE.CCFI, mean.CCFI)
  return(output)
}
