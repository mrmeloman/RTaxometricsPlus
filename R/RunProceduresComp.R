RunProceduresComp <-
function(x, parameters) {
  # 
  # Runs MAMBAC, MAXEIG, L-Mode, MAXSLOPE analyses for comparison data.
  #
  # Args:
  #            x: Data (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Averaged curves for each procedure performed (list).
  # 
  b <- parameters$n.samples
  MAMBAC.results <- matrix(0, nrow = b, ncol = parameters$n.cuts)
  MAXEIG.results.x <- matrix(0, nrow = b, ncol = parameters$windows)
  MAXEIG.results.y <- matrix(0, nrow = b, ncol = parameters$windows)
  LMode.results.x <- matrix(0, nrow = b, ncol = 512)
  LMode.results.y <- matrix(0, nrow = b, ncol = 512)
  MAXSLOPE.results.x <- matrix(0, nrow = b, ncol = parameters$n - 1)
  MAXSLOPE.results.y <- matrix(0, nrow = b, ncol = parameters$n - 1)
  for (i in 1:b) {
    variance <- F
    while (!variance) {
      x.b <- x[sample(1:parameters$n.pop, parameters$n, replace = TRUE), ]
      variance <- prod(diag(var(x.b)))
    }
    if (parameters$MAMBAC) {
      MAMBAC.results.1 <- RunMAMBAC(x.b, parameters)
      MAMBAC.results[i, ] <- apply(MAMBAC.results.1, 2, mean)
    }
    if (parameters$MAXEIG) {
      MAXEIG.results <- RunMAXEIG(x.b, parameters)
      MAXEIG.results.x.1 <- MAXEIG.results$curve.x
      MAXEIG.results.y.1 <- MAXEIG.results$curve.y
      MAXEIG.results.x[i, ] <- apply(MAXEIG.results.x.1, 2, mean)
      MAXEIG.results.y[i, ] <- apply(MAXEIG.results.y.1, 2, mean)
    }
    if (parameters$LMode) {
      LMode.results <- RunLMode(x.b)
      LMode.results.x[i, ] <- LMode.results$curve.x
      LMode.results.y[i, ] <- LMode.results$curve.y
    }
    if (parameters$MAXSLOPE) {
      MAXSLOPE.results <- RunMAXSLOPE(x.b)
      MAXSLOPE.results.x.1 <- MAXSLOPE.results$curve.x
      MAXSLOPE.results.y.1 <- MAXSLOPE.results$curve.y
      MAXSLOPE.results.x[i, ] <- apply(MAXSLOPE.results.x.1, 2, mean)
      MAXSLOPE.results.y[i, ] <- apply(MAXSLOPE.results.y.1, 2, mean)
    }
  }
  return(list(MAMBAC = MAMBAC.results, MAXEIG.x = MAXEIG.results.x, 
    MAXEIG.y = MAXEIG.results.y, LMode.x = LMode.results.x, LMode.y = 
    LMode.results.y, MAXSLOPE.x = MAXSLOPE.results.x, MAXSLOPE.y = 
    MAXSLOPE.results.y))
}
