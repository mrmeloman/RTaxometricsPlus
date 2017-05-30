RunProcedures <-
function(x, parameters) {
  # 
  # Runs MAMBAC, MAXEIG, L-Mode, MAXSLOPE analyses for empirical data.
  #
  # Args:
  #            x: Data (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Curve-level data for each procedure performed (list).
  # 
  k <- parameters$k
  variance <- T
  for (i in 1:dim(x)[2]) {
    if (var(x[, i]) == 0) {
    	  variance <- F
      while (!variance) {
        x[, i] <- sort(sample(x[, i], parameters$n, replace = TRUE))
        variance <- min(x[, i]) != max(x[, i])
      }
    }
  }
  if (parameters$MAMBAC) {
    MAMBAC.results <- RunMAMBAC(x, parameters)
  } else {
    MAMBAC.results <- 0
  }
  if (parameters$MAXEIG) {
    MAXEIG.results <- RunMAXEIG(x, parameters)
    MAXEIG.results.x <- MAXEIG.results$curve.x
    MAXEIG.results.y <- MAXEIG.results$curve.y
  } else {
    MAXEIG.results.x <- 0
    MAXEIG.results.y <- 0
  }
  if (parameters$LMode) {
    LMode.results <- RunLMode(x)
    LMode.results.x <- LMode.results$curve.x
    LMode.results.y <- LMode.results$curve.y
  } else {
    LMode.results.x <- 0
    LMode.results.y <- 0
  }
  if (parameters$MAXSLOPE) {
    MAXSLOPE.results <- RunMAXSLOPE(x)
    MAXSLOPE.results.x <- MAXSLOPE.results$curve.x
    MAXSLOPE.results.y <- MAXSLOPE.results$curve.y
  } else {
    MAXSLOPE.results.x <- 0
    MAXSLOPE.results.y <- 0
  }
  return(list(MAMBAC = MAMBAC.results, MAXEIG.x = MAXEIG.results.x, 
    MAXEIG.y = MAXEIG.results.y, LMode.x = LMode.results.x, LMode.y = 
    LMode.results.y, MAXSLOPE.x = MAXSLOPE.results.x, MAXSLOPE.y = 
    MAXSLOPE.results.y))
}
