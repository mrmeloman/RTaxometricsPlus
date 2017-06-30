CheckParameters <-
function(x, parameters) {
  # 
  # Checks parameter specifications for problems, adjusts as needed.
  #
  # Args:
  #            x: Data (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Data parameters, adjusted as needed (list).
  # 
  if (parameters$profile) {
    if (parameters$min.p < .025) {
    	  parameters$min.p <- .025
    	  cat("  * min.p too small, set to .025\n")
    }
    if (parameters$max.p > .975) {
      parameters$max.p <- .975
      cat("  * max.p too large, set to .975\n")
    }
    if (parameters$min.p > parameters$max.p) {
      parameters$min.p <- .025
      parameters$max.p <- .975
      cat("  * min.p > max.p, set to .025 and .975\n")
    }
    if (parameters$num.p < 20) {
    	  parameters$num.p <- 20
      cat("  * num.p too small, set to 20\n")
    }
  }
  if (parameters$n.pop < 10000) {
    parameters$n.pop <- 10000
    cat("  * n.pop too small, set to 10000\n")
  }
  if (parameters$n.samples < 10) {
    parameters$n.samples <- 10
    cat("  * n.samples too small, set to 10\n")
  }
  ties <- F
  for (i in 1:parameters$k) {
    if (length(unique(x[, i])) < parameters$n) {
      ties <- T
    }
  }
  if (parameters$reps < 1) {
    parameters$reps <- 1
    cat("  * reps too small, set to 1\n")
  }
  if ((parameters$reps > 1) & (!ties)) {
    parameters$reps <- 1
    cat("  * no tied scores, reps set to 1\n")
  }
  if ((parameters$reps == 1) & (ties)) {
    parameters$reps <- 10
    cat("  * tied scores, reps set to 10\n")
  }
  if (!parameters$MAMBAC & !parameters$MAXEIG & !parameters$LMode & 
      !parameters$MAXSLOPE) {
    parameters$MAMBAC <- T
    parameters$MAXEIG <- T
    parameters$LMode <- T
    parameters$MAXSLOPE <- T
    cat("  * no procedures requested, reset to request all procedures\n")
  }
  if ((parameters$k > 2) & (parameters$MAXSLOPE)) {
    parameters$MAXSLOPE <- F
    cat("  * more than 2 variables, MAXSLOPE will not be performed\n")
  }
  if (parameters$k == 2) {
    if (parameters$MAXSLOPE) {
      if ((parameters$MAXEIG) & (!parameters$LMode)) {
        cat("  * only 2 variables, MAXEIG cannot be performed\n")
      }
      if ((!parameters$MAXEIG) & (parameters$LMode)) {
        cat("  * only 2 variables, L-Mode cannot be performed\n")
      }
      if ((parameters$MAXEIG) & (parameters$LMode)) {
        cat("  * only 2 variables, MAXEIG and L-Mode cannot be performed\n")
      } 
    } else {
      if ((parameters$MAXEIG) & (!parameters$LMode)) {
        cat("  * only 2 variables, MAXSLOPE will replace MAXEIG\n")
        parameters$MAXSLOPE <- T
      }
      if ((!parameters$MAXEIG) & (parameters$LMode)) {
        cat("  * only 2 variables, L-Mode cannot be performed\n")
      }
      if ((parameters$MAXEIG) & (parameters$LMode)) {
        cat("  * only 2 variables, MAXSLOPE will replace MAXEIG and L-Mode\n")
        parameters$MAXSLOPE <- T
      }
    }
    parameters$MAXEIG <- F
    parameters$LMode <- F
  }
  if ((parameters$MAMBAC) & ((parameters$assign.MAMBAC < 1) | 
      (parameters$assign.MAMBAC > 2))) {
    parameters$assign.MAMBAC <- 1
    cat("  * invalid MAMBAC assignment method, set to method 1\n")
  }
  if ((parameters$MAMBAC) & (parameters$n.end < 10)) {
    parameters$n.end <- 10
    cat("  * n.end too small, set to 10\n")
  }
  if ((parameters$MAMBAC) & (parameters$n.end > (parameters$n / 4))) {
    parameters$n.end <- round(parameters$n / 4)
    cat("  * n.end too large, set to N / 4 =", parameters$n.end, "\n")
  }
  if ((parameters$MAMBAC) & (parameters$n.cuts < 25)) {
    parameters$n.cuts <- 25
    cat("  * n.cuts too small, set to 25\n")
  }
  if ((parameters$MAMBAC) & (parameters$n.cuts > (parameters$n / 2))) {
    parameters$n.cuts <- round(parameters$n / 2)
    cat("  * n.cuts too large, set to N / 2 =", parameters$n.cuts, "\n")
  }
  if ((parameters$MAXEIG) & ((parameters$assign.MAXEIG < 1) | 
      (parameters$assign.MAXEIG > 3))) {
    parameters$assign.MAXEIG <- 1
    cat("  * invalid MAXEIG assignment method, set to method 1\n")
  }
  if ((parameters$MAXEIG) & (parameters$windows < 10)) {
    parameters$windows <- 10
    cat("  * windows too small, set to 10\n")
  }
  if ((parameters$MAXEIG) & (parameters$windows > (parameters$n / 10))) {
    parameters$windows <- round(parameters$n / 10)
    cat("  * windows too small, set to N / 10 =", parameters$windows, "\n")
  }
  if ((parameters$MAXEIG) & (parameters$overlap < 0)) {
    parameters$overlap <- 0
    cat("  * overlap too small, set to 0\n")
  }
  if ((parameters$MAXEIG) & (parameters$overlap > .9)) {
    parameters$overlap <- .9
    cat("  * overlap too large, set to .9\n")
  }
  if ((parameters$LMode) & (parameters$mode.l >= 0)) {
    parameters$mode.l <- -.001
    cat("  * mode.l non-negative, set to -.001\n")
  }
  if ((parameters$LMode) & (parameters$mode.r <= 0)) {
    parameters$mode.r <- .001
    cat("  * mode.r non-positive, set to .001\n")
  }
  return(parameters)
}
