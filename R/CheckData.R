CheckData <-
function(x) {
  # 
  # Checks whether the data are appropriate for taxometric analysis.
  #
  # Args:
  #   x: Data containing case classifications in the final column coded as 1 = 
  #      complement, 2 = taxon (matrix).
  #
  # Returns:
  #   Nothing; text output only.
  # 
  x <- RemoveMissingData(x)
  n <- dim(x)[1]
  k <- dim(x)[2] - 1
  group <- x[, k + 1]
  x <- x[, 1:k]
  x.t <- x[(group == 2), ]
  x.c <- x[(group == 1), ]
  n.t <- dim(x.t)[1]
  n.c <- dim(x.c)[1]
  p <- n.t / n
  for (i in 1:k) {  # prevent crashes when variance = 0 within group(s)
    if (min(x.t[, i]) == max(x.t[, i])) {
      x.t[, i] <- x.t[, i] + rnorm(n.t, 0, .0001)
    }
    if (min(x.c[, i]) == max(x.c[, i])) {
      x.c[, i] <- x.c[, i] + rnorm(n.c, 0, .0001)
    }
  }
  cat("\nSample size:  N =", n, "\n")
  if (n < 300) {
    cat("  * This is smaller than the recommended minimum of N = 300.\n")
  }
  cat("Taxon base rate:  P =", p, "\n")
  if (p < .10) {
    cat("  * This is smaller than the recommended minimum of P = .10.\n")
  }
  if (p > .90) {
    cat("  * This is larger than the recommended maximum of P = .90.\n")
  }
  cat("Taxon size:  n =", n.t, "\n")
  if (n.t < 50) {
    cat("  * It may be difficult to differentiate categories when one",
        "is this small.\n")
  }
  cat("Complement size:  n =", n.c, "\n")
  if (n.c < 50) {
    cat("  * It may be difficult to differentiate categories when one",
        "is this small.\n")
  }
  cat("Number of variables:  k =", k, "\n")
  if (k == 1) {
    cat("  * Not enough variables to perform any taxometric procedures.\n")
  }
  if (k == 2) {
    cat("  * Only enough variables to perform MAMBAC and MAXSLOPE.\n")
  }
  dists <- matrix(0, nrow = k, ncol = 4)
  dimnames(dists) <- list(paste("v", 1:k, sep = ""), 
    c("M", "SD", "Skewness", "Kurtosis"))
  for (i in 1:k)
    dists[i, ] <- SummarizeDist(x[, i])
  cat("\nDistributions:\n\n")
  print(round(dists, 2))
  validities <- matrix(0, nrow = k + 1, ncol = 1)
  dimnames(validities) <- list(c(paste("v", 1:k, sep = ""), "Mean"), 
    "Cohen's d")
  for (i in 1:k)
    validities[i, 1] <- CalculateValidity(x.t[, i], x.c[, i])
  validities[k + 1, 1] <- mean(validities[1:k, 1])
  cat("\nValidities:\n\n")
  print(round(validities, 2))
  if (min(validities) < 1.25) {
    cat("  * One or more values below the recommended minimum of d = 1.25.\n")
  }
  cat("\nWithin-group correlations (taxon):\n\n")
  cor.t <- cor(x.t)
  dimnames(cor.t) <- list(paste("v", 1:k, sep = ""), paste("v", 1:k, sep = ""))
  print(round(cor.t, 2))
  cat("Mean =", round(mean(cor.t[lower.tri(cor.t)]), 2), "\n")
  if (max(cor.t[lower.tri(cor.t)]) > .30) {
    cat("  * One or more values above the recommended maximum of r = .30.\n")
  }
  cat("\nWithin-group correlations (complement):\n\n")
  cor.c <- cor(x.c)
  dimnames(cor.c) <- list(paste("v", 1:k, sep = ""), paste("v", 1:k, sep = ""))
  print(round(cor.c, 2))
  cat("Mean =", round(mean(cor.c[lower.tri(cor.c)]), 2), "\n")
  if (max(cor.c[lower.tri(cor.c)]) > .30) {
    cat("  * One or more values above the recommended maximum of r = .30.\n")
  }
}
