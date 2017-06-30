RunFactorAnalysis <-
function(x, cor.matrix = FALSE, n.factors = 0, 
  max.iter = 50, criterion = .01) {
  # 
  # Performs factor analysis.
  #
  # Args:
  #            x: Data or correlation matrix (matrix). 
  #   cor.matrix: Whether or not x is a correlation matrix (T/F).
  #    n.factors: Number of factors to use (scalar).
  #     max.iter: Maximum number of iterations (scalar).
  #    criterion: Acceptably small change in h2 between iterations (scalar).
  #
  # Returns:
  #   Factor loadings and number of factors (list).
  # 
  x <- as.matrix(x)
  k <- dim(x)[2]
  if (n.factors == 0) n.factors <- k
  if (!cor.matrix) {
    cor.x <- cor(x)
  } else {
    cor.x <- x
  }
  old.h2 <- rep(99, k)
  h2 <- rep(0, k)
  change <- 1
  iter <- 0
  factor.loadings <- matrix(nrow = k, ncol = n.factors)
  while ((change >= criterion) & (iter < max.iter)) {
    iter <- iter + 1
    eig <- eigen(cor.x)
    l <- sqrt(eig$values[1:n.factors])
    for (i in 1:n.factors) {
      factor.loadings[, i] <- eig$vectors[, i] * l[i]
    }
    for (i in 1:k) {
      h2[i] <- sum(factor.loadings[i, ] * factor.loadings[i, ])
      change <- max(abs(old.h2 - h2))
      old.h2 <- h2
      diag(cor.x) <- h2
    }
  }
  if (n.factors == k) {
    n.factors <- sum(eig$values > 1)
  }
  if (factor.loadings[1, 1] < 0) {
    factor.loadings <- factor.loadings * -1
  }
  return(list(loadings = factor.loadings[, 1:n.factors], factors = n.factors))
}
