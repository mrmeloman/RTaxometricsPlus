GenerateData <-
function(x, n, n.factors = 0, max.trials = 5, 
  initial.multiplier = 1) {
  # 
  # Generates population of comparison data.
  #
  # Args:
  #                    x: Data (matrix).
  #                    n: Size of population to create (scalar).
  #            n.factors: Number of factors used to reproduce correlations 
  #                       (scalar).
  #           max.trials: Maximum number of trials (scalar).
  #   initial.multiplier: Size of multiplier to adjust target correlations 
  #                       (scalar).
  #   
  # Returns:
  #   Population of comparison data (matrix).
  # 
  k <- dim(x)[2]
  x.comp <- matrix(0, nrow = n, ncol = k)
  distributions <- matrix(0, nrow = n, ncol = k)
  iteration <- 0
  rmsr.best <- 2
  trials.without.improvement <- 0
  for (i in 1:k) {
    if (min(x[, i]) == max(x[, i])) {
      x[, i] <- x[, i] + rnorm(dim(x)[1], 0, .0001)
    }
    variance <- F
    while (!variance) {
      distributions[, i] <- sort(sample(x[, i], size = n, replace = TRUE))
      variance <- min(distributions[, i]) != max(distributions[, i])
    }
  }
  cor.target <- cor(x)
  cor.intermediate <- cor.target
  if (n.factors == 0) {
    eigenvalues.observed <- eigen(cor.intermediate)$values
    eigenvalues.random <- matrix(0, nrow = 100, ncol = k)
    x.random <- matrix(0, nrow = n, ncol = k)
    for (i in 1:100) {
      for (j in 1:k) {
        variance <- F
        while (!variance) {
          x.random[, j] <- sample(distributions[, j], size = n, replace = TRUE)
          variance <- min(x.random[, j]) != max(x.random[, j])
        }
      }
      eigenvalues.random[i, ] <- eigen(cor(x.random))$values
    }
    eigenvalues.random <- apply(eigenvalues.random, 2, mean) 
    n.factors <- max(1, sum(eigenvalues.observed > eigenvalues.random))
  }
  comp.shared <- matrix(rnorm(n * n.factors, 0, 1), nrow = n, ncol = n.factors)
  comp.unique <- matrix(rnorm(n * k, 0, 1), nrow = n, ncol = k)
  load.shared <- matrix(0, nrow = k, ncol = n.factors)
  load.unique <- matrix(0, nrow = k, ncol = 1)
  while (trials.without.improvement < max.trials) {
    iteration <- iteration + 1
    factor.analysis <- RunFactorAnalysis(cor.intermediate, cor.matrix = TRUE, 
      n.factors = n.factors)
    if (n.factors == 1) {
    	  load.shared[, 1] <- factor.analysis$loadings
    	} else {
      for (i in 1:n.factors) {
        load.shared[, i] <- factor.analysis$loadings[, i]
      }
    }
    load.shared[load.shared > 1] <- 1
    load.shared[load.shared < -1] <- -1
    if (load.shared[1, 1] < 0) {
      load.shared <- load.shared * -1
    }
    for (i in 1:k) {
      if (sum(load.shared[i, ] * load.shared[i, ]) < 1) {
        load.unique[i, 1] <- (1 - sum(load.shared[i, ] * load.shared[i, ]))
      } else {
        load.unique[i, 1] <- 0
      }
    }
    load.unique <- sqrt(load.unique)
    for (i in 1:k) {
      x.comp[ ,i] <- (comp.shared %*% t(load.shared))[, i] + 
        comp.unique[, i] * load.unique[i, 1]
    }
    for (i in 1:k) {
       x.comp <- x.comp[sort.list(x.comp[, i]), ]
       x.comp[, i] <- distributions[, i]
    }
    cor.reproduced <- cor(x.comp)
    cor.residual <- cor.target - cor.reproduced
    rmsr <- sqrt(sum(cor.residual[lower.tri(cor.residual)] * 
      cor.residual[lower.tri(cor.residual)]) / (.5 * (k * k - k)))
    if (rmsr < rmsr.best) {
      rmsr.best <- rmsr
      cor.best <- cor.intermediate
      res.best <- cor.residual
      cor.intermediate <- cor.intermediate + initial.multiplier * cor.residual
      trials.without.improvement <- 0
    } else {
      trials.without.improvement <- trials.without.improvement + 1
      current.multiplier <- initial.multiplier * .5 ^ trials.without.improvement
      cor.intermediate <- cor.best + current.multiplier * res.best
    }
  }
  factor.analysis <- RunFactorAnalysis(cor.best, cor.matrix = TRUE, 
    n.factors = n.factors)
  if (n.factors == 1) {
    load.shared[, 1] <- factor.analysis$loadings
  } else {
    for (i in 1:n.factors)
      load.shared[,i] <- factor.analysis$loadings[, i]
  }
  load.shared[load.shared > 1] <- 1
  load.shared[load.shared < -1] <- -1
  if (load.shared[1, 1] < 0) {
    load.shared <- load.shared * -1
  }
  for (i in 1:k) {
    if (sum(load.shared[i, ] * load.shared[i, ]) < 1) {
      load.unique[i,1] <- (1 - sum(load.shared[i, ] * load.shared[i, ]))
    } else {
      load.unique[i, 1] <- 0
    }
  }
  load.unique <- sqrt(load.unique)
  for (i in 1:k) {
    x.comp[, i] <- (comp.shared %*% t(load.shared))[, i] + 
        comp.unique[, i] * load.unique[i, 1]
  }
  x.comp <- apply(x.comp, 2, scale)
  for (i in 1:k) {
    x.comp <- x.comp[sort.list(x.comp[, i]), ]
    x.comp[, i] <- distributions[, i]
  }
  return(x.comp)
}
