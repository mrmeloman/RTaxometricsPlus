CreateSample <-
function(n, k, r, g, h, uniform) {
  # 
  # Generates sample of correlated data with univariate g-and-h distributions.
  #
  # Args:
  #        n: Size of sample to create (scalar).
  #        k: Number of variables (scalar).
  #        r: Correlation among variables (scalar).
  #        g: Parameter used to control asymmetry (scalar); sign indicates 
  #           direction and absolute value indicates magnitude of skew 
  #           (e.g., +/- .30 yields substantial asymmetry).
  #        h: Parameter used to control tail weight (scalar); positive values 
  #           yield tails that are longer/thinner than a standard normal curve, 
  #           negative values do the reverse (e.g., +/- .15 is a substantial 
  #           departure from normality).
  #  uniform: Whether to generate random values (the program default) 
  #           or use uniformly distributed quantiles (T/F).
  #   
  # Returns:
  #   Sample of data (matrix).
  # 
  max.trials <- 5
  initial.multiplier <- 1
  x <- matrix(0, nrow = n, ncol = k)
  distributions <- matrix(0, nrow = n, ncol = k)
  for (i in 1:k) {
  	distributions[, i] <- sort(CreateVariable(n, g, h, uniform))
  }
  iteration <- 0
  rmsr.best <- 2
  trials.without.improvement <- 0
  cor.target <- diag(k)
  cor.target <- cor.target + r - (diag(k) * r)
  cor.intermediate <- cor.target
  comp.shared <- matrix(rnorm(n, 0, 1), nrow = n, ncol = 1)
  comp.unique <- matrix(rnorm(n * k, 0, 1), nrow = n, ncol = k)
  load.shared <- matrix(0, nrow = k, ncol = 1)
  load.unique <- matrix(0, nrow = k, ncol = 1)
  while (trials.without.improvement < max.trials) {
    iteration <- iteration + 1
    factor.analysis <- RunFactorAnalysis(cor.intermediate, cor.matrix = TRUE, 
      n.factors = 1)
    load.shared[, 1] <- factor.analysis$loadings
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
      x[ ,i] <- (comp.shared %*% t(load.shared))[, i] + comp.unique[, i] * 
                load.unique[i, 1]
    }
    for (i in 1:k) {
       x <- x[sort.list(x[, i]), ]
       x[, i] <- distributions[, i]
    }
    cor.reproduced <- cor(x)
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
    n.factors = 1)
  load.shared[, 1] <- factor.analysis$loadings
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
    x[, i] <- (comp.shared %*% t(load.shared))[, i] + comp.unique[, i] * 
              load.unique[i, 1]
  }
  x <- apply(x, 2, scale)
  for (i in 1:k) {
    x <- x[sort.list(x[, i]), ]
    x[, i] <- distributions[, i]
  }
  return(x)
}
