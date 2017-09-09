CreateData <-
function (str, n = 600, k = 4, p = .50, d = 2.00, r = .00, 
                        r.tax = .00, r.comp = .00, g = 0, h = 0, cuts = 0, 
                        uniform = F, seed = 1) {
  # 
  # Creates a data set that can vary according to a number of basic parameters.
  #
  # Args:
  #      str: Structure of the data, either "dim" for dimensional data or 
  #           anything else for categorical data (text).
  #        n: Sample size (scalar).
  #        k: Number of variables (scalar).
  #        p: Taxon base rate (scalar).
  #        d: Standardized mean difference between groups (scalar).
  #        r: Correlation among variables (scalar).
  #    r.tax: Correlation among variables within the taxon (scalar).
  #   r.comp: Correlation among variables within the complement (scalar).
  #        g: Parameter used to control asymmetry (scalar); sign indicates
  #           direction and absolute value indicates magnitude of skew (e.g., 
  #           +/- .30 yields substantial asymmetry).
  #        h: Parameter used to control tail weight (scalar); positive 
  #           values yield tails that are longer/thinner than a standard 
  #           normal curve, negative values do the reverse (e.g., +/- .15 
  #           is a substantial departure from normality).
  #     cuts: Parameter used to create ordered categorieas, if nonzero 
  #           (scalar); number of categories will be cuts + 1.
  #  uniform: Whether to generate random values (the program default) or use 
  #           uniformly distributed quantiles (T/F).
  #     seed: Random number seed (scalar).
  #
  # Returns:
  #   Sample of data (matrix).
  # 
  set.seed(seed)
  data <- matrix(0, nrow = n, ncol = k + 1)
  if (str == "dim") {  # create sample of dimensional data
    if (r > 0) {
      r.exp <- r
    } else {
      r.wg <- mean(c(r.tax, r.comp))
      r.exp <- (p * (1 - p) * d ^ 2 + r.wg) / (1 + (p * (1 - p) * d ^ 2))
    }
    data[, 1:k] <- CreateSample(n, k, r.exp, g, h, uniform)
    data <- ClassifyCases(data[, 1:k], p)
  } else {  # create sample of categorical data
    n.tax <- round(n * p)
    n.comp <- n - n.tax
    data[1:n.tax, 1:k] <- CreateSample(n.tax, k, r.tax, g, h, uniform)
    data[(n.tax + 1):n, 1:k] <- CreateSample(n.comp, k, r.comp, g, h, uniform)
    data[, k + 1] <- c(rep(2, n.tax), rep(1, n.comp))
    for (i in 1:k) {
      data[, i] <- data[, i] + (d * (data[, (k + 1)] - 1.5))
    }
  }
  if (cuts > 0) {
    for (i in 1:k) {
      x.sorted <- data[sort.list(data[, i]), i]
      trim <- x.sorted[round(.01 * n):round(.99 * n)]
      dens <- density(trim, from = min(trim), to = max(trim))
      n.dens <- length(dens$y)
      cut.pts <- round(seq(from = 0, to = n.dens, length = (cuts + 1)))
      cut.vals <- rep(0, cuts)
      for (j in 1:cuts)
      cut.vals[j] <- sum(dens$y[(cut.pts[j] + 1):cut.pts[j + 1]])
      vals <- round(cut.vals * (n / sum(cut.vals)))
      if (sum(vals) != n) {
        vals[round(cuts / 2)] <- vals[round(cuts/2)] + (n - sum(vals))
      }
      for (j in 1:cuts)
      cut.vals[j] <- x.sorted[sum(vals[1:j])]
      cut.vals <- c(-100, cut.vals, 100)
      data[, i] <- cut(data[, i], breaks = cut.vals)[1:n]
    }
  }
  as.data.frame(data)
  return(data)
}
