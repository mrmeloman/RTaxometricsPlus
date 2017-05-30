CreateData <-
function (str, n = 600, k = 4, p = .50, d = 2.00, r = .00, 
                        r.tax = .00, r.comp = .00, skew = 0, cuts = 0, 
                        seed = 1) {
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
  #     skew: Parameter used to control skewness (if nonzero); lower values 
  #           impart greater skew (scalar).
  #     cuts: Parameter used to create ordered categorieas, if nonzero (scalar).
  #     seed: Random number seed (scalar).
  #
  # Returns:
  #   Sample of data; k columns contain data, final column contains classification
  #   (matrix).
  # 
  set.seed(seed)
  if (skew < 0) {
    p <- 1 - p
  }
  if (str == "dim") {  # create sample of dimensional data
    data <- matrix(rep(0, n * k), ncol = k)
  if (r > 0) {
    r.exp <- r
  } else {
    r.wg <- mean(c(r.tax, r.comp))
    r.exp <- (p * (1 - p) * d ^ 2 + r.wg) / (1 + (p * (1 - p) * d ^ 2))
  }
    loading.common <- sqrt(r.exp)
    loading.error <- sqrt(1 - r.exp)
    random.common <- rnorm(n, mean = 0, sd = 1)
    for (i in 1:k)
      data[, i] <- loading.common * random.common + loading.error *
        rnorm(n, mean = 0, sd = 1)
  } else {  # create sample of categorical data
    data <- matrix(rep(0, n * (k + 1)), ncol = (k + 1))
    n.tax <- round(n * p)
    n.comp <- n - n.tax
    data[, (k + 1)] <- c(rep(2, n.tax), rep(1, n.comp))
    loading.common <- sqrt(r.tax)  # create data representing taxon
    loading.error <- sqrt(1 - r.tax)
    random.common <- rnorm(n.tax, mean = 0, sd = 1)
    for (i in 1:k) {
      data[1:n.tax, i] <- loading.common * random.common + loading.error * 
        rnorm(n.tax, mean = 0, sd = 1)
    }
    loading.common <- sqrt(r.comp)  # create data representing complement
    loading.error <- sqrt(1 - r.comp)
    random.common <- rnorm(n.comp, mean = 0, sd = 1)
    for (i in 1:k) {
      data[(n.tax + 1):n, i] <- loading.common * random.common + 
        loading.error * rnorm(n.comp, mean = 0, sd = 1)
    }
    if (skew >= 0) {
      for (i in 1:k)
        data[, i] <- data[, i] + (d * (data[, (k + 1)] - 1.5))
    } else {
      for (i in 1:k)
        data[, i] <- data[, i] - (d * (data[, (k + 1)] - 1.5))
    }
    if (skew > 0) {
      for (i in 1:k)
        data[, i] <- exp(data[, i] / skew)
    }
    if (skew < 0) {
      for (i in 1:k)
        data[, i] <- -exp(data[, i] / skew)
      if (str != "dim") {
        data[, (k + 1)] <- 3 - data[, (k + 1)]
      }
    }
  }
  data[, 1:k] <- apply(data[, 1:k], 2, scale)  # standardize variables
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
  if (str == "dim") {
    data <- ClassifyCases(data, p = p)
  }
  as.data.frame(data)
  return(data)
}
