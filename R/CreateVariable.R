CreateVariable <-
function(n, g, h, uniform) {
  # 
  # Generates variable with g-and-h distribution.
  #
  # Args:
  #        n: Size of sample to create (scalar).
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
  #   Single variable (vector).
  # 
  if (uniform) {
    z <- qnorm(seq(from = (.5 / n), to = ((n - .5) / n), length = n), 0, 1)
  } else {
    z <- rnorm(n, 0, 1)
  }
  if ((g == 0) & (h == 0)) {
    y <- z
    m.pop <- 0
    sd.pop <- 1
  }
  if ((g != 0) & (h == 0)) {
    y <- (exp(g * z) - 1) / g
    m.pop <- (exp((g ^ 2) / 2) - 1) / g
    sd.pop <- sqrt(exp(g ^ 2) * (exp(g ^ 2) - 1) / (g ^ 2))
  }
  if (h >= .5) {
    h <- .49
  }
  if ((g == 0) & (h != 0)) {
    y <- z * exp((h * z ^ 2) / 2)
    m.pop <- 0
    sd.pop <- sqrt(1 / ((1 - 2 * h) ^ (3 / 2)))
  }
  if ((g != 0) & (h != 0)) {
    y <- ((exp(g * z) - 1) / g) * exp((h * z ^ 2) / 2)
    m.pop <- (1 / (g * sqrt(1 - h))) * (exp((g ^ 2) / (2 * (1 - h))) - 1)
    sd.pop <- sqrt((1 / ((g ^ 2) * sqrt(1 - (2 * h)))) * 
              ((exp((2 * g ^ 2) / (1 - 2 * h))) - 
              (2 * exp((g ^ 2) / (2 * (1 - 2 * h)))) + 1))
  }
  y <- ((y - m.pop) / sd.pop)
  return(y)
}
