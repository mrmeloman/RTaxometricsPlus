GetSpecifications <-
function(parameters) {
  # 
  # Provides analytic specifications.
  #
  # Args:
  #          parameters: Data and program parameters (list).
  #
  # Returns a list object containing the following specifications, as applicable:
  #                   n: sample size (scalar).
  #         n.variables: number of indicator variables (scalar).
  #     comp.data.n.pop: size of each finite population of comparison data (scalar).
  # comp.data.n.samples: number of samples of each type of comparison data (scalar).
  #   comp.data.taxon.p: taxon base rate used to construct population of categorical 
  #                      comparison data (scalar).
  #        replications: number of internal replications (scalar).
  #   MAMBAC.indicators: method of assigning indicator variables to input-output 
  #                      roles for MAMBAC analyses (text).
  #     MAMBAC.n.curves: number of MAMBAC curves (scalar).
  #       MAMBAC.n.cuts: number of cutting scores used for each MAMBAC curve 
  #                      (scalar).
  #        MAMBAC.n.end: number of cases beyond final cutting score at each end of 
  #                      a MAMBAC curve (scalar).
  #   MAXEIG.indicators: method of assigning indicator variables to input-output 
  #                      roles for MAXEIG analyses (text).
  #     MAXEIG.n.curves: number of MAXEIG curves (scalar).
  #    MAXEIG.n.windows: number of overlapping windows used for each MAXEIG curve 
  #                      (scalar).
  #      MAXEIG.overlap: amount of overlap between adjacent windows in a MAXEIG
  #                      analysis (scalar).
  #   LMode.search.left: starting location to search for left mode in an LMode
  #                      analysis (scalar).
  #  LMode.search.right: starting location to search for right mode in an LMode
  #                      analysis (scalar).
  # 
  output <- list(n = parameters$n, 
                 n.variables = parameters$k, 
                 comp.data.n.pop = parameters$n.pop, 
                 comp.data.n.samples = parameters$n.samples)
  if (!parameters$profile) {
  	output <- c(output, comp.data.taxon.p = parameters$p)
  }
  output <- c(output, replications = parameters$reps)
  if (parameters$MAMBAC) {
    if (parameters$assign.MAMBAC == 1) {
      ind.label <- "all possible input-output pairs"
    } else { 
      ind.label <- "output = one variable, input = sum of remaining variables"
    }
    output <- c(output, MAMBAC.indicators = ind.label,
                MAMBAC.n.curves = dim(AssignMAMBAC(parameters))[1],
                MAMBAC.n.cuts = parameters$n.cuts,
                MAMBAC.n.end = parameters$n.end)
  }
  if (parameters$MAXEIG) {
    if (parameters$assign.MAXEIG == 1) {
      ind.label <- "all possible input-output-output triplets"
    }
    if (parameters$assign.MAXEIG == 2) {
      ind.label <- "input = one variable, output = all other variables"
    }
    if (parameters$assign.MAXEIG == 3) {
      ind.label <- "output = two variables, input = sum of remaining variables"
    }
    output <- c(output, MAXEIG.indicators = ind.label,
                MAXEIG.n.curves = dim(AssignMAXEIG(parameters))[1],
                MAXEIG.n.windows = parameters$windows,
                MAXEIG.overlap = parameters$overlap)
  }
  if (parameters$LMode & !parameters$profile) {
    output <- c(output, LMode.search.left = parameters$mode.l,
                LMode.search.right = parameters$mode.r)
  }
  output <- output[2:length(output)]
  return(output)
}
