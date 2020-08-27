RunCCFIProfile <-
function(x, seed = 1, min.p = .025, max.p = .975, num.p = 39, 
  n.pop = 100000, n.samples = 100, reps = 1, MAMBAC = TRUE, assign.MAMBAC = 1, 
  n.cuts = 50, n.end = 25, MAXEIG = TRUE, assign.MAXEIG = 1, windows = 50, 
  overlap = .90, LMode = TRUE, mode.l = -.001, mode.r = .001, MAXSLOPE = FALSE, 
  graph = 1, text.file = FALSE, profile = TRUE) {
  # 
  # Performs a series of taxometric analyses to generate a CCFI profile.
  # 
  # Args:
  #                x: Data (matrix).
  #             seed: Random number seed (scalar).
  #            min.p: Minimum base rate for CCFI profile (scalar).
  #            max.p: Maximum base rate for CCFI profile (scalar).
  #            num.p: Number of base rates for CCFI profile (scalar).
  #            n.pop: Size of populations of comparison data (scalar).
  #        n.samples: Number of samples of comparison data (scalar).
  #             reps: Number of times to resort tied scores and redo 
  #                   calculations, averaging to obtain final results (scalar).
  #           MAMBAC: Whether MAMBAC is performed (T/F).
  #    assign.MAMBAC: Whether variables are used in all input-output pairings
  #                   (1) or variables are summed to form input (2).
  #           n.cuts: Number of cuts in MAMBAC (scalar).
  #            n.end: Number of cases beyond final cuts in MAMBAC (scalar).
  #           MAXEIG: Whether MAXEIG is performed (T/F).
  #    assign.MAXEIG: Whether variables are used in all input-output 
  #                   triplets (1), each variable serves as input once (2), 
  #                   or variables are summed to form input (3).
  #          windows: Number of overlapping windows (scalar).
  #          overlap: Proportion of overlap between windows (scalar).
  #            LMode: Whether L-Mode is performed (T/F).
  #           mode.l: Position beyond which to search for left mode (scalar).
  #           mode.r: Position beyond which to search for right mode (scalar).
  #         MAXSLOPE: Whether MAXSLOPE is performed (T/F).
  #            graph: Whether to display graphs on screen (1), save as a
  #                   compressed .jpeg file (2), or save as a high-resolution
  #                   .tiff file (3)
  #        text.file: Whether to divert text output to a .txt file (T/F).
  #          profile: Whether CCFI profile is generated (T/F).
  # 
  # Returns:
  #   Aggregated CCFI values.
  # 
  set.seed(seed)
  cat("\nSTATUS OF PROGRAM EXECUTION\n\n")
  cat("Checking for missing data\n")
  x <- RemoveMissingData(x)
  n <- dim(x)[1]
  k <- dim(x)[2]
  parameters <- list(min.p = min.p, max.p = max.p, num.p = num.p, n = n, k 
                     = k, n.pop = n.pop, n.samples = n.samples, reps = reps, 
                     MAMBAC = MAMBAC, assign.MAMBAC = assign.MAMBAC, 
                     n.cuts = n.cuts, n.end = n.end, MAXEIG = MAXEIG, 
                     assign.MAXEIG = assign.MAXEIG, windows = windows, 
                     overlap = overlap, LMode = LMode, mode.l = mode.l, 
                     mode.r = mode.r, MAXSLOPE = MAXSLOPE, graph = graph, 
                     text.file = text.file, profile = TRUE)
  cat("Checking for variance\n")
  x <- AddVariance(x, k, parameters)
  x <- apply(x, 2, scale)
  cat("Checking program parameters\n")
  parameters <- CheckParameters(x, parameters)
  cat("Analyzing empirical data\n")
  x.results <- RunProcedures(x, parameters)
  cat("Generating population of dimensional comparison data\n")
  x.dim <- GenerateData(x, n = parameters$n.pop)
  cat("Analyzing samples of dimensional comparison data\n")
  x.dim.results <- RunProceduresComp(x.dim, parameters)
  cat("Generating populations of categorical comparison data and",
      "analyzing samples\n")
  ps <- seq(parameters$min.p, parameters$max.p, length = parameters$num.p)
  CCFIs <- matrix(0, nrow = 5, ncol = parameters$num.p)
  baserates <- matrix(0, nrow = 5, ncol = parameters$num.p)
  sum.CCFI <- 0
  sum.p <- 0
  num <- 0
  for (i in 1:parameters$num.p) {
    x.classes <- ClassifyCases(x, p = ps[i])
    cat("  p =", round(ps[i], 3), "\n")
    group <- x.classes[, k + 1]
    x.cat.taxon <- GenerateData(x.classes[(group == 2), 1:k],
      n = round(parameters$n.pop * ps[i]))
    x.cat.complement <- GenerateData(x.classes[(group == 1), 1:k],
      n = parameters$n.pop - round(parameters$n.pop * ps[i]))
    x.cat <- rbind(x.cat.taxon, x.cat.complement)
    x.cat.results <- RunProceduresComp(x.cat, parameters)
    CCFIs[, i] <- CalculateCCFIs(x.results, x.dim.results, x.cat.results,
                                 parameters)
  }
  
  if (parameters$text.file) {
    cat("Printing results to .txt file\n\n")
    sink(paste("RunCCFIProfile ", date(), ".txt", sep = ""))
  }
  
  DisplaySpecifications(parameters)
  textoutput <- DisplayTextOutput(CCFIs, parameters)
  
  if (!parameters$text.file) {
    DisplayProfiles(CCFIs, parameters)
  }
  
  if (parameters$text.file) {
    sink()
    if (parameters$graph == 2) {
      DisplayProfiles(CCFIs, parameters)
    }
    if (parameters$graph == 3) {
      DisplayProfiles(CCFIs, parameters)
    }
  }
  
  invisible(textoutput)
}
