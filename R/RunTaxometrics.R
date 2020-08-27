RunTaxometrics <-
function(x, seed = 1, n.pop = 100000, n.samples = 100, 
  reps = 1, MAMBAC = TRUE, assign.MAMBAC = 1, n.cuts = 50, n.end = 25, 
  MAXEIG = TRUE, assign.MAXEIG = 1, windows = 50, overlap = .90, LMode = TRUE, 
  mode.l = -.001, mode.r = .001, MAXSLOPE = FALSE, graph = 1, text.file = FALSE) {
  # 
  # Performs a series of taxometric analyses for a sample of data.
  # 
  # Args:
  #                x: Data (matrix).
  #             seed: Random number seed (scalar).
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
  #
  # Returns:
  #   CCFI values.
  # 
  set.seed(seed)
  cat("\nSTATUS OF PROGRAM EXECUTION\n\n")
  cat("Checking for missing data\n")
  x <- RemoveMissingData(x)
  n <- dim(x)[1]
  k <- dim(x)[2] - 1
  group <- x[, k + 1]
  x <- x[, 1:k]
  cat("Checking classification variable\n")
  CheckClassification(group, n)
  p <- sum(group == 2) / n
  parameters <- list(n = n, k = k, p = p, n.pop = n.pop, 
                     n.samples = n.samples, reps = reps, MAMBAC = MAMBAC, 
                     assign.MAMBAC = assign.MAMBAC, n.cuts = n.cuts, 
                     n.end = n.end, MAXEIG = MAXEIG, 
                     assign.MAXEIG = assign.MAXEIG, windows = windows, 
                     overlap = overlap, LMode = LMode, mode.l = mode.l, 
                     mode.r = mode.r, MAXSLOPE = MAXSLOPE, graph = graph,
                     text.file = text.file, profile = FALSE)
  cat("Checking for variance\n")
  x <- AddVariance(x, k, parameters)
  x <- apply(x, 2, scale)
  cat("Checking program parameters\n")
  parameters <- CheckParameters(x, parameters)
  cat("Generating population of dimensional comparison data\n")
  x.dim <- GenerateData(x, n = parameters$n.pop)
  cat("Generating population of categorical comparison data\n")
  cat("  Generating taxon\n")
  x.cat.taxon <- GenerateData(x[(group == 2), ], 
                              n = round(parameters$n.pop * p))
  cat("  Generating complement\n")
  x.cat.complement <- GenerateData(x[(group == 1), ], 
                                   n = parameters$n.pop - 
                                   round(parameters$n.pop * p))
  x.cat <- rbind(x.cat.taxon, x.cat.complement)
  cat("Analyzing empirical data\n")
  x.results <- RunProcedures(x, parameters)
  cat("Analyzing samples of dimensional comparison data\n")
  x.dim.results <- RunProceduresComp(x.dim, parameters)
  cat("Analyzing samples of categorical comparison data\n")
  x.cat.results <- RunProceduresComp(x.cat, parameters)
  
  if (parameters$text.file) {
    cat("Printing results to .txt file")
    sink(paste("RunTaxometrics ", date(), ".txt", sep = ""))
  }
  
  DisplaySpecifications(parameters)
  
  CCFI <- DisplayCCFIs(x.results, x.dim.results, x.cat.results, parameters)
  DisplayBaseRates(x.results, parameters)
  
  if (!parameters$text.file){
    DisplayPanels(x.results, x.dim.results, x.cat.results, parameters)
  }
  
  if (parameters$text.file) {
    sink()
    if (parameters$graph == 2) {
      DisplayPanels(x.results, x.dim.results, x.cat.results, parameters)
    }
    if (parameters$graph == 3) {
      DisplayPanels(x.results, x.dim.results, x.cat.results, parameters)
    }
  }
  
  invisible(CCFI)
  

}
