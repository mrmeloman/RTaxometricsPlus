DisplayPanels <-
function(x.results, x.dim.results, x.cat.results, parameters) {
  # 
  # Provides panels of graphs for taxometric analysis.
  #
  # Args:
  #       x.results: Empirical data results (list).
  #   x.dim.results: Dimensional comparison data results (list).
  #   x.cat.results: Categorical comparison data results (list).
  #      parameters: Data and program parameters (list).
  #
  # Returns:
  #   Nothing; graphical output.
  #
  if (parameters$graph == 1) {
    dev.new(height = 6, width = 4.5)
  }
  if (parameters$graph == 2) {
  	jpeg(paste("RunTaxometrics ", date(), ".jpeg", sep = ""), units = "in", 
  	     height = 6, width = 4.5, res = 500, quality = 50)
  }
  if (parameters$graph == 3) {
  	tiff(paste("RunTaxometrics ", date(), ".tiff", sep = ""), units = "in", 
  	     height = 6, width = 4.5, res = 500)
  }
  par(mfrow = c(3, 2), omi = c(0, .2, .2, 0), mai = c(.7, .5, .25, .25))
  if (parameters$MAMBAC) {
    PlotPanel(x.results, x.dim.results, x.cat.results, parameters, 
              procedure = "MAMBAC")
  }
  if (parameters$MAXEIG) {
    PlotPanel(x.results, x.dim.results, x.cat.results, parameters, 
              procedure = "MAXEIG")
  }
  if (parameters$LMode) {
    PlotPanel(x.results, x.dim.results, x.cat.results, parameters, 
              procedure = "LMode")
  }
  if (parameters$MAXSLOPE) {
    PlotPanel(x.results, x.dim.results, x.cat.results, parameters, 
              procedure = "MAXSLOPE")
  }
  if (parameters$graph == 2) {
    cat("\nSaving graph to .jpeg file\n")
  	dev.off()
  }
  if (parameters$graph == 3) {
    cat("\nSaving graph to .tiff file\n")
    dev.off()
  }
}
