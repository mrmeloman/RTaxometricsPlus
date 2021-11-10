DisplayProfiles <-
function(CCFIs, parameters) {
  # 
  # Plots CCFI profiles.
  #
  # MODIFIED by Mikhail Reshetnikov 06.10.2021
  #
  # Changelog:
  # 1. Replaced all base R plot functions with ggplot functions
  #
  # 2. Placed plot parameters at the top of the script
  # ("Set parameters for the plot elements" section)
  #
  # 3. Added comments at some places
  #
  # Args:
  #        CCFIs: CCFI values across base rates and procedures (matrix).
  #   parameters: Data and program parameters (list).
  #
  # Returns:
  #   Nothing; graphical output.
  
  #Set parameters for the plot elements----
  #Overall plot parameters
  plot.title <- "CCFI Profile"
  x.label <- "Taxon base rates"
  y.label <- "CCFI"
  axis.ticks.size <- 24
  axis.names.size <- 34
  legend.labels.size <- 14
  
  plot.theme <- ggplot2::theme_classic() +
    ggplot2::theme(plot.title = ggplot2::element_text(
      hjust = 0.5,
      size = 44),
      axis.text.x = ggplot2::element_text(size = axis.ticks.size,
                                          color = "#000000"),
      axis.text.y = ggplot2::element_text(size = axis.ticks.size,
                                          color = "#000000"),
      axis.title.x = ggplot2::element_text(size = axis.names.size),
      axis.title.y = ggplot2::element_text(size = axis.names.size))
  
  #Horizontal line at the center of y axis:
  hline.size <- 1
  hline.color <- "#000000"
  hline.alpha <- 1
  hline.linetype <- "dashed"
  
  #Rectangle around the horizontal line
  #Representing ambiguous values (.45 and .55)
  amb.ymin <- 0.45
  amb.ymax <- 0.55
  amb.color <- "#f2f2f2"
  
  #Grid lines
  grid.lines.size <- 1
  grid.lines.linetype <- "solid"
  grid.lines.color <- "#d6d6d6"
  
  #MAMBAC data lines and dots
  #(by default affects all data lines and dots, not just MAMBAC)
  mambac.line.size <- 2
  mambac.line.color <- "#EC8180"
  mambac.linetype <- "dotted"
  mambac.line.alpha <- 1
  
  mambac.dots.size <- 3
  mambac.dots.color <- mambac.line.color
  mambac.dots.shape <- 0
  mambac.dots.alpha <- mambac.line.alpha
  
  #MAXEIG data lines and dots
  maxeig.line.size <- mambac.line.size
  maxeig.line.color <- "#66bd67"
  maxeig.linetype <- mambac.linetype
  maxeig.line.alpha <- mambac.line.alpha
  
  maxeig.dots.size <- mambac.dots.size + 1
  maxeig.dots.color <- maxeig.line.color
  maxeig.dots.shape <- 5
  maxeig.dots.alpha <- maxeig.line.alpha
  
  #L-Mode data lines and dots
  lmode.line.size <- mambac.line.size
  lmode.line.color <- "#8180EC"
  lmode.linetype <- mambac.linetype
  lmode.line.alpha <- mambac.line.alpha
  
  lmode.dots.size <- mambac.dots.size
  lmode.dots.color <- lmode.line.color
  lmode.dots.shape <- 2
  lmode.dots.alpha <- lmode.line.alpha
  
  #MAXSLOPE data lines and dots
  maxslope.line.size <- mambac.line.size
  maxslope.line.color <- "#57C4E5"
  maxslope.linetype <- mambac.linetype
  maxslope.line.alpha <- mambac.line.alpha
  
  maxslope.dots.size <- mambac.dots.size
  maxslope.dots.color <- maxslope.line.color
  maxslope.dots.shape <- 8
  maxslope.dots.alpha <- maxslope.line.alpha
  
  #Mean CCFI data lines and dots
  mCCFI.line.size <- mambac.line.size
  mCCFI.line.color <- "#000000"
  mCCFI.linetype <- "solid"
  mCCFI.line.alpha <- mambac.line.alpha
  
  mCCFI.dots.size <- mambac.dots.size + 1
  mCCFI.dots.color <- mCCFI.line.color
  mCCFI.dots.shape <- 16
  mCCFI.dots.alpha <- mCCFI.line.alpha

  #Create a dataframe with base rates----
  xs <- seq(parameters$min.p, parameters$max.p, length = parameters$num.p)
  plot.data <- data.frame(base.rates = xs, mean.CCFI = CCFIs[5, ])
  #Add CCFI values for every method used in the plot.data dataframe----
  if (parameters$MAMBAC) {
    plot.data$MAMBAC <- CCFIs[1, ]
  }
  if (parameters$MAXEIG) {
    plot.data$MAXEIG <- CCFIs[2, ]
  }
  if (parameters$LMode) {
    plot.data$LMode <- CCFIs[3, ]
  }
  if (parameters$MAXSLOPE) {
    plot.data$MAXSLOPE <- CCFIs[4, ]
  }
  
  #Save a plot template to the variable----
  CCFI.plot <- ggplot2::ggplot(data = plot.data,
                               ggplot2::aes(x = base.rates)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf,
                                    xmax = Inf,
                                    ymin = amb.ymin,
                                    ymax = amb.ymax),
                       fill = amb.color,
                       color = NA) +
    ggplot2::geom_vline(xintercept = seq(0.1, 0.9, 0.1),
                        size = grid.lines.size,
                        linetype = grid.lines.linetype,
                        color = grid.lines.color) +
    ggplot2::geom_hline(yintercept = 0.5,
               size = hline.size,
               linetype = hline.linetype,
               color = hline.color,
               alpha = hline.alpha) +
    ggplot2::geom_hline(yintercept = -0.01,
                        size = 2.5,
                        linetype = "solid",
                        color = "#000000",
                        alpha = 1) +
    ggplot2::geom_vline(xintercept = 0,
                        size = 2.5,
                        linetype = "solid",
                        color = "#000000",
                        alpha = 1) +
    ggplot2::scale_x_continuous(
      name = x.label,
      breaks = seq(0, 1, by = 0.1),
      limits = c(0, 1.02),
      expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      name = y.label,
      breaks = seq(0, 1, by = 0.1),
      limits = c(-0.01, 1),
      expand = c(0, 0)) +
    ggplot2::ggtitle(plot.title) +
    ggplot2::annotate(geom="text",
                      x = 0.90,
                      y = 0.95,
                      label="MAMBAC",
                      color = mambac.line.color,
                      size = legend.labels.size) +
    ggplot2::annotate(geom="text",
                      x = 0.90,
                      y = 0.9,
                      label="MAXEIG",
                      color = maxeig.line.color,
                      size = legend.labels.size) +
    ggplot2::annotate(geom="text",
                      x = 0.90,
                      y = 0.85,
                      label="L-Mode",
                      color = lmode.line.color,
                      size = legend.labels.size) +
    ggplot2::annotate(geom="text",
                      x = 0.90,
                      y = 0.80,
                      label="Mean",
                      color = mCCFI.line.color,
                      size = legend.labels.size) +
    plot.theme
  
  #Add geoms for every method used----
  if (parameters$MAMBAC) {
    CCFI.plot <- CCFI.plot +
      ggplot2::geom_path(y = plot.data$MAMBAC,
                size = mambac.line.size,
                color = mambac.line.color,
                linetype = mambac.linetype,
                alpha = mambac.line.alpha) +
      ggplot2::geom_point(x = plot.data$base.rates,
                 y = plot.data$MAMBAC,
                 size = mambac.dots.size,
                 color = mambac.dots.color,
                 shape = mambac.dots.shape,
                 alpha = mambac.dots.alpha)
      
  }
  if (parameters$MAXEIG) {
    CCFI.plot <- CCFI.plot +
      ggplot2::geom_path(y = plot.data$MAXEIG,
                size = maxeig.line.size,
                color = maxeig.line.color,
                linetype = maxeig.linetype,
                alpha = maxeig.line.alpha) +
      ggplot2::geom_point(x = plot.data$base.rates,
                 y = plot.data$MAXEIG,
                 size = maxeig.dots.size,
                 color = maxeig.dots.color,
                 shape = maxeig.dots.shape,
                 alpha = maxeig.dots.alpha)
      
  }
  if (parameters$LMode) {
    CCFI.plot <- CCFI.plot +
      ggplot2::geom_path(y = plot.data$LMode,
                size = lmode.line.size,
                color = lmode.line.color,
                linetype = lmode.linetype,
                alpha = lmode.line.alpha) +
      ggplot2::geom_point(x = plot.data$base.rates,
                 y = plot.data$LMode,
                 size = lmode.dots.size,
                 color = lmode.dots.color,
                 shape = lmode.dots.shape,
                 alpha = lmode.dots.alpha)
      
  }
  if (parameters$MAXSLOPE) {
    CCFI.plot <- CCFI.plot +
      ggplot2::geom_path(y = plot.data$MAXSLOPE,
                size = maxslope.line.size,
                color = maxslope.line.color,
                linetype = maxslope.linetype,
                alpha = maxslope.line.alpha) +
      ggplot2::geom_point(x = plot.data$base.rates,
                 y = plot.data$MAXSLOPE,
                 size = maxslope.dots.size,
                 color = maxslope.dots.color,
                 shape = maxslope.dots.shape,
                 alpha = maxslope.dots.alpha)
      
  }
  
  #Add geoms for mean CCFI----
  CCFI.plot <- CCFI.plot +
    ggplot2::geom_path(y = plot.data$mean.CCFI,
              size = mCCFI.line.size,
              color = mCCFI.line.color,
              linetype = mCCFI.linetype,
              alpha = mCCFI.line.alpha) +
    ggplot2::geom_point(x = plot.data$base.rates,
               y = plot.data$mean.CCFI,
               size = mCCFI.dots.size,
               color = mCCFI.dots.color,
               shape = mCCFI.dots.shape,
               alpha = mCCFI.dots.alpha)
  
  #Display the plot----
  if(parameters$graph == 1){
    cat("\nPrinting a ggplot")
    print(CCFI.plot)
  }
  #Save the plot if needed
  #Try this! https://www.roelpeters.be/how-to-anti-alias-plots-in-r/
  if (parameters$graph == 2) {
    cat("\nSaving graph to .jpeg file\n")
    ggplot2::ggsave(filename = paste0("RunCCFIProfile", date()),
           plot = CCFI.plot,
           device = "jpeg")
  }
  if (parameters$graph == 3) {
    cat("\nSaving graph to .tiff file\n")
    ggplot2::ggsave(filename = paste0("RunCCFIProfile", date()),
           plot = CCFI.plot,
           device = "tiff")
  }
  if (parameters$graph == 4) {
    return(CCFI.plot)
  }
}
