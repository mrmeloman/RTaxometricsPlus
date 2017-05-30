\name{PlotPanel}
\alias{PlotPanel}
\title{
Plots a panel of curves
}
\description{
This function plots a two-panel graph with results for empirical and
comparison data
}
\usage{
PlotPanel(x.results, x.dim.results, x.cat.results, parameters, procedure)
}
\arguments{
  \item{x.results}{
Empirical data results
}
  \item{x.dim.results}{
Dimensional comparison data results
}
  \item{x.cat.results}{
Categorical comparison data results
}
  \item{parameters}{
The data and program parameters
}
  \item{procedure}{
Name of taxometric procedure
}
}
\details{
Called by higher-order functions; users do not need to call this 
function directly.
}
\value{
This function returns nothing, and provides graphical output only
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
