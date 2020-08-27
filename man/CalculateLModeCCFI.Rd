\name{CalculateLModeCCFI}
\alias{CalculateLModeCCFI}
\title{
Calculates L-Mode CCFI
}
\description{
This function calculates CCFI for an L-Mode curve
}
\usage{
CalculateLModeCCFI(curve.x, curve.y, curve.dim.x, curve.dim.y, curve.cat.x, curve.cat.y)
}
\arguments{
  \item{curve.x}{
Empirical data curve, x
}
  \item{curve.y}{
Empirical data curve, y
}
  \item{curve.dim.x}{
Average curve for dimensional comparison data, x
}
  \item{curve.dim.y}{
Average curve for dimensional comparison data, y
}
  \item{curve.cat.x}{
Average curve for categorical comparison data, x
}
  \item{curve.cat.y}{
Average curve for categorical comparison data, y
}
}
\details{
Called by higher-order functions; users do not need to call this 
function directly.
}
\value{
CCFI value
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}