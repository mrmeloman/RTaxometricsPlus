\name{CalculateCCFI}
\alias{CalculateCCFI}
\title{
Calculates CCFIs 
}
\description{
This function calculates CCFIs for a MAMBAC, MAXEIG, or MAXSLOPE curve
}
\usage{
CalculateCCFI(curve, curve.dim, curve.cat)
}
\arguments{
  \item{curve}{
Empirical data curve
}
  \item{curve.dim}{
Average curve for dimensional comparison data
}
  \item{curve.cat}{
Average curve for categorical comparison data
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