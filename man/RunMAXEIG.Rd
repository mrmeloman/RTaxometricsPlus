\name{RunMAXEIG}
\alias{RunMAXEIG}
\title{
Performs MAXEIG
}
\description{
This function performs the MAXEIG analysis
}
\usage{
RunMAXEIG(x, parameters)
}
\arguments{
  \item{x}{
The data matrix
}
  \item{parameters}{
The data and program parameters
}
}
\details{
Called by higher-order functions; users do not need to call this 
function directly.
}
\value{
Panel of MAXEIG curves:
\item{curve.x }{X values of curve}
\item{curve.y }{Y values of curve}
}
\references{
Waller, N.G., & Meehl, P.E. (1998). Multivariate normal procedures:
Distinguishing types from continua. Thousand Oaks, CA, US: Sage
Publications, Inc.
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}