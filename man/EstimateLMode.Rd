\name{EstimateLMode}
\alias{EstimateLMode}
\title{
Estimates L-Mode base rate
}
\description{
This function estimates the taxon base rate for an L-Mode curve
}
\usage{
EstimateLMode(curve.x, curve.y, parameters)
}
\arguments{
  \item{curve.x}{
X values of density
}
  \item{curve.y}{
Y values of density
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
List of base rate estimates:
\item{p.r}{Based on location of left mode}
\item{p.l}{Based on location of right mode}
\item{p.estimate}{mean}
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
