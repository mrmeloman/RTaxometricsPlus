\name{ProcessProfile}
\alias{ProcessProfile}
\title{
Calculates CCFIs and base rates for CCFI profile
}
\description{
This function calculates the aggregated CCFI and base rate estimate for
one CCFI profile
}
\usage{
ProcessProfile(CCFIs, parameters)
}
\arguments{
  \item{CCFIs}{
CCFI values across base rates for a single procedure
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
List of aggregated CCFI and base rate estimate
\item{CCFI }{Aggregated CCFI}
\item{p.est }{Base rate estimate}
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
