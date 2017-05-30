\name{RunProcedures}
\alias{RunProcedures}
\title{
Runs taxometric procedures for empirical data
}
\description{
This function runs the MAMBAC, MAXEIG, L-Mode, and MAXSLOPE analyses
for empirical data
}
\usage{
RunProcedures(x, parameters)
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
A list of curve-level data for each procedure performed: 
\item{MAMBAC }{MAMBAC curve}
\item{MAXEIG.x }{X values of MAXEIG curve}
\item{MAXEIG.y }{Y values of MAXEIG curve}
\item{LMode.x }{X values of LMode curve}
\item{LMode.y }{Y values of LMode curve}
\item{MAXSLOPE.x }{X values of MAXSLOPE curve}
\item{MAXSLOPE.y }{Y values of MAXSLOPE curve}
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}