\name{GenerateData}
\alias{GenerateData}
\title{
Generates comparison data
}
\description{
This function generates a population of comparison data
}
\usage{
GenerateData(x, n, n.factors = 0, max.trials = 5, initial.multiplier = 1)
}
\arguments{
  \item{x}{
The data matrix
}
  \item{n}{
Size of population to create
}
  \item{n.factors}{
The number of factors used to reproduce correlations. The default value
is 0.
}
  \item{max.trials}{
Maximum number of trials. The default value is 5.
}
  \item{initial.multiplier}{
Size of multiplier to adjust target correlations. The default value
is 1.
}
}
\details{
Called by higher-order functions; users do not need to call this 
function directly.
}
\value{
Population of comparison data
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
