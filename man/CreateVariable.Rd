\name{CreateVariable}
\alias{CreateVariable}
\title{
Creates a variable
}
\description{
Generates variable with g-and-h distribution.
}
\usage{
CreateVariable(n, g, h, uniform)
}
\arguments{
  \item{n}{
Size of sample to create
}
  \item{g}{
Parameter used to control asymmetry (scalar); sign indicates direction 
and absolute value indicates magnitude of skew (e.g., +/- .30 yields 
substantial asymmetry).
}
  \item{h}{
Parameter used to control tail weight (scalar); positive values yield 
tails that are longer/thinner than a standard normal curve, negative 
values do the reverse (e.g., +/- .15 is a substantial departure 
from normality).
}
  \item{uniform}{
Whether to generate random values (the program default) or use 
uniformly distributed quantiles (T/F).
}
}
\details{
Called by higher-order functions; users do not need to call this 
function directly.
}
\value{
Single variable
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
