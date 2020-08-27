\name{CreateSample}
\alias{CreateSample}
\title{
Creates sample of data
}
\description{
Generates sample of correlated data with univariate g-and-h distributions.
}
\usage{
CreateSample(n, k, r, g, h, uniform)
}
\arguments{
  \item{n}{
Sample size
}
  \item{k}{
Number of variables
}
  \item{r}{
Correlation among variables 
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
Sample of data
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
