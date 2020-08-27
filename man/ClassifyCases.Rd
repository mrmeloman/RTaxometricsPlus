\name{ClassifyCases}
\alias{ClassifyCases}
\title{
Assigns cases to groups
}
\description{
This function assigns cases to groups using the base-rate classification
technique. Cases are sorted according to their total scores on all indicators,
and the highest-scoring cases are assigned to the taxon such that the proportion
of taxon members equals the specified base rate estimate.
}
\usage{
ClassifyCases(x, p, cols = 0)
}
\arguments{
  \item{x}{
The supplied data matrix.
}
  \item{p}{
The base rate estimate that will be used to classify cases.
}
  \item{cols}{
The column numbers that contain variables 
}
}
\details{
Users should call this function directly if they wish to assign cases to 
groups. 
}
\value{
Data matrix with a new classification variable.
}
\references{
Ruscio, J. (2009). Assigning cases to groups using taxometric results: an 
empirical comparison of classification techniques. Assessment, 16(1), 55-70.
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
