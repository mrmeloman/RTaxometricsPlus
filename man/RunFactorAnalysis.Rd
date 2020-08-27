\name{RunFactorAnalysis}
\alias{RunFactorAnalysis}
\title{
Performs factor analysis
}
\description{
This function performs factor analysis
}
\usage{
RunFactorAnalysis(x, cor.matrix = FALSE, n.factors = 0, max.iter = 50, criterion = 0.01)
}
\arguments{
  \item{x}{
The data or correlation matrix
}
  \item{cor.matrix}{
Whether x is a correlation matrix. The default is FALSE.
}
  \item{n.factors}{
The number of factors to use. The default value is 0.
}
  \item{max.iter}{
The maximum number of iterations. The default value is 50.
}
  \item{criterion}{
Acceptably small change in h2 between interations. The default value is .01.
}
}
\details{
Called by higher-order functions; users do not need to call this 
function directly.
}
\value{
List of factor loadings and number of factors
\item{loadings }{The factor loadings}
\item{factors }{The number of factors}
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}