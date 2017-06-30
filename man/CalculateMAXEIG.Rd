\name{CalculateMAXEIG}
\alias{CalculateMAXEIG}
\title{
Calculates MAXEIG curve
}
\description{
This function calculates one MAXEIG curve
}
\usage{
CalculateMAXEIG(input, outputs, parameters)
}
\arguments{
  \item{input}{
Input indicator 
}
  \item{outputs}{
Output indicators
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
List object with one MAXEIG curve:
\item{curve.x}{x values}
\item{curve.y}{y values}
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
