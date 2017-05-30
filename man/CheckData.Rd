\name{CheckData}
\alias{CheckData}
\title{
Checks supplied data set
}
\description{
This function checks whether the supplied empirical data set is appropriate for 
taxometric analysis, and provides descriptive statistics about the data set.  If
data do not meet certain requirements, the program prints warnings in the output,
with details about which specific criteria are not met.
}
\usage{
CheckData(x)
}
\arguments{
  \item{x}{
The supplied data matrix. Cases missing any data will be removed prior to 
analysis.
}
}
\details{
This function should be called directly by users before performing any 
taxometric procedures.
}
\value{
This program returns nothing, and provides text output only.
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
\examples{
# create or import data set

# creates a categorical data set
test.cat <- CreateData("cat")
# Checks data
CheckData(test.cat)

# creates a dimensional data set
test.dim <- CreateData("dim")
# Checks data
CheckData(test.dim)
}