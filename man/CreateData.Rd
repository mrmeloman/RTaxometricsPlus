\name{CreateData}
\alias{CreateData}
\title{
Creates a data set
}
\description{
This function creates an artificial data set based on either dimensional or
categorical latent structure, which can vary according to a number of basic
parameters. Such data can be useful for getting to know the taxometric
programs and becoming familiar with their output by conducting analyses using
data sets whose parameters are known.
}
\usage{
CreateData(str, n = 600, k = 4, p = 0.5, d = 2, r = 0, r.tax = 0, r.comp = 0, 
skew = 0, cuts = 0, seed = 1)
}
\arguments{
  \item{str}{
The type of data to be generated. Specify either "dim" for dimensional data
or "cat" (or anything else) for categorical data.
}
  \item{n}{
Sample size. The default value is 600.
}
  \item{k}{
Number of variables. The default value is 4.
}
  \item{p}{
Taxon base rate. The default value is .5.
}
  \item{d}{
Standardized mean difference between groups. The default value is 2.
}
  \item{r}{
Correlation among variables. The default value is 0.
}
  \item{r.tax}{
Correlation among variables within the taxon. The default value is 0.
}
  \item{r.comp}{
Correlation among variables within the complement. The default value is 0.
}
  \item{skew}{
Amount of skew to be applied to variables. The default value is 0.
}
  \item{cuts}{
Number of values to use when generating ordered categorical data. The 
default value is 0.
}
  \item{seed}{
Random number seed; specifying the same seed enables users to generate and
analyze identical data sets. The default value is 1.
}
}
\details{
Users should call this function directly if they wish to create an 
artificial data set.
}
\value{
Data matrix; k columns contain data, final column contains classification.
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <wangs7@tcnj.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}
\examples{
# creates a categorical data set
test.cat <- CreateData("cat")

# creates a dimensional data set
test.dim <- CreateData("dim")
}