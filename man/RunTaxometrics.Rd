\name{RunTaxometrics}
\alias{RunTaxometrics}
\title{
Taxometric analysis for a sample of data
}
\description{
Performs taxometric analysis for a sample of data and provides text (analytic
specifications, CCFI values, base rate estimates) and graphical (panels of 
empirical data curves superimposed above comparison data curves) output. By
default, the function will run MAMBAC, MAXEIG, and L-Mode, unless only 2 
variables are provided, in which case the program will run MAMBAC and MAXSLOPE.
}
\usage{
RunTaxometrics(x, seed = 1, n.pop = 1e+05, n.samples = 100, reps = 1, 
MAMBAC = TRUE, assign.MAMBAC = 1, n.cuts = 50, n.end = 25, MAXEIG =
TRUE, assign.MAXEIG = 1, windows = 50, overlap = 0.9, LMode = TRUE, mode.l = 
-0.001, mode.r = 0.001, MAXSLOPE = FALSE, graph = 1, text.file = FALSE)
}
\arguments{
  \item{x}{
The supplied data matrix. Cases missing any data will be removed prior to 
analysis.
}
  \item{seed}{
Random number seed provided prior to analysis of empirical data as well as
prior to generating each population of comparison data. The default value is 1.
}
  \item{n.pop}{
Size of the finite populations of categorical and dimensional comparison data.
The default value is 100,000. 
}
  \item{n.samples}{
Number of comparison data sets of each structure to generate and analyze. The
default value is 100.
}
  \item{reps}{
Number of times to resort cases along the input indicator at random and redo
the calculations (if tied scores are found), averaging to obtain final 
results.The default value is 1 if no tied scores are found, and 10 if tied 
scores are found.
}
  \item{MAMBAC}{
Whether the MAMBAC procedure is performed. The default value is TRUE.
}
  \item{assign.MAMBAC}{
How variables are assigned as input and output variables in the MAMBAC 
procedure. Variables may be used in all possible input-output pairings
(assing.MAMBAC = 1), or variables may be summed to form the input variable
(assign.MAMBAC = 2). The default value is 1.
}
  \item{n.cuts}{
The total number of cuts to make along the input variable when performing 
the MAMBAC procedure. The default value is 25.
}
  \item{n.end}{
The number of cases to set aside at each extreme along the input variable 
before making the first and last cuts when performing the MAMBAC procedure.
The default value is 25.
}
  \item{MAXEIG}{
Whether the MAXEIG procedure is performed. The default value is TRUE if 
k is >= 3, and FALSE if k < 3.
}
  \item{assign.MAXEIG}{
How variables are assigned as input and output variables in the MAXEIG 
procedure. Variables may be used in all input-output triplets 
(assign.MAXEIG = 1), each variable may serve as input once (assign.MAXEIG 
= 2), or variables may be summed to form the input (assign.MAXEIG = 3).
The default value is 1.
}
  \item{windows}{
The nubmer of overlapping windows to use when performing the MAXEIG
procedure. The default value is 50.
}
  \item{overlap}{
The amount of overlap between windows when performing the MAXEIG
procedure. The default value is .90.
}
  \item{LMode}{
Whether the L-Mode procedure is performed. The default value is TRUE
if k is >= 3, and FALSE if k < 3. 
}
  \item{mode.l}{
Position beyond which to serach for the left mode when performing the
L-Mode procedure. The default value is -.001.
}
  \item{mode.r}{
Position beyond which to serach for the right mode when performing the
L-Mode procedure. The default value is .001.
}
  \item{MAXSLOPE}{
Whether the MAXSLOPE procedure is performed. The default value is FALSE
if k >= 3, and TRUE if k < 3.
}
  \item{graph}{
Whether to display graphs on screen (1), save as a compressed .jpeg file (2), 
or save as a high-resolution .tiff file (3). The default value is 1.
}
  \item{text.file}{
Whether to divert text output to a .txt file (T/F). 
The default value is FALSE. 
}
}
\details{
This function should be called directly by users who wish to perform 
taxometric analyses for a sample of data.
}
\value{
This program returns CCFI values, and provides text and graphical output.
Note that any CCFI values of 0 represent missing values, as analyses 
will never yield a CCFI of 0. 
}
\author{
John Ruscio <ruscio@tcnj.edu> and Shirley Wang <shirleywang@g.harvard.edu>
Maintainer: John Ruscio <ruscio@tcnj.edu>
}