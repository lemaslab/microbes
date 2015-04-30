\name{microbes-package}
\alias{microbes-package}
\alias{microbes}
\docType{package}
\title{This package analyzes data from next generation 16S sequencing
This function is used internally by many accessors and in many functions/methods that need to accessa particular type of component data. If something is wrong, or the slot is missing, the expectedbehavior is that this function will return NULL. Thus, the output can be tested by is.null as verification of the presence of a particular data component. Unlike the component-specific accessors (e.g. otu_table, or phy_tree), the default behavior is not to stop with an error if the desired slot is empty. In all cases this is controlled by the errorIfNULL argument, which can be set to TRUE if an error is desired.

}
\description{The functions in this package compute diversity and relative abundance comparisons between two groups}
\details{
\tabular{ll}{
Package: \tab microbes\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-07\cr
License: \tab What license is it under?\cr
}
}
\author{
dlemas and atomczik
Maintainer: dlemas
}
\references{refernces will be added here}
\keyword{ package }
\keyword{ microbiome }

\seealso{ put link here for tutorial}
\examples{}
