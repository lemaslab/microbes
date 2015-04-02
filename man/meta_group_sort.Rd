\name{meta_group_sort}
\alias{meta_group_sort}
\title{[text]}
\description{[text]}
\usage{meta_group_sort(data, variable)}
\arguments{
  \item{data}{
[text]}
  \item{variable}{
[text]}
}
\details{
[text]}
\value{
[text]
}
\references{
[text]}
\author{
[text]}
\note{
}


\seealso{
[text]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, variable) 
{
    data.sort = data[order(variable), ]
    return(data.sort)
  }
}

\keyword{ [TEXT] }

