\name{meta_otu_sort}
\alias{meta_otu_sort}
\title{[text]}
\description{[text]}
\usage{meta_otu_sort(otu_table, meta_sample_id)}
\arguments{
  \item{otu_table}{[text]}
  \item{meta_sample_id}{[text]}
}
\details{[text]}
\value{[text]}
\references{[text]}
\author{[text]}
\note{[text]}


\seealso{[text]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (otu_table, meta_sample_id) 
{
    otu.sort = otu_table[, match(meta_sample_id, names(otu_table))]
    return(otu.sort)
  }
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
