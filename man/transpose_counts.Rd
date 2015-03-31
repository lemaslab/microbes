\name{transpose_counts}
\alias{transpose_counts}

\title{transpose_counts}
\description{[TEXT]}
\usage{transpose_counts <- function(otu.counts)}
\arguments{
  \item{counts}{[TEXT]}
  \item{functional.map}{[TEXT]}
}
\details{[TEXT]}
\value{[TEXT]
}
\references{no references}
\author{dlemas}
\note{no further notes}

\seealso{no additional links}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
transpose_counts <- function(otu.counts) {
  
  ## flips otu.normed
  otu.counts.t = setNames(data.frame(t(otu.counts[,])), row.names(otu.counts))
  ## make the study_id a column
  otu.counts.t$library_id <-row.names(otu.counts.t)
  ## move study_id to row.names
  row.names(otu.counts.t) <- otu.counts.t$library_id
  otu.counts.new <- otu.counts.t[,-1]
  last.col.no=dim(otu.counts.new)[2]
  second.last.col.no=last.col.no-1
  A=otu.counts.new[c(last.col.no,1:second.last.col.no)]
  
return(A)}
}

\keyword{transpose_counts}
