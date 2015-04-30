\name{normalize_counts}
\alias{normalize_counts}
\title{This function normalizes the raw otu counts by participant to control for variation in sample size and concentration}
\description{normalize_counts totals the amount of bugs for each particpant and divides each otu by that toal}
\usage{
normalize_counts(otu_count)
}
\arguments{
  \item{otu_count}{A data frame including the raw counts for each bug and participant.}
}
\details{no further details}
\value{
The output is a data frame in the same format as the input.
}
\author{atomczik}



\seealso{abundance_crop}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (otu_count) 
{
    otu.normed = otu_count
    index = names(otu.normed)
    myIndex <- length(index)
    n <- length(row.names(otu.normed))
    m <- length(otu.normed)
    for (i in 1:m) {
        col = index[i]
        n <- length(row.names(otu.normed))
        otu.normed[1:n, i] <- (otu.normed[1:n, i])/sum(otu.normed[1:n, 
            i])
    }
    return(otu.normed)
  }
}

\keyword{ normalize_counts }
