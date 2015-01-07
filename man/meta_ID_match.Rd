\name{meta_ID_match}
\alias{meta_ID_match}
\title{ This function determines which participant names match between the meta and otu.counts/otu.normed data frames.}
\description{This function determines which participant names match between the meta and otu.counts/otu.normed data frames.}
\usage{
meta_ID_match(data, otu)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{metadata or any other data frame with a participant ID column}
  \item{otu}{otu.normed, otu.counts or any other data frame with a participant ID column}
}
}
\details{All inputs must have a study_id column}
\value{
The output is a list of 'study_id's that match between the two inputs
}
\references{
no references}
\author{
atomczik}
\note{
no further notes}


\seealso{meta_ID_error}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, otu) 
{
    data.match = subset(data, !(is.na(pmatch(data$study_id, names(otu)))))
    return(data.match)
  }
}

\keyword{ match }
\keyword{ ID }
