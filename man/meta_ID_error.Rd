\name{meta_ID_error}
\alias{meta_ID_error}
\title{ This function determines which participant names do not match between the meta and otu.counts/otu.normed data frames.}
\description{This function determines which participant names do not match between the meta and otu.counts/otu.normed data frames.}
\usage{
meta_ID_error(data, otu)
}
\arguments{
  \item{data}{metadata or any other data frame with a participant ID column}

  \item{otu}{
otu.normed, otu.counts or any other data frame with a participant ID column}
}

\details{All inputs must have a study_id column}
\value{
The output is a list of 'study_id's that do not match between the two inputs
}

\author{
atomczik}



\seealso{
meta_ID_match}
\examples{


## The function is currently defined as
function (data, otu) 
{
    data.error = subset(data, (is.na(pmatch(data$study_id, names(otu)))))
    return(data.error)
  }
}

\keyword{ match }
\keyword{ ID }
