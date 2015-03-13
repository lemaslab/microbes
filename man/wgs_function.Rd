\name{wgs_function}
\alias{wgs_function}
\title{[TEXT]}
\description{[TEXT]}
\usage{
wgs_function(df, pathway.file)
}
\arguments{
  \item{df}{[TEXT]}
  \item{pathway.file}{[TEXT]}
}
\details{[TEXT]}
\value{[TEXT]
}
\references{[TEXT]}
\author{atomczik}
\note{[TEXT]}
\seealso{[TEXT]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, pathway.file) 
{
    cog.path <- cog.path.ref(pathway.file)
    cog.cv.ra <- cog_cv_ra(df)
    cog.path.cv.ra <- merge(cog.path, cog.cv.ra, by = "cog.name")
    cog.path.cv.ra <- cog.path.cv.ra[order(cog.path.cv.ra$pathway.name), 
        ]
    return(cog.path.cv.ra)
  }
}

\keyword{ [TEXT] }

