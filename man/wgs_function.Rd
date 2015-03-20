\name{wgs_function}
\alias{wgs_function}
\title{[Compiled wgs analyisis}
\description{this function reformats WGS data to prepare it for analyisis}
\usage{
wgs_function(df, pathway.file)
}
\arguments{
  \item{df}{wgs.counts or wgs.ra}
  \item{pathway.file}{A file referencing the pathway category to which each cog belongs}
}

\author{atomczik}
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

\keyword{ cog }

