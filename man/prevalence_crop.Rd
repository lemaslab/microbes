\name{prevalence_crop}
\alias{prevalence_crop}
\title{ Subset dataset by minimum OTU prevalence}
\description{Subset df.counts or df.ra by a selected minimum prevalence level.}
\usage{
prevalence_crop(df, x)}
\arguments{
  \item{df}{df.counts or df.normed}
  \item{x}{Desired minimum prevalence level}}
\details{This function works best with df.ra}
\value{The output is a subset of the original data frame with the same formatting.}

\author{atomczik}


\examples{


## The function is currently defined as
function (df, x) 
{
    n <- length(row.names(df))
    m <- length(df)
    k <- m + 1
    for (i in 1:n) {
        df$blank[i] <- (sum((df[i, ]) <= 1e-07))
    }
    df <- subset(df, !(df$blank >= ((1 - x) * m)))
    df <- df[, -k]
    return(df)
  }
}

\keyword{ [text] }

