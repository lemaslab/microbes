\name{prevalence_crop}
\alias{prevalence_crop}
\title{
[text]}
\description{
[text]}
\usage{
prevalence_crop(df, x)
}
\arguments{
  \item{df}{
[text]}
  \item{x}{
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
[text]}


\seealso{
[text]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

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

