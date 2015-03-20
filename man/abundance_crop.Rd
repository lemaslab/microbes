\name{abundance_crop}

\alias{abundance_crop}

\title{Subset data frame by minimum mean abundance}

\description{This function subsets df.counts to include only bugs that make up a selected amount of the microbiome. This function takes the data frame (df) and eliminates all rows that average to less that x}

\usage{abundance_crop(df, x)}

\arguments{
  \item{df}{df.normed or any data frame with all numeric entries between 0 and 1.}
  \item{x}{a number between 0 and 1}}

\details{df must have labels only in row.names and column names. All other entries must be numeric and between 0 and 1.}

\value{The output from this function is a a subset of the original data frame with consistent formatting.}

\author{atomczik}


\seealso{prevalence_crop}
\examples{

## The function is currently defined as
function (df, x) 
{
    n <- length(row.names(df))
    m <- length(df)
    k <- m + 1
    for (i in 1:n) {
        df$sum[i] <- (sum(df[i, ])/m)
    }
    df <- subset(df, !(df$sum <= x))
    df <- df[, -k]
    head(df)
    return(df)
  }
}

\keyword{ ~crop }
\keyword{ ~abundance }
\keyword{ ~subset }

