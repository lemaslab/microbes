\name{abundance_crop}
\alias{abundance_crop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{This function crops the data to include only bugs that make up a significant amount of the microbiome.}
\description{This function takes the data frame (df) and eliminates all rows that average to less that x}
\usage{
abundance_crop(df, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{otu.normed or any data frame with all numeric entries}
  \item{x}{a number between 0 and 1}
}
\details{df must have labels only in row.names and column names}
\value{Output is a data frame in the same form as the orginal input.
}
\references{no references}
\author{atomczik}
\note{no further notes}


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
