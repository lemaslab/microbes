\name{cog_cv_ra}
\alias{cog_cv_ra}
\title{Cog Core/Variable and Relative Abundance}
\description{This function classifies each cog into "core" or "variable" categories. "Core" is defined as any cog that is in 100 percent of particiants. "Variable" is defined as any cog that is in less than 100 percent of participants.}
\usage{cog_cv_ra(df)}
\arguments{ \item{df}{ the input is a dataframe with the column names participants and the row names cogs.}}
\details{no additional details}
\value{The output is a data frame with three columns. The first column is the cog name, the second column classifies the cog by "core" or "variable" and the}
\references{http://www.ncbi.nlm.nih.gov/pubmed/19043404}
\author{atomczik}
\note{no further notes}


\seealso{ see also cog.path.ref}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df) 
{
    df <- normalize_counts(df)
    n <- length(row.names(df))
    m <- length(df)
    k <- m + 1
    for (i in 1:n) {
        df[i, ] <- as.numeric(df[i, ])
    }
    df$core_variable <- "blank"
    for (i in 1:n) {
        df$core_variable[i] <- ((sum((df[i, ]) >= 1e-07)) - 1)
    }
    for (i in 1:n) {
        if (df[i, ]$core_variable >= m) {
            df[i, ]$core_variable <- "core"
        }
        else {
            df[i, ]$core_variable <- "variable"
        }
    }
    df$RA_mean <- "blank"
    for (i in 1:n) {
        df$RA_mean[i] <- mean(as.numeric(df[i, 1:m]))
    }
    df$RA_sd <- "blank"
    for (i in 1:n) {
        df$RA_sd[i] <- sd(as.numeric(df[i, 1:m]))
    }
    cog.cv.ra <- as.data.frame(cbind(row.names(df), df$core_variable, 
        df$RA_mean, df$RA_sd))
    names(cog.cv.ra) <- c("cog.name", "core_variable", "RA_mean", 
        "RA_sd")
    cog.cv.ra$RA_mean <- as.numeric(cog.cv.ra$RA_mean)
    cog.cv.ra$RA_sd <- as.numeric(cog.cv.ra$RA_sd)
    return(cog.cv.ra)
  }
}

\keyword{ core }
\keyword{ variable }
