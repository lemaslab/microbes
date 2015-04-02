\name{crop_counts}
\alias{crop_counts}
\title{Subsetting Counts dataframes by Metadata}
\description{ This function subsets data frames into smaller dataframes including only participants who meet the critereon in the metadata.}
\usage{crop_counts(df, meta, variables, values)}
\arguments{
  \item{df}{ data frame with column names of the study_id's}
  \item{meta}{ metadata dataframe}
  \item{variables}{The names of the columns in metadata that the dataframe is being subset based on. These variables should be listed together in one vector.}
  \item{values}{The desired values of the columns above. Listed together in one vector in the order that "variables" is listed.}
}
\details{It is important that "variables" and "values" are the same length and cooresponding entries are in the same order.  }
\value{
The output is a subsetted version of the original data frame containing only the participants that contain the specified values for the specified variables.}
\references{ no references}
\author{atomczik}
\note{no further notes}
\seealso{ no additional functions}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, meta, variables, values) 
{
    df.t = setNames(data.frame(t(df[, ])), row.names(df))
    df.t$study_id <- row.names(df.t)
    merged.data <- merge(meta, df.t, by = "study_id")
    row.names(merged.data) <- merged.data$study_id
    merged.data <- merged.data[, -1]
    for (i in 1:length(variables)) {
        variable.column <- which(names(merged.data) == variables[i])
        merged.data = subset(merged.data, (merged.data[, variable.column] == 
            values[i]))
    }
    cropped.counts <- merged.data
    m <- length(meta)
    n <- length(merged.data)
    p <- m + 1
    cropped.counts = cropped.counts[, m:n]
    cropped.counts = setNames(data.frame(t(cropped.counts[, ])), 
        row.names(cropped.counts))
    return(cropped.counts)
  }
}

\keyword{ subset }
\keyword{ crop }
