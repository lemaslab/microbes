\name{crop_counts}

\alias{crop_counts}

\title{Subset df.counts by metadata variables}

\description{This function subsets data frames into smaller data frames including only participants who meet selected criterion in the metadata.}

\usage{crop_counts(df, meta, variables, values)}

\arguments{
  \item{df}{ data frame with column names of the study_id's}
  \item{meta}{ metadata dataframe}
  \item{variables}{The names of the columns in metadata that the data frame is being subset based on. These variables should be listed together in one vector.}
  \item{values}{The desired values of the variables above. “values” must be the same length as “variables”, with entries in corresponding order.}
}

\details{It is important that "variables" and "values" are the same length and corresponding entries are in the same order.}
\value{
The output is a subsetted version of the original data frame containing only the participants that contain the specified values for the specified variables.}


\author{atomczik}


\examples{

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
