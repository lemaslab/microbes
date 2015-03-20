\name{dissim_vector}
\alias{dissim_vector}
\title{Morisita-Horne Similarity Combination Function}

\description{This function calculates the Morisita-Horne similarity between samples that meet the given requirements.}
\usage{dissim_vector(otu.normed, meta, same, different)}

\arguments{
  \item{otu.normed}{ df.ra with study_id  and OTU names as the column names and row names respectively.}
  \item{meta}{ Metadata data frame with variable names in the column name position.}
  \item{same}{ A vector containing the names of the metadata columns that are to be the same.}
  \item{different}{A vector containing the names of the metadata columns that are to be different.}}

\details{The out.normed entry should be subset to only look that the relevant participants before being input into the function. For example, if the desired outcome is the similarity between women of the same age of different race, then the initial data frame needs to be subset to include only women, same=“race” and different=“race”. The output is a numeric vector. }

\value{The returned output is a vector containing the Morisita-Horne Simmilarity between all sample pairs that meet the above restrictions.}

\references{http://www.ncbi.nlm.nih.gov/pubmed/19043404}

\author{atomczik}

\examples{

## The function is currently defined as
function (otu.normed, meta, same, different) 
{
    w <- length(row.names(meta))
    fake.same <- "fake"
    fake.same[1:w] <- "same"
    fake.different <- "fake"
    fake.different[1:w] <- 1:w
    meta.f <- cbind(meta, fake.same, fake.different)
    same <- c(same, "fake.same")
    different <- c(different, "fake.different")
    otu.normed.t = setNames(data.frame(t(otu.normed[, ])), row.names(otu.normed))
    otu.normed.t$study_id <- row.names(otu.normed.t)
    merged.data <- merge(meta.f, otu.normed.t, by = "study_id")
    row.names(merged.data) <- merged.data$study_id
    n <- length(row.names(merged.data))
    m <- length(meta.f)
    p <- length(merged.data)
    q <- m + 1
    s.n <- length(same)
    d.n <- length(different)
    out <- "blank"
    out[1:n] <- "blank"
    for (i in 1:n) {
        sample.itself <- merged.data[i, q:p]
        df <- merged.data
        for (j in 1:s.n) {
            s = which(names(df) == same[j])
            idx = which(df[, s] == merged.data[i, s])
            df = df[idx, ]
        }
        for (k in 1:d.n) {
            d = which(names(df) == different[k])
            idx = which(!(df[, d] == merged.data[i, d]))
            df = df[idx, ]
        }
        df.subset <- df[, q:p]
        df.subset <- rbind(sample.itself, df.subset)
        dissim <- "blank"
        r <- length(row.names(df.subset))
        for (l in 1:r) {
            dissim[l] <- morisita.horn(sample.itself, df.subset[l, 
                ])
        }
        dissim <- dissim[-1]
        out[i] <- list(dissim)
        dissim.vector <- unlist(out)
        dissim.vector <- suppressWarnings(as.numeric(dissim.vector))
        dissim.vector <- as.numeric(sort(dissim.vector))
        dissim.vector <- as.numeric(dissim.vector[c(TRUE, FALSE)])
    }
    return(dissim.vector)
  }
}

\keyword{ similarity}
\keyword{ Morisita }
