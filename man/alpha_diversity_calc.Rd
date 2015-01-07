\name{alpha_diversity_calc}
\alias{alpha_diversity_calc}
\title{This function calculated the alpha diversity for each participant.}
\description{The diversity is calculated six difference ways and output in a table with each column corresponding to a participant}
\usage{
alpha_diversity_calc(otu_table, study_id, study_group)
}
\arguments{
  \item{otu_table}{otu.counts or otu.normed}
  \item{study_id}{The name of the column that corresponds to the participant IDs. }
  \item{study_group}{The name of the colum that corresponds with the study groups we are comparing}
}
\details{no further details}
\value{The output table is composed of six diversity measurements for each participant. 
}
\references{
No references}
\author{
atomczik}
\note{
no further notes}


\seealso{alpha_diversity_compare}
\examples{

## The function is currently defined as
function (otu_table, study_id, study_group) 
{
    TABLE1 <- data.frame(study_id = character(), shannon = numeric(), 
        simpson = numeric(), invsimpson = numeric(), fisherfit = numeric(), 
        diversity = numeric(), renyi = numeric(), stringsAsFactors = FALSE)
    index = as.character(study_id)
    index
    myIndex <- length(index)
    myIndex
    for (i in 1:myIndex) {
        col = index[i]
        shannon = diversity(otu_table[, col], index = "shannon", 
            MARGIN = 1, base = exp(1))
        simpson = diversity(otu_table[, col], index = "simpson", 
            MARGIN = 1, base = exp(1))
        invsimpson = diversity(otu_table[, col], index = "invsimpson", 
            MARGIN = 1, base = exp(1))
        fisherfit = fisherfit(otu_table[, col])$estimate
        diversity = diversity(otu_table[, col])
        renyi = renyi(otu_table[, col])[[11]]
        continuous_block = cbind(col, shannon, simpson, invsimpson, 
            fisherfit, diversity, renyi)
        continuous_block
        TABLE1[i, 1] = col
        TABLE1[i, 2] = shannon
        TABLE1[i, 3] = simpson
        TABLE1[i, 4] = invsimpson
        TABLE1[i, 5] = fisherfit
        TABLE1[i, 6] = diversity
        TABLE1[i, 7] = renyi
    }
    TABLE2 = setNames(data.frame(t(TABLE1[, -1])), TABLE1[, 1])
    return(TABLE2)
  }
}

\keyword{ ~diversity }
\keyword{ ~alpha}
