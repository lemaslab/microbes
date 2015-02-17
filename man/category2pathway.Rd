\name{category2pathway}
\alias{category2pathway}

\title{category2pathway}
\description{[TEXT]}
\usage{category2pathway(wgs_gene_count, pathway.file)}
\arguments{
  \item{wgs_gene_count}{[TEXT]}
  \item{pathway.file}{[TEXT]}
}
\details{[TEXT]}
\value{[TEXT]
}
\references{no references}
\author{dlemas}
\note{no further notes}

\seealso{no additional links}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (wgs_gene_count, pathway.file) 
{
    participant.gene.count = wgs_gene_count
    row.names(participant.gene.count)
    participant.names = colnames(wgs_gene_count)
    new.df.names = c("pathway", colnames(wgs_gene_count))
    part.numer = length(participant.names) + 1
    PATH.TABLE = as.data.frame(setNames(replicate(part.numer, 
        numeric(0), simplify = F), new.df.names))
    index = names(pathway.file)
    index
    myIndex <- length(index)
    for (i in 1:(myIndex)) {
        col = index[i]
        col
        pathway.annot = pathway.file[[i]]
        path.annot.match = row.names(participant.gene.count) \%in\% 
            pathway.annot
        which.row = which(row.names(participant.gene.count) \%in\% 
            pathway.annot)
        part.number = length(participant.gene.count)
        path.gene.counts = participant.gene.count[which.row, 
            ]
        path.count = apply(path.gene.counts, 2, sum)
        PATH.TABLE[i, ] = c(col, path.count)
    }
    return(PATH.TABLE)
  }
}

\keyword{ COG }
