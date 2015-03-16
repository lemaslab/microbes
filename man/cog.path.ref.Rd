\name{cog.path.ref}
\alias{cog.path.ref}
\title{ cog/pathway reference}
\description{This function makes a list of all cog/pathway combinations}
\usage{cog.path.ref(pathway.file)}
\arguments{
  \item{pathway.file}{This file contains a list vectors. The name of the vector is the pathway and the elements of the vector are the cogs found in that pathway}}
\details{No details}
\value{The output is a list of cog/pathway comnbinations}
\references{No references}
\author{atomczik}
\note{no further notes}


\seealso{ cog_cv_ra}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (pathway.file) 
{
    index = names(pathway.file)
    index
    myIndex <- length(index)
    subsets <- "blank"
    subsets <- list(subsets)
    for (i in 1:myIndex) {
        col = index[i]
        col
        cog.name = pathway.file[[i]]
        n <- length(cog.name)
        pathway.name <- names(pathway.file)[i]
        pathway.name[1:n] <- pathway.name
        subsets[[i]] <- as.data.frame(cbind(cog.name, pathway.name))
    }
    cog.path <- ldply(subsets, data.frame)
    cog.path$cog.name <- as.character(cog.path$cog.name)
    cog.path <- cog.path[order(cog.path$cog.name), ]
    return(cog.path)
  }
}

\keyword{reformat}
\keyword{cog}
