\name{cog.path.ref}
\alias{cog.path.ref}
\title{ cog/pathway reference}
\description{Convert the cog.pathway file to a list of cog/pathway combinations}
\usage{cog.path.ref(pathway.file)}
\arguments{
  \item{pathway.file}{This file contains a list of vectors. The name of the vector is the pathway and the elements of the vector are the cogs found in that pathway}}


\details{This function simply reformats the cog.pathway file for ease of use for future analysis}


\value{The output is a data frame with each row being a unique cog/pathway combination.}
\author{atomczik}


\seealso{ cog_cv_ra}
\examples{
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
