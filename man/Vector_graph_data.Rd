\name{Vector_graph_data}
\alias{Vector_graph_data}
\title{ Compile Vectors into a graphable format}
\description{ This function takes a list of vectors and compiles them into a dataframe to use with ggpolt2}
\usage{Vector_graph_data(vectors)}
\arguments{
  \item{vectors}{List of vector names already in Global Environment}
}
\details{[TEXT]}
\value{[TEXT]}
\references{[TEXT]}
\author{atomczik}
\note{[TEXT]}


\seealso{[TEXT]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (vectors) 
{
    name <- vectors
    mean <- "blank"
    sd <- "blank"
    for (i in 1:length(name)) {
        mean[i] <- round(mean(get(name[i])), digits = 4)
        sd[i] <- round(sd(get(name[i])), digits = 4)
    }
    fig.1.data <- as.data.frame(cbind(name, mean, sd))
    fig.1.data$name <- with(fig.1.data, factor(name, levels = (unique(name))))
    fig.1.data$mean <- as.numeric(as.character(fig.1.data$mean))
    return(fig.1.data)
  }
}

\keyword{[TEXT]}
