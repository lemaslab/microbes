\name{abund_stack_graph}
\alias{abund_stack_graph}
\title{Relative Abundance Stacked Bar Graph}
\description{This function is used to graph otu.normed for a two group comparison.}
\usage{
abund_stack_graph(df2)
}
\arguments{
  \item{df2}{[TEXT]}
}
\details{[TEXT]}
\value{[TEXT]
}
\references{[TEXT]}
\author{[TEXT]}
\note{[TEXT]}


\seealso{[TEXT]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df2) 
{
    df2[, 4] <- -df2[, 4]
    df <- as.data.frame(x)
    df$OTU_name <- with(df, factor(OTU_name, levels = sort(unique(OTU_name))))
    df.graph <- ggplot() + geom_bar(data = df, aes(x = as.factor(OTU_name), 
        y = (df[, 2]), fill = names(df)[2]), stat = "identity") + 
        geom_bar(data = df, aes(x = as.factor(OTU_name), y = (df[, 
            4]), fill = names(df)[4]), stat = "identity") + geom_hline(yintercept = 0) + 
        coord_flip() + scale_y_continuous(labels = abs) + xlab("") + 
        ylab("") + theme(axis.text.x = element_blank()) + theme(axis.text.y = element_blank()) + 
        scale_fill_manual(values = c("navyblue", "orange2"))
    return(df.graph)
  }
}

\keyword{[TEXT]}
