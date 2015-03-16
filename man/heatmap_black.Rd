\name{heatmap_black}
\alias{heatmap_black}
\title{Geom_tile Graph}
\description{This function returns a geom_tile graph with color indicating positive/negative and shading indicating the magnitude of the value.}
\usage{heatmap_black(df, xvar, yvar, zvar, pvar, p)}
\arguments{
  \item{df}{ A dataframe containing the information to be graphed with variable names in the column names.}
  \item{xvar}{Column name of variable to graph on the x axis.}
  \item{yvar}{Column name of variable to graph on the y axis.}
  \item{zvar}{Column name of variable that defines the shading}
  \item{pvar}{Column name of the p.value column.}
  \item{p}{The maximum p.value for one of the yvar variables to be colored on the graph. All other yvar varibles will occur white in the graph.}}
\details{no additional details}
\value{the output is a tile-graph with zvar defining the magnitude/direction of the tile colors.}
\references{ no additional references}
\author{atomczik}
\note{ no further notes}


\seealso{ no additional code}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (df, xvar, yvar, zvar, pvar, p) 
{
    df <- df[, c(xvar, yvar, zvar, pvar)]
    names(df) <- c("xvar", "yvar", "value", "p.value")
    subset <- as.data.frame(subset(df, df$p.value <= p))
    neg <- subset(subset, subset$value <= 0)
    neg$value <- abs(neg$value)
    pos <- subset(subset, subset$value > 0)
    heat <- ggplot(df, aes(xvar, yvar)) + geom_tile(data = neg, 
        fill = "red", alpha = neg$value) + geom_tile(data = pos, 
        fill = "blue", alpha = pos$value) + geom_tile(data = subset(df, 
        df$p.value > p), aes(colour = "insignificant"), linetype = 0, 
        fill = "white") + xlab(xvar) + ylab(yvar) + theme_bw()
    return(heat)
  }
}

\keyword{ heatmap }
\keyword{ black }