#' A Normalization by participant function. 
#'
#' This function normalizes the raw OTU counts by participant.
#' @param df is otu.counts
#' @keywords normalize
#' @export
#' @examples
#' ra_table()


# **************************************************************************** #
# ***************              START FUNCTION                  *************** #
# **************************************************************************** #


ra_table <- function(otu_count) {

otu.ra =otu_count
index=names(otu.ra)
myIndex<-length(index)
n <- length(row.names(otu.ra))
m <- length(otu.ra)
# Start the Loop
for (i in 1:m)
{
  # Create column index
  col=index[i]
  n <- length(row.names(otu.ra))
  otu.ra[1:n,i] <- (otu.ra[1:n,i])/sum(otu.ra[1:n,i])
}

return(otu.ra)

} # End function

