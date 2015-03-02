
#'
#' @param df is otu.counts
#' @keywords transpose
#' @export
#' @examples
#' transpose_counts()

# otu.counts=otu.phylum
 

## Updated 28Jan15

#library(vegan)

# **************************************************************************** #
# ***************              START FUNCTION                  *************** #
# **************************************************************************** #
transpose_counts <- function(otu.counts) {
  
  ## flips otu.normed
  otu.counts.t = setNames(data.frame(t(otu.counts[,])), row.names(otu.counts))
  ## make the study_id a column
  otu.counts.t$library_id <-row.names(otu.counts.t)
  ## move study_id to row.names
  row.names(otu.counts.t) <- otu.counts.t$library_id
  otu.counts.new <- otu.counts.t[,-1]
  A=moveme(otu.counts.new,"first")
  
return(output)}


