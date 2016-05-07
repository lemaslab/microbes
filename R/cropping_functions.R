#' Crop data set by prevalence adn average relative abundance of bugs
#' 
#' This function will collapses COG/KEGG counts into metabolic pathways.
#' 
#' @param df data frame with COG/KEGG counts
#' @param pathway.file array with pathways composed of COG/KEGG annotations
#' @keywords pathway
#' @export
#' @examples
#' # Simulated data 
#'   head(df.wgs.counts)
#'   cogpathway=path.file
#'   df.wgs.pathway.counts=category2pathway(df.wgs.counts,cogpathway)
# **************************************************************************** #
# ***************        prevalence_crop() Function            *************** #
# **************************************************************************** #


prevalence_crop <- function(df, x) {
  n <- length(row.names(df))
  m <- length(df)
  k <- m + 1;k2=m+2
  for (i in 1:n) {
    temp=ifelse(df[i,c(1:m)]==0,TRUE,FALSE)
    df$blank[i] <- length(which(temp==TRUE))  
    df$blank2[i]=df$blank[i]/m
  } # end function
  df <- subset(df, !(df$blank >=x))
  df <- df[,c(-k, -k2)]
  return(df)

} # End function

# **************************************************************************** #
# ***************        abundance_crop() Function            *************** #
# **************************************************************************** #
abundance_crop <- function(df, x) {
  n <- length(row.names(df))
  m <- length(df)
  k <- m+1

for (i in 1:n)
  {df$sum[i] <- (sum(df[i,])/m)}

### Cut average is less than 5%
df <- subset(df, !(df$sum<=x))
df <- df[,-k]
head(df)

return(df)

} #End of function