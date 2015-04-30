
#'
#' @param df is otu.normed
#' @keywords Morisita
#' @export
#' @examples
#' distance_measure()

otu.counts=otu.phylum

# **************************************************************************** #
# ***************              START FUNCTION                  *************** #
# **************************************************************************** #
distance_measure <- function(otu.counts, library_id) {
  
  # transpose df
  A=transpose_counts(otu.counts)
  B=A[-1]
  
    
# **************************************************************************** #
# ***************      Create dissimilarity matrix             *************** #
# **************************************************************************** #
horn.df <- as.data.frame(as.matrix(vegdist(B, method="horn", 
                                               binary=FALSE, 
                                               diag=TRUE, 
                                               upper=TRUE, 
                                               na.rm = FALSE)))

# **************************************************************************** #
# ***************       All- non-self dissimilarity            *************** #
# **************************************************************************** #

all.combo=combn(as.character(library_id),2)

## Empty data.frame 
ALL.TABLE<-data.frame(col1=character(),
                      col2=character(),
                      distance=numeric(),
                      stringsAsFactors=FALSE); ALL.TABLE 

index=dim(all.combo)[2]


for (i in 1:index){
  
  grp1=all.combo[1,i]
  grp2=all.combo[2,i]
  dis.measure=horn.df[grp1,grp2]  
  
  ALL.TABLE[i,1]=grp1
  ALL.TABLE[i,2]=grp2
  ALL.TABLE[i,3]=dis.measure
  } # end of function
  return(ALL.TABLE)
}




