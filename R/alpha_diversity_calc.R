##' Calculate alpha diversity measures for each sample.
##' 
##' This function creates a data frame with alpha diversity estimates calculated for 
##' each sample.
##' @param otu_table data frame with 16s counts
##' @keywords alpha diversity
##' @export
##' @examples
##' # Simulated data 
##'   head( )
##'   alpha_diversity_calc(otu.species.sort)

alpha_diversity_calc <- function(otu_table) {
  #otu_table <- otu.counts 
  
  
#library("vegan")
  
# **************************************************************************** #
# ***************           Create Empty Data.Frame            *************** #
# **************************************************************************** #

  TABLE1<-data.frame(
                   study_id=character(),
                   shannon=numeric(),
                   simpson=numeric(),
                   invsimpson=numeric(),
                   fisherfit=numeric(),
                   diversity=numeric(),
                   renyi=numeric(),
                   stringsAsFactors=FALSE) 

# **************************************************************************** #
# ***************                 Loop Index                   *************** #
# **************************************************************************** #


# How many participants?

# How many participants?

index=as.character(names(otu_table));index 
myIndex<-length(names(otu_table));myIndex 

# **************************************************************************** #
# ***************                 Start Loop                   *************** #
# **************************************************************************** #
for (i in 1:myIndex)
{ 
  #Column Index
  col=index[i]
  #names(otu_table)
  
  
  #Functions
  shannon= diversity(otu_table[,col], index = "shannon", MARGIN = 1, base = exp(1))
  simpson= diversity(otu_table[,col], index = "simpson", MARGIN = 1, base = exp(1))
  invsimpson= diversity(otu_table[,col], index = "invsimpson", MARGIN = 1, base = exp(1))
  fisherfit=fisherfit(otu_table[,col])$estimate
  diversity= diversity(otu_table[,col])
  renyi=renyi(otu_table[,col])[[11]]

  # Bind variables
  continuous_block=cbind(col, shannon, simpson,invsimpson, fisherfit, diversity, renyi);continuous_block
     
  # Add to data.frame
  TABLE1[i,1]=col
  TABLE1[i,2]=shannon
  TABLE1[i,3]=simpson
  TABLE1[i,4]=invsimpson
  TABLE1[i,5]=fisherfit
  TABLE1[i,6]=diversity
  TABLE1[i,7]=renyi

  
} 

TABLE2 = setNames(data.frame(t(TABLE1[,-1])), TABLE1[,1])
return(TABLE2)
#group=as.numeric(meta_sort_group)
#table.grp=cbind(TABLE1,group)

} # End function
