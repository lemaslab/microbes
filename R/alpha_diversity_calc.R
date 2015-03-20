## updated 19Mar15 atomczik
  
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
  
  
  ##These are some of the functions to choose from.
  ## Functions can be added or deleted here, but must also be added below as well.
  shannon= diversity(otu_table[,col], index = "shannon", MARGIN = 1, base = exp(1))
  simpson= diversity(otu_table[,col], index = "simpson", MARGIN = 1, base = exp(1))
  invsimpson= diversity(otu_table[,col], index = "invsimpson", MARGIN = 1, base = exp(1))
  fisherfit=fisherfit(otu_table[,col])$estimate
  diversity= diversity(otu_table[,col])
  renyi=renyi(otu_table[,col])[[11]]

  ## Bind variables
  ## This puts together the variables that are going to be in the df output.
  continuous_block=cbind(col, shannon, simpson,invsimpson, fisherfit, diversity, renyi);continuous_block
     
  ## Create a blank df for the diversity measures
  TABLE1[i,1]=col
  TABLE1[i,2]=shannon
  TABLE1[i,3]=simpson
  TABLE1[i,4]=invsimpson
  TABLE1[i,5]=fisherfit
  TABLE1[i,6]=diversity
  TABLE1[i,7]=renyi

  
} 

## Transpose the data frame to have study_id as the column names.
TABLE2 = setNames(data.frame(t(TABLE1[,-1])), TABLE1[,1])

## output
return(TABLE2)

## I'm not sure why this code was left in here, but could likely be deleted.
#group=as.numeric(meta_sort_group)
#table.grp=cbind(TABLE1,group)

} # End function
