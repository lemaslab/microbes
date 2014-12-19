# **************************************************************************** #
# ***************       table_group_compare.R                  *************** #
# **************************************************************************** #
#
# This function takes a vector of groups and creates an empty data.frame with 
# the following headers:
#
# outcome, all_mean, all_sd, all_mean_sd, Gr1, Gr2, Grn, df, t-stat, p-value
#
# **************************************************************************** #
# ***************                 Loop Index                   *************** #
# **************************************************************************** #

table_group_compare <- function(group_vector){

  group_vector.factor=as.factor(group_vector)
  
# Groups
groups=as.factor(as.numeric(group_vector.factor));groups
group.names=unique(as.character(group_vector.factor))

# Group Data.Frame
grouping.vector<-data.frame(group_mean=character(),
                            group_sd=character(),
                            group_mean_sd=character(),
                            stringsAsFactors=FALSE); grouping.vector 

index=group.names;index 
myIndex<-length(index);myIndex 

for (i in 1:myIndex)
{ 
  # Group
  group.n=index[i]
  # Group names
  group.mean=paste(group.n,"mean",sep="_")
  group.sd=paste(group.n,"sd",sep="_")
  group.mean.sd=paste(group.n,"mean","sd",sep="_")
  
  # populate df
  grouping.vector[i,1]=group.mean
  grouping.vector[i,2]=group.sd
  grouping.vector[i,3]=group.mean.sd  
  
}

# Take output "grouping vector" and loop through to create a single vector will all variables
# first loop
index=group.names;index 
myIndex<-length(index);myIndex 

# Create vector
x=c(as.character(grouping.vector[1,]))
for (i in 2:myIndex)
{
  newItems=c(as.character(grouping.vector[i,]))
  x=c(x,newItems)
}

# name of new group variables
table.cols.1=c("outcome","all_mean","all_sd","all_mean_sd")
table.cols.2=x 
table.cols.3=c("t.stat","df","p.value")

# Final name of data.frame columns
table.cols.final=c(table.cols.1,table.cols.2,table.cols.3)
# make lowercase
table.cols.final=tolower(table.cols.final)

# How many columns in data.frame?
df.cols=length(table.cols.final)

# Create data.frame
df = data.frame(matrix(vector(), 0, df.cols, dimnames=list(c(), c(table.cols.final))), stringsAsFactors=F)


return(df)
}

# Test function
#test_vector=c("grp1","grp2","grp3","grp4","grp1");test_vector
#A=table_group_compare(test_vector);A

