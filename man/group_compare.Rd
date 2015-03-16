\name{group_compare}
\alias{group_compare}
\title{This function compares the relative abundance for each bug by study group}
\description{[text]}
\usage{
group_compare <- function(otu.normed, meta, library_id, grouping_variable)
}
\arguments{
  \item{otu.normed}{
[text]}
  \item{meta}{
[text]}
  \item{library_id}{
[text]}
  \item{grougrouping_variablep_a}{
[text]}
}
\details{
[text]}
\value{
[text]
}
\references{
[text]}
\author{
[text]}
\note{
[text]}


\seealso{
[text]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
group_compare <- function(otu.normed, meta, library_id, grouping_variable) {
  
  # Format Variables
  library_id=as.character(library_id)
  grouping_variable=as.character(grouping_variable)
  
  # Transpose Count Table
  otu.normed.t = setNames(data.frame(t(otu.normed[,])), row.names(otu.normed))
  otu.normed.t$library_id <-row.names(otu.normed.t)
  meta.grp=as.data.frame(cbind(library_id, grouping_variable))
  merged.data <- merge( meta.grp, otu.normed.t, by="library_id")
  
  # Group names (only works for 2 groups)
  group_a=levels(merged.data$grouping_variable)[1]
  group_b=levels(merged.data$grouping_variable)[2]
  
  # Create names for colums
  col1="OTU_name"
  col2=paste(group_a,"_mean",sep="")
  col3=paste(group_a,"_sd",sep="")  
  col4=paste(group_b,"_mean",sep="")
  col5=paste(group_b,"_sd",sep="")  
  col6="p.value_t.test"
  col7="p.value_wilcoxon"    
  names <- c(col1, col2, col3, col4, col5, col6, col7)
  
  # Create index for loops
  index=names(merged.data)
  myIndex<-length(index)
  
  # Calculate mean/sd
  mean_group=as.data.frame(aggregate(merged.data[3:myIndex],by=list(merged.data$grouping_variable),FUN=mean))
  sd_group=as.data.frame(aggregate(merged.data[3:myIndex],by=list(merged.data$grouping_variable),FUN=sd))
  
  ## Empty data.frame 
  TABLE1<-data.frame(col1=character(),
                     col2=numeric(),
                     col3=numeric(),
                     col4=numeric(),
                     col5=numeric(),
                     col6=numeric(),
                     col7=numeric(),
                     stringsAsFactors=FALSE); TABLE1 
  names(TABLE1) <-names

# Start the Loop
  for (i in 3:myIndex)  {
    
    # Create column index
    col=index[i]
    
    # Pull values to populate table
    grp1.mean=round(mean_group[1,i-1],digits=3)
    grp1.sd=round(mean_group[2,i-1],digits=3)
    grp2.mean=round(sd_group[1,i-1], digits=3)
    grp2.sd=round(sd_group[2,i-1], digits=3)  
    
    # Now caluclate the t-test
    stats.t=t.test(merged.data[,col]~merged.data[,2])
        
    # Pull out the p.value-with rounding- from this object (same process for df, t-stat, ect)
    p.value_t.test=round(stats.t$p.value,digits=4)    
    
    #Wilcoxon
    stats.w = wilcox.test(merged.data[,col]~merged.data[,2]) 
    p.value_wilcoxon=round(stats.w$p.value,digits=4)
    
    
    # Bind variables- internal check
    continuous_block=cbind(col,grp1.mean,grp1.sd,grp2.mean,grp2.sd,p.value_t.test, p.value_wilcoxon);continuous_block
        
    # Now populate the TABLE1 df with these values    
    
    # Add to data.frame
    TABLE1[i,1]=col
    TABLE1[i,2]=grp1.mean
    TABLE1[i,3]=grp1.sd
    TABLE1[i,4]=grp2.mean
    TABLE1[i,5]=grp2.sd
    TABLE1[i,6]=p.value_t.test
    TABLE1[i,7]=p.value_wilcoxon
    
  } # End of Loop
  TABLE1
  TABLE2 <- TABLE1[-(1:2),]
  
  
  return(TABLE2)
<<<<<<< HEAD
} 

\keyword{ [text] }
\keyword{ [text] }
=======
} }

\keyword{TEXT}
>>>>>>> b64c95e3bf92eabe14cf435fb02ef6406bbc51b1
