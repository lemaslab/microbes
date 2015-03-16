#' Condense COG/KEGG counts to metabolic pathways.
#' 
#' This function will collapses COG/KEGG counts into metabolic pathways.
#' 
#' @param wgs_gene_count data frame with COG/KEGG counts
#' @param pathway.file array with pathways composed of COG/KEGG annotations
#' @keywords pathway
#' @export
#' @examples
#' # Simulated data 
#'   head(df.wgs.counts)
#'   cogpathway=path.file
#'   df.wgs.pathway.counts=category2pathway(df.wgs.counts,cogpathway)
#'   
#' # Mouse data
#'   head(mouse.cog)
#'   cogpathway=category2pathway.cog
#'   mouse.cog=category2pathway(mouse.cog,cogpathway)
category2pathway <- function(wgs_gene_count, pathway.file) {
        # select participant data
          participant.gene.count=wgs_gene_count
          row.names(participant.gene.count)
        # Empty data.frame for pathway counts(rows=pathways and column=participants)
          participant.names=colnames(wgs_gene_count) # Could also come from Metadata
          new.df.names=c("pathway",colnames(wgs_gene_count)) # Could also come from Metadata
part.numer=length(participant.names)+1
PATH.TABLE=as.data.frame(setNames(replicate(part.numer,numeric(0), simplify = F), new.df.names))
# Loop through pathways and collapse gene counts into pathways within each participant  
# Create index for loops
index=names(pathway.file);index; 
myIndex<-length(index) 
# Start the Loop
for (i in 1:(myIndex))
{
  # Create column index
  col=index[i];col
  pathway.annot=pathway.file[[i]]
  path.annot.match=row.names(participant.gene.count)%in%pathway.annot
  which.row=which(row.names(participant.gene.count)%in%pathway.annot)
  part.number=length(participant.gene.count)
  path.gene.counts=participant.gene.count[which.row,]  
  path.count=apply(path.gene.counts, 2, sum) 
  PATH.TABLE[i,]=c(col,path.count)  
} # End of loop 


############################ Added by Angela 05Mar15 ############################
## Make pathway names the rownames (important for normalize.counts)
row.names(PATH.TABLE) <- PATH.TABLE$pathway
## Delete rid of pathway column
PATH.TABLE <- PATH.TABLE[,-1]
n <- (length(PATH.TABLE))
for (i in 1:n )  {PATH.TABLE[,i] <- as.numeric(PATH.TABLE[,i])}
#################################################################################



return(PATH.TABLE)



} # End of function
