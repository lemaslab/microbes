#' Condense COG/KEGG counts to metabolic pathways.
#' 
#' This function will collapses COG/KEGG counts into metabolic pathways.
#' 
#' @param wgs.counts data frame with COG/KEGG counts
#' @param functional.map array that links COG/KEGG terms with annotated pathways
#' @keywords pathway
#' @export
#' @examples
#' # Twin data 
#'   head(df.wgs.counts)
#'   cogpathway=path.file
#'   path.test.ko2=gene2path(ko.L4.baby.2, ko.map, 2)
#'   
gene2path=function(wgs.counts, functional.map, path.level){
  
  # navigate path level
  func.map=functional.map[path.level]
  #str(func.map)
  func.names=names(func.map[[1]])
  pathway.file=func.map[[1]]
  pathway.file[[1]]
  
  # select KO terms
  participant.gene.count=wgs.counts
  row.names(participant.gene.count)
  # Empty data.frame for pathway counts(rows=pathways and column=participants)
  participant.names=colnames(wgs.counts) # Could also come from Metadata
  path.level.name=paste("pathway_L",path.level,sep="")
  new.df.names=c(path.level.name,colnames(wgs.counts)) # Could also come from Metadata
  part.numer=length(participant.names)+1
  PATH.TABLE=as.data.frame(setNames(replicate(part.numer,numeric(0), simplify = F), new.df.names))
  # Loop through pathways and collapse gene counts into pathways within each participant  
  # Create index for loops
  index=func.names;index; 
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
    path.count=as.numeric(apply(path.gene.counts, 2, sum)) 
    PATH.TABLE[i,]=c(col,path.count)  
  } # End of loop  
  
  row.names(PATH.TABLE)=as.vector(PATH.TABLE[[1]])
  path.row.names=row.names(PATH.TABLE)
  PATH.TABLE=PATH.TABLE[,-1]
  path.numeric=as.data.frame(sapply(PATH.TABLE,function(x) as.numeric(x)))
  row.names(path.numeric)=path.row.names  
  return(path.numeric)
} # End of function


