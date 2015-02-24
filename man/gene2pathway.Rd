\name{gene2pathway}
\alias{gene2pathway}

\title{gene2pathway}
\description{[TEXT]}
\usage{gene2path=function(wgs.counts, functional.map, path.level)}
\arguments{
  \item{counts}{[TEXT]}
  \item{functional.map}{[TEXT]}
}
\details{[TEXT]}
\value{[TEXT]
}
\references{no references}
\author{dlemas}
\note{no further notes}

\seealso{no additional links}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
    path.count=apply(path.gene.counts, 2, sum) 
    PATH.TABLE[i,]=c(col,path.count)  
  } # End of loop  
  return(PATH.TABLE)
} # End of function
}

\keyword{ COG }
