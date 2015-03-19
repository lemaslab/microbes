
###################################################################
#######################    Test     ############################
###################################################################

#otu.normed
#meta
#same <- c("sample.Twin_Mother","sample.Family", "sample.zygosity" )
#different <- c("sample.sample_id")
#test <- MH_sim(otu.normed, meta, same, different)
#length(test)
#length(unlist(test))

###################################################################
#######################    Library     ############################
###################################################################
#install.packages("fossil")
library(fossil)

###################################################################
#######################    Iputs     ##############################
###################################################################

MH_sim <- function(otu.normed, meta, same, different){
  
###################################################################
###In case same or different are empty, we make fake values #######
###################################################################
w<- length(row.names(meta))
fake.same <- "fake"
fake.same[1:w] <- "same"
fake.different <- "fake"
fake.different[1:w] <- 1:w
meta.f <- cbind(meta, fake.same, fake.different)

same <- c(same, "fake.same")
different <- c(different, "fake.different")

###################################################################
##################    Subsetting     ##############################
###################################################################
## Subset data by what we want to look at: moms over time, babies over time, mom vs. baby
## flips otu.normed
otu.normed.t = setNames(data.frame(t(otu.normed[,])), row.names(otu.normed))
## make the study_id a column
otu.normed.t$study_id <-row.names(otu.normed.t)
## add grouping variable onto out.normed.t
merged.data <- merge( meta.f, otu.normed.t, by="study_id")
## move study_id to row.names
row.names(merged.data) <- merged.data$study_id
##merged.data <- merged.data[,-1]

#merged.data$grouping_variable is merged.data[,1]
#merged.mom=subset(merged.data, (merged.data$sample.Twin_Mother== "Mom"))
#merged.twin=subset(merged.data, (merged.data$sample.Twin_Mother== "Twin"))



###################################################################	
##Select the sample that you are looking at:
n <- length(row.names(merged.data))
m <- length(meta.f)
p <- length(merged.data)
q <- m+1 #start of otu data
s.n <- length(same)
d.n <- length(different)


###################################################################
######### Start doing calculations for each individual ############
###################################################################

out <- "blank"
out[1:n] <- "blank"

for (i in 1:n) { 
 
 
## identitify the sample that you are looking at
sample.itself <- merged.data[i, q:p]

## subset based on traits
df<- merged.data



  for(j in 1:s.n){
    s=which(names(df)==same[j]) 
    idx=which(    df[,s]  ==  merged.data[i,s] )
    df=df[idx,]   }
  for(k in 1:d.n){
    d=which(names(df)==different[k]) 
    idx= which(! (   df[,d]  ==  merged.data[i,d] ))
    df=df[idx,]   }

## crop out excess metadata
df.subset <- df[,q:p]

## Add in the sample.itelf at the beginning to delete later
## This is in case df.subset is empty.
## putting it in the beginning it will always be simil[1]
df.subset<- rbind(sample.itself, df.subset)

## calulate simil
simil <- "blank"
r <- length(row.names(df.subset))
for (l in 1:r){ simil[l] <- morisita.horn(sample.itself, df.subset[l,])}
simil <- simil[-1]
out[i]<- list(simil)
simil.vector <- unlist(out)


############ This is the line that causes all of the WARNINGS !!!!!! ###############
simil.vector <- suppressWarnings(as.numeric(simil.vector))
####################################################################################

simil.vector <- as.numeric(sort(simil.vector))
simil.vector <- as.numeric(simil.vector[c(TRUE,FALSE)])
}


return(simil.vector) }








