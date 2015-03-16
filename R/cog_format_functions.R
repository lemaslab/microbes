#####################################################################################################
#####################################         WGS Analysis           ################################
#####################################################################################################

# COG Pathway File (need to change name in Data directory so names match: currently COG_Pathway_File)
# also need to identify if there are R packages or other resources we can use as pathway key.


##Next steps:
## Step1: add a column to twin_wgs_L4_cog called core_variable (which says whether the COG in 100% of the participants or not)
## Step2: add a column to twin_wgs_L4_cog called RA (which is the average RA)
## Step3: bar graph graph x=cog, y=RA, group = RA)

############################
####  cog.path.ref()   #####
############################

cog.path.ref <- function(pathway.file) {

# Create index for loops
index=names(pathway.file);index; 
myIndex<-length(index) 
## start the loop
subsets <- "blank"
subsets <- list(subsets)
for (i in 1:myIndex){
  # Create column index
  col=index[i];col
  cog.name=pathway.file[[i]] 
  n <- length(cog.name)
  pathway.name <- names(pathway.file)[i]
  pathway.name[1:n] <- pathway.name  
  
  subsets[[i]] <- as.data.frame(cbind( cog.name, pathway.name))  
}

cog.path <- ldply(subsets , data.frame) ##needs package plyr
cog.path$cog.name <- as.character(cog.path$cog.name)

## order the cog.path file by cog.name
cog.path <- cog.path[order(cog.path$cog.name), ]

return(cog.path)}



############################
####     cog.cv.ra      #####
############################

cog_cv_ra <- function(df) {
  
df<- normalize_counts(df)
n <- length(row.names(df)) ## of cogs
m <- length(df) #of participants
k <- m+1 ## which column the cog_variable is going to be
for (i in 1:n) {df[i,] <- as.numeric(df[i,])}


## Step1: add a column to twin_wgs_L4_cog called core_variable (which says whether the COG in 100% of the participants)
df$core_variable <- "blank"
for (i in 1:n)
{df$core_variable[i] <- ((sum((df[i,]) >=0.0000001))-1)}
for (i in 1:n){ if (df[i,]$core_variable>=m ){df[i,]$core_variable <- "core"}else{df[i,]$core_variable <- "variable"} }

## Step2: add a column to twin_wgs_L4_cog called RA (which is the average RA)
df$RA_mean <- "blank"
for (i in 1:n) {df$RA_mean[i] <- mean(as.numeric(df[i,1:m])  )    }

## Add sd column
df$RA_sd <- "blank"
for (i in 1:n) {df$RA_sd[i] <- sd(as.numeric(df[i,1:m])  )    }



cog.cv.ra <- as.data.frame (cbind(row.names(df), df$core_variable, df$RA_mean, df$RA_sd))
names(cog.cv.ra ) <- c("cog.name", "core_variable","RA_mean", "RA_sd")
cog.cv.ra$RA_mean <- as.numeric (  cog.cv.ra$RA_mean   )
cog.cv.ra$RA_sd <- as.numeric (  cog.cv.ra$RA_sd   )


return(cog.cv.ra) }

############################
####     Together      #####
############################

#cog.path.cv.ra <- merge(cog.path,cog.cv.ra ,  by="cog.name")
#cog.path.cv.ra <- cog.path.cv.ra[order(cog.path.cv.ra$pathway.name), ]

wgs_function <- function(df, pathway.file){
  cog.path <- cog.path.ref(pathway.file) 
  cog.cv.ra <- cog_cv_ra(df)
  cog.path.cv.ra <- merge(cog.path, cog.cv.ra ,  by="cog.name")
  cog.path.cv.ra <- cog.path.cv.ra[order(cog.path.cv.ra$pathway.name), ]

return(cog.path.cv.ra)}