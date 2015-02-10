
#' 
#'
#' 
#' @param df is otu.normed
#' @keywords Morisita
#' @export
#' @examples
#' dissim_pair_compare()

## Updated 28Jan15


#library(vegan)

# **************************************************************************** #
# ***************              START FUNCTION                  *************** #
# **************************************************************************** #
dissim_pair_compare <- function(otu.normed, meta, grouping_variable, group_a, group_b) {
  
  ## flips otu.normed
  otu.normed.t = setNames(data.frame(t(otu.normed[,])), row.names(otu.normed))
  ## make the study_id a column
  otu.normed.t$study_id <-row.names(otu.normed.t)
  ## subset meta to be only study_id and grouping_variable
  meta.grp= meta[c("study_id", grouping_variable)]
  ## add grouping variable onto out.normed.t
  merged.data <- merge( meta.grp, otu.normed.t, by="study_id")
  ## move study_id to row.names
  row.names(merged.data) <- merged.data$study_id
  merged.data <- merged.data[,-1]
  
  # Create two df's using subset() for the grouping variable
  #merged.data$grouping_variable is merged.data[,1]
  grp.a.data=subset(merged.data, merged.data[,1]== group_a)
  grp.b.data=subset(merged.data, merged.data[,1]== group_b)
  ## get rid of study_id and grouping variable rows
  grp.a.data=grp.a.data[,-(1)]
  grp.b.data=grp.b.data[,-(1)]
  
  ## measurements for matrix
  a <- length(row.names(grp.a.data)) # number of participants in group a
  b <- length(row.names(grp.b.data)) # number of participants in group b
  n <- a+b # length of dissimilarity matrix
  m <- n*n # total entries in matrix
  
  ## put the groups together to put into dissimilarity function
  ## the first "a" rows are group_a
  ## the second "b" rows are group_b
  ## therefore there are a+b=n rows
  ## the colums are the bugs
  both.data <- rbind(grp.a.data, grp.b.data)
  
# **************************************************************************** #
# ***************      Create dissimilarity matrix             *************** #
# **************************************************************************** #
  horn.df <- as.data.frame(as.matrix(vegdist(both.data, 
                                             method="horn", 
                                             binary=FALSE, 
                                             diag=TRUE, 
                                             upper=TRUE, 
                                             na.rm = FALSE)))
  
# **************************************************************************** #
# ***************       All- non-self dissimilarity            *************** #
# **************************************************************************** #
  all.horn <- data.frame(matrix((NA), nrow = n, ncol = n))
  for ( j in (2:n)) {   
    for( i in (1:(j-1))) {
      all.horn[i,j] <- horn.df[i,j] }}
  all.horn.vector <- as.vector(as.matrix(all.horn, na.rm=TRUE))
  all.horn.vector <- all.horn.vector[!is.na(all.horn.vector)]
  
# **************************************************************************** #
# ***************    Dissimilarity within group a              *************** #
# **************************************************************************** #
  a.horn <- data.frame(matrix(NA, nrow = n, ncol = n))
  for ( j in (2:a)) {   
    for( i in (1:(j-1))) {
      a.horn[i,j] <- horn.df[i,j] }}
  a.horn.vector <- as.vector(as.matrix(a.horn, na.rm=TRUE))
  a.horn.vector <- a.horn.vector[!is.na(a.horn.vector)]
  
# **************************************************************************** #
# ***************    Dissimilarity within group b              *************** #
# **************************************************************************** #
  b.horn <- data.frame(matrix(NA, nrow = n, ncol = n))
  for ( j in ((a+2):n)) {   
    for( i in ((a+1):(j-1))) {
      b.horn[i,j] <- horn.df[i,j] }}
  b.horn.vector <- as.vector(as.matrix(b.horn, na.rm=TRUE))
  b.horn.vector <- b.horn.vector[!is.na(b.horn.vector)]
  
# **************************************************************************** #
# ***************    Dissimilarity between groups a and b      *************** #
# **************************************************************************** #
  between.horn <- data.frame(matrix(NA, nrow = n, ncol = n))
  for ( j in ((a+1):n)) {   
    for( i in (1:a)) {
      between.horn[i,j] <- horn.df[i,j] }}
  between.horn.vector <- as.vector(as.matrix(between.horn, na.rm=TRUE))
  between.horn.vector <- between.horn.vector[!is.na(between.horn.vector)]

# **************************************************************************** #
# **************************************************************************** #
# ***************    Dissilimilarity between paired-groups     *************** #
# **************************************************************************** #
# **************************************************************************** #

#### I dropped a participant to create a mock data frame to build this function
## this section should be deleted
## horn.df.fake <- horn.df[,-1] ### I am just prettending that the groups are equal by deleted a person in group a
## a=6
## b=6
## n=a+b
## m= n*n
# **************************************************************************** #
# *************** Dissilimilarity between related participants *************** #
# **************************************************************************** #

related.horn <- data.frame(matrix(NA, nrow = n, ncol = n))
for ( j in ((a+1):n)) {   
  i=j-a
  related.horn[i,j] <- horn.df[i,j] }
related.horn.vector <- as.vector(as.matrix(related.horn, na.rm=TRUE))
related.horn.vector <- related.horn.vector[!is.na(related.horn.vector)]

# **************************************************************************** #
# ***************Dissilimilarity between unrelated participants*************** #
# **************************************************************************** #

unrelated.horn <- data.frame(matrix(NA, nrow = n, ncol = n))
for ( j in ((a+1): (n-1)      )) {   
  for( i in c(0:(j-a-1), (j-a+1):a   ) ){
    unrelated.horn[i,j] <- horn.df[i,j] }}
for ( j in (n)) {   
  for( i in (0:(j-a-1)   ) ){
    unrelated.horn[i,j] <- horn.df[i,j] }}

unrelated.horn.vector <- as.vector(as.matrix(unrelated.horn, na.rm=TRUE))
unrelated.horn.vector <- unrelated.horn.vector[!is.na(unrelated.horn.vector)]
  
  
# **************************************************************************** #
# ***************    List all Dissim vectors for output        *************** #
# **************************************************************************** #
  output <- list(all.horn.vector, 
                 a.horn.vector, 
                 b.horn.vector, 
                 between.horn.vector, 
                 related.horn.vector, 
                 unrelated.horn.vector)
  names(output)<-c("all.dissim",
                   group_a,
                   group_b, 
                   "between.dissim", 
                   "related.dissim", 
                   "unrelated.dissim")
names(output)<-c("All",
                 group_a, 
                 group_b, 
                 paste(group_a, "vs.", group_b, sep=" "),
                 "Related (paired)",
                 "Unrelated")
# **************************************************************************** #
# ***************              End Function                    *************** #
# **************************************************************************** #
  
  
  return(output)}
##TEST##dissim_pair_compare(otu.normed, meta, "bmi_group", "NW", "Ob")
##TEST## Double check of function, these two lines should be the same
##TEST## sum(output$a.dissim)+sum(output$b.dissim)+sum(output$between.dissim)
##TEST## sum(output$all.dissim)







