
crop_counts <- function(df, meta, variables, values){
  
#######################################################################
###################### Subset data by groups ##########################
#######################################################################

## Subset data by what we want to look at: moms over time, babies over time, mom vs. baby
## flips otu.normed
df.t = setNames(data.frame(t(df[,])), row.names(df))
## make the study_id a column
df.t$study_id <-row.names(df.t)
## subset meta to be only study_id and grouping_variable
##meta.variables= meta[variables]
## add grouping variable onto out.normed.t
merged.data <- merge( meta, df.t, by="study_id")
## move study_id to row.names
row.names(merged.data) <- merged.data$study_id
merged.data <- merged.data[,-1]

### start loop here
for (i in 1: length(variables)){
variable.column <- which(names(merged.data)==variables[i])
merged.data=subset(merged.data, (merged.data[,variable.column]== values[i]))}
cropped.counts <- merged.data
## get rid of study_id and grouping variable rows
m <- length(meta)
n <- length(merged.data)
p <- m+1
cropped.counts=cropped.counts[, m:n]

##Transpose back to having the columns be the participants (on top)
cropped.counts = setNames(data.frame(t(cropped.counts[,])), row.names(cropped.counts))

return(cropped.counts)

}


