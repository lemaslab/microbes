## Updated 05Feb15

rm(list = ls())

library(devtools)
install_github("atomczik/microbes")
library(microbes)
library(ggplot2)
# Twin Study Data
#meta <- twin_meta
#otu.counts <- twin_otu_class_green
wgs.counts <- twin_wgs_L4_cog

setwd("C:/Users/tomczika/Documents/GitHub/microbes")
load_all()



#####################################################################################################
################################        Checking data format         ################################
##################################################################################################### 

error <- meta_ID_error(meta, otu.counts); dim(error)
matches <- meta_ID_match(meta, otu.counts); dim(matches)
error <- meta_ID_error(mom.meta, mom.counts); dim(error)
matches <- meta_ID_match(mom.meta, mom.counts); dim(matches)
error <- meta_ID_error(twin.meta, twin.counts); dim(error)
matches <- meta_ID_match(twin.meta, twin.counts); dim(matches)
error <- meta_ID_error(dz.meta, dz.counts); dim(error)
matches <- meta_ID_match(dz.meta, dz.counts); dim(matches)
error <- meta_ID_error(mz.meta, mz.counts); dim(error)
matches <- meta_ID_match(mz.meta, mz.counts); dim(matches)
error <- meta_ID_error(dz.twin.meta, dz.twin.counts); dim(error)
matches <- meta_ID_match(dz.twin.meta, dz.twin.counts); dim(matches)
error <- meta_ID_error(mz.twin.meta, mz.twin.counts); dim(error)
matches <- meta_ID_match(mz.twin.meta, mz.twin.counts); dim(matches)
# Sort metadata by BMI
###meta.bmi <- meta_group_sort(meta, meta$bmi_group)
#####################################################################################################
##########################  metadata analyisis (including hm variables)    ##########################
#####################################################################################################

#meta.bmi.compare <- otu_abundance_compare(meta[,c(1,3,4,5)], meta,"bmi_group","lean","obese")
##!!!! This function does not work because we have no numeric entries in the metadata file !!!!

#####################################################################################################
##########################          otu data prep and analysis             ##########################
#####################################################################################################
## Normalize
otu.normed=normalize_counts(otu.counts)
names(otu.normed) <- c(1:275)
otu.normed[,100:150]
mom.normed=normalize_counts(mom.counts)
twin.normed=normalize_counts(twin.counts)
dz.normed=normalize_counts(dz.counts)
mz.normed=normalize_counts(mz.counts)
dz.twin.normed=normalize_counts(dz.twin.counts)
mz.twin.normed=normalize_counts(mz.twin.counts)

## Drop OTUs that occur in < 4 participants
#otu.normed <- prevalence_crop(otu.normed, 0.2) 
#mom.normed <- prevalence_crop(mom.normed, 0.2) 
#twin.normed <- prevalence_crop(twin.normed, 0.2) 
#str(otu.normed)

##Crop out bugs with low relative abundance bugs 
#otu.normed <- abundance_crop(otu.normed, 0.001) 
#mom.normed <- abundance_crop(mom.normed, 0.001) 
#twin.normed <- abundance_crop(twin.normed, 0.001) 

## Relative Abundance Comparision   
#zygosity.compare <- otu_abundance_compare(twin.normed, meta, "sample.zygosity", "DZ", "MZ")
div.table=alpha_diversity_calc(twin.counts, twin.meta$study_id, twin.meta$sample.zygosity)
div.mean.sd = otu_abundance_compare(div.table, twin.meta, "Twin_Mother", "Twin", "Mom") ## need it to be two variables not DZ MZ and NA

#####################################################################################################
#####################################      category2pathway.R       #################################
#####################################################################################################

### Reduce COG genes to COG pathways
df.wgs.pathway.counts=category2pathway(wgs.counts,category2pathway.cog)

#####################################################################################################
#####################################         Figure 1              #################################
#####################################################################################################
# Measure the Morisita Horn distance between participants
# Make sure you have loaded ggplot2
d.index <- dissim_compare(twin.normed, twin.meta, "sample.zygosity", "DZ", "MZ")
d.index <- dissim_compare(otu.normed, meta, "sample.Twin_Mother", "Twin", "Mom")

dissim_graph(d.index)


#####################################################################################################
##########################           Figure 1 Version 2             #################################
#####################################################################################################
### had to switch "sample.Family" ID's to have at least 2 digits (or are not inside one another)

###############
##   All     ##
###############
same <- c()
different <- c()
test <- dissim_vector(otu.normed, meta, same, different)
long.test <- unlist(test)
length(long.test)

###############
## Unrelated ##
###############

same <- c()
different <- c("sample.Family")
unrelated <- dissim_vector(otu.normed, meta, same, different)
long.unrelated <- unlist(unrelated)

###############
## Twin-Mom  ##
###############
## DZ
dz.maternal <- dissim_vector(dz.normed, dz.meta, same=c("sample.Family"), different=c("sample.Twin_Mother"))
long.dz.maternal <- unlist(dz.maternal)

## MZ
mz.maternal <- dissim_vector(mz.normed, mz.meta, same=c("sample.Family"), different=c("sample.Twin_Mother"))
long.mz.maternal <- unlist(mz.maternal)

###############
## Twin-Twin ##
###############
## DZ
dz.twin.related <- dissim_vector(dz.twin.normed, dz.twin.meta, same=c("sample.Family"), different=c())
long.dz.twin.related <- unlist(dz.twin.related)

## MZ
mz.twin.related <- dissim_vector(mz.twin.normed, mz.twin.meta, same=c("sample.Family"), different=c())
long.mz.twin.related <- unlist(mz.twin.related)

###############
##    Self   ##
###############
self <- dissim_vector(otu.normed, meta, same=c("sample.Individual"), different=c())
long.self <- unlist(self)


###############
##    TEST   ##
###############
## DZ
empty.test <- dissim_vector(otu.normed, meta, same=c("sample.Family"), different=c("sample.zygosity"))
long.empty.test <- unlist(empty.test)


###############
##   mean    ##
###############
mean(as.numeric(long.unrelated)) ## mh- 0.7278472
mean(as.numeric(long.dz.maternal))## mh- 0.7204292
mean(as.numeric(long.mz.maternal))## mh- 0.7710072
mean(as.numeric(long.dz.twin.related))## mh- 0.8013037
mean(as.numeric(long.mz.twin.related))## mh- 0.8252955
mean(as.numeric(long.self))## mh- 0.8722896


###############
##   sd    ##
###############
sd(as.numeric(long.unrelated))## mh- 0.2273444
sd(as.numeric(long.dz.maternal))## mh- 0.2613898
sd(as.numeric(long.mz.maternal))## mh- 0.1863213
sd(as.numeric(long.dz.twin.related))## mh- 0.2521966
sd(as.numeric(long.mz.twin.related))## mh- 0.2036607
sd(as.numeric(long.self))## mh- 0.2366377

#####################################################################################################
#####################################         Figure 2               ################################
#####################################################################################################
ra.graph <- otu.normed
melt.normed<- melt(t(ra.graph ), id.vars = c("study_id"))
names(melt.normed) <- c("study_id", "bug", "RA")
p <- ggplot(melt.normed, aes(x=bug,y=study_id))
figure.2 <- p + geom_tile(aes(fill=melt.normed$RA))
#####################################################################################################
#####################################         Figure 3               ################################
#####################################################################################################
## I have removed all legends and then am just printing out one. They should all match
## I manually annotated the labels
## Other than that, the same code works for any "otu.normed" set to ra.graph <- otu.normed
## choose which group you want to graph:

## Legend for all of the following graphs
#install.packages("gtable")
library(reshape2)
library(gtable)
ra.graph <- otu.normed
melt.normed<- melt(t(ra.graph ), id.vars = c("study_id"))
names(melt.normed) <- c("study_id", "bug", "RA")
ra.graph.reference <- ggplot(melt.normed, aes(x=study_id, y=RA, group=bug, colour = bug, position = "stack"))+ 
  geom_area()+aes(fill=bug )
plot(gtable_filter(ggplot_gtable( ggplot_build(  ra.graph.reference) ),"guide-box"))

## Individual Plots
ra.graph <- otu.normed
melt.normed<- melt(t(ra.graph ), id.vars = c("study_id"))
names(melt.normed) <- c("study_id", "bug", "RA")
figure.3 <- ggplot(melt.normed, aes(x=study_id, y=RA, group=bug, colour = bug, position = "stack"))+ 
  geom_area()+aes(fill=bug ) + xlab("Label.x") + theme_bw() +
  labs(title = "Title") + theme(legend.position="none") +
  theme(axis.text.x = element_blank()) ## axis labels blank b/c of so many participants

#####################################################################################################
#####################################         Figure 4              ################################
#####################################################################################################

melt.wgs <- melt(df.wgs.pathway.counts, id.vars="pathway")
names(melt.wgs) <- c("pathway", "study_id", "value")
figure.4 <- ggplot(melt.wgs, aes(x=pathway,y=value)) + 
  geom_bar(stat="identity", aes(fill=study_id)) +coord_flip()










#otu.normed[,c(112,118,220,241,267,131,138,256)]
