
#####################################################################################################
##########################                Start-up                         ##########################
#####################################################################################################
rm(list=ls())
now=Sys.Date(); today=format(now, format="%d%b%y")

# install.packages(devtools)
# install Rtools 3.3 from http://cran.r-project.org/bin/windows/Rtools/ and then run find_rtools().
# on UF system, I needed to add C:\\Rtools\\bin & C:\\Rtools\\gcc-4.6.3\\bin to the PATH
# find_rtools(T)
# check the PATH
# Sys.getenv("PATH")

library(devtools)
install_github("dlemas/microbes")
library(microbes)

#####################################################################################################
##########################           Explore Data Sets                     ##########################
#####################################################################################################

# Twin Study Data
meta <- twin.meta.18mz;            head(meta)
otu.phylum <- twin.18mz.phylum;    head(otu.phylum)
otu.family <- twin.18mz.family;    head(otu.family)
otu.genus <- twin.18mz.genus;      head(otu.genus)
wgs.counts <- twin.wgs.18mz.ko.l4; head(wgs.counts)



#####################################################################################################
##########################          TAXONOMIC ANALYSES                     ##########################
#####################################################################################################
## Normalize
otu.normed=normalize_counts(otu.phylum)
sum(otu.normed[,2])

## Drop OTUs that occur in < 4 participants
otu.normed <- prevalence_crop(otu.normed, 0.2) ## removed 5

## Crop out bugs with low relative abundance bugs 
otu.normed <- abundance_crop(otu.normed, 0.001) ## removed 3

## Calculate Diversity   
div.table=alpha_diversity_calc(otu.counts, meta$sample_name, meta$sample.twin_mother)

## Compare Accordint to twin grouping   
twin.compare <- group_compare(otu.normed, meta, meta$sample_name, meta$sample.twin_mother)
div.compare = group_compare(div.table, meta, meta$sample_name, meta$sample.twin_mother)


#####################################################################################################
##########################                METAGENOMIC ANALYSES             ##########################
#####################################################################################################

##  Reduce COG genes to COG pathways     
cog.counts=category2pathway(twin_wgs_L4_cog, category2pathway.cog)
# Reduce KO terms to KO pathways
ko.counts.l4=gene2pathway(twin.wgs.18mz.ko.l4,ko.map, 1)



## cog Analysis     
cog.path <- cog.path.ref(category2pathway.cog) 
cog.cv.ra <- cog_cv_ra(twin_wgs_L4_cog)
cog.analysis <- wgs_function(twin_wgs_L4_cog, category2pathway.cog )

## ko Analysis 
##!!!## ko.path <- cog.path.ref(ko.map) 
ko.cv.ra <- cog_cv_ra(twin.wgs.18mz.ko.l4)
##!!!## ko.analysis <- wgs_function(twin.wgs.18mz.ko.l4, ko.map )

# Keg analysis
#keg.path <- cog.path.ref(ko.map) 
#keg.cv.ra <- cog_cv_ra(twin.18mz.kegg.L4)
#keg.analysis <- wgs_function(twin.18mz.kegg.L4, ko.map )

### Not sure how to incoorporate this one
##category2pathway(twin_wgs_L4_cog, cog.cv.ra)
# aggregate 
##IGNORE ## test <- aggregate(cog.analysis[,c(1,4,5)], by=list(cog.analysis$cog.name), FUN=mean, na.rm=TRUE)
##IGNORE ##test <- (test[,c(1,3,4)])
##IGNORE ##row.names(test) <- test[,1]
##IGNORE ##test <- test[,-1]
##IGNORE ##category2pathway(test, category2pathway.cog)

## Checking data format   
# This function is probably unnessessary
# Leaving it in for now
error <- meta_ID_error(meta, otu.counts); dim(error)
matches <- meta_ID_match(meta, otu.counts); dim(matches)

## Normalize
otu.normed <- normalize_counts(otu.counts)
wgs.normed <- normalize_counts(cog.counts)

## Drop OTUs that occur in < 4 participants
otu.normed <- prevalence_crop(otu.normed, 0.2) 
wgs.normed <- prevalence_crop(wgs.normed, 0.2) 

##Crop out bugs with low relative abundance bugs 
otu.normed <- abundance_crop(otu.normed, 0.001) 
wgs.normed <- abundance_crop(wgs.normed, 0.001) 

## Subset data
dz.normed <- crop_counts(otu.normed, meta, c("sample.zygosity"), c("DZ"))
mz.normed <- crop_counts(otu.normed, meta, c("sample.zygosity"), c("MZ"))
dz.twin.normed <- crop_counts(otu.normed, meta, c("sample.zygosity", "sample.Twin_Mother"), c("DZ","Twin"))
mz.twin.normed <- crop_counts(otu.normed, meta, c("sample.zygosity", "sample.Twin_Mother"), c("MZ","Twin"))

## Relative Abundance Comparision   
#zygosity.compare <- otu_abundance_compare(twin.normed, meta, "sample.zygosity", "DZ", "MZ")
div.table=alpha_diversity_calc(otu.counts)
div.mean.sd = otu_abundance_compare(div.table, meta, "sample.Twin_Mother", "Mom" , "Twin") ## need it to be two variables not DZ MZ and NA

## Morisita Horne Dissimilarity Analysis 
#!!! had to switch "sample.Family" ID's to have at least 2 digits (or are not inside one another)
#!!! before I got rid of dz.twin.meat and mz.twin.meta (all of the meta subsets), 
#!!! ...the results were much shorter. (hmmmm?????)
unrelated <- dissim_vector(otu.normed, meta, same <- c(), different <- c("sample.Family"))
dz.maternal <- dissim_vector(dz.normed, meta, same=c("sample.Family"), different=c("sample.Twin_Mother"))
mz.maternal <- dissim_vector(mz.normed, meta, same=c("sample.Family"), different=c("sample.Twin_Mother"))
dz.twin.related <- dissim_vector(dz.twin.normed, meta, same=c("sample.Family"), different=c())
mz.twin.related <- dissim_vector(mz.twin.normed, meta, same=c("sample.Family"), different=c())
self <- dissim_vector(otu.normed, meta, same=c("sample.Individual"), different=c())

## Dissim comparisons 
t.test.test <- t.test(as.numeric(mz.maternal), as.numeric(dz.maternal))

#####################################################################################################
##########################               Graphing                   #################################
#####################################################################################################

## Graph data prep
melt.normed<- melt(t(otu.normed), id.vars = c("study_id"))
names(melt.normed) <- c("study_id", "bug", "RA")
melt.wgs<- melt(t(wgs.normed), id.vars = c("study_id"))
names(melt.wgs) <- c("study_id", "pathway", "RA")

###############
#### Fig.1 ####
###############
fig.1.data <- Vector_graph_data(vectors <- c("unrelated", "dz.maternal", "mz.maternal", "dz.twin.related", "mz.twin.related", "self"))
fig.1 <- ggplot(fig.1.data, aes(x=factor(name), y=mean)) + 
  geom_bar(stat="identity") + coord_cartesian(ylim=c(0.75,1))

fig.1.data$mean.inv <- -log(fig.1.data$mean)
fig.1inv <- ggplot(fig.1.data, aes(x=factor(name), y=mean.inv)) + 
  geom_bar(stat="identity") + coord_cartesian(ylim=c(0,0.3))
###############
#### Fig.2 ####
###############
fig.2<- ggplot(melt.normed, aes(x=bug,y=study_id)) + 
  geom_tile(aes(fill=melt.normed$RA))

###############
#### Fig.3 ####
###############         
## I have removed all legends and then am just printing out one. They should all match
## I manually annotated the labels
## Other than that, the same code works for any "otu.normed" set to ra.graph <- otu.normed
## choose which group you want to graph:

## Legend for OTU graphs
ra.graph.reference <- ggplot(melt.normed, aes(x=study_id, y=RA, group=bug, colour = bug, position = "stack"))+ 
  geom_area()+aes(fill=bug )
plot(gtable_filter(ggplot_gtable( ggplot_build(  ra.graph.reference) ),"guide-box"))

## Individual OTU Plots
fig3.otu <- ggplot(melt.normed, aes(x=study_id, y=RA, group=bug, colour = bug, position = "stack"))+ 
  geom_area()+aes(fill=bug ) + xlab("Label.x") + theme_bw() +
  labs(title = "Title") + theme(legend.position="none") +
  theme(axis.text.x = element_blank()) ## axis labels blank b/c of so many participants

## Legend for WGS graphs
ra.graph.reference <- ggplot(melt.wgs, aes(x=study_id, y=RA, group=pathway, colour = pathway, position = "stack"))+ 
  geom_area()+aes(fill=pathway )
plot(gtable_filter(ggplot_gtable( ggplot_build(  ra.graph.reference) ),"guide-box"))

## Individual  WGS Plots
fig3.wgs <- ggplot(melt.wgs, aes(x=study_id, y=RA, group=pathway, colour = pathway, position = "stack"))+ 
  geom_area()+aes(fill=pathway ) + xlab("Label.x") + theme_bw() +
  labs(title = "Title") + theme(legend.position="none") +
  theme(axis.text.x = element_blank()) ## axis labels blank b/c of so many participants







###############
#### Fig.4 ####
############### 
## !!! comine bars
## !!! Figure out whether RA or RA_mean (Angela knows what this means)
fig4.wgs <- ggplot(cog.analysis[1:200,] , aes(x=pathway.name, y=RA)) + geom_bar(aes(fill=cog.analysis[1:200,]$core_variable, width=0.5), position = position_dodge(width = 0.9), stat="identity") +
  coord_flip()+ scale_fill_grey()

head(cog.analysis)