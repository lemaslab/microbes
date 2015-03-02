
# Updated 28Feb15: twin.ko.data

rm(list = ls())


# Testing local repo
# setwd("C:/Users/lemasd/Documents/GitHub/microbes")
# library(devtools)
load_all()

# Testing github repo
library(devtools)
install_github("dlemas/microbes")
library(microbes)

# Twin Study Data
meta <- twin.meta.18mz
otu.phylum <- twin.18mz.phylum
otu.family <- twin.18mz.family
otu.genus <- twin.18mz.genus
wgs.counts <- twin.wgs.18mz.ko.l4

#####################################################################################################
################################        Checking data format         ################################
##################################################################################################### 

otu.counts=otu.family

error <- meta_ID_error(meta, otu.counts); dim(error)
matches <- meta_ID_match(meta, otu.counts); dim(matches)

# Sort metadata by BMI
names(meta)
meta.bmi <- meta_group_sort(meta, meta$sample.ObesityCat)


#####################################################################################################
##########################          otu data prep and analysis             ##########################
#####################################################################################################
## Normalize
otu.normed=normalize_counts(otu.counts)
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

# **************************************************************************** #
# ***************                   category2pathway.R                         #
# **************************************************************************** #

# Reduce KO terms to KO pathways
wgs.counts.l3=gene2path(wgs.counts,ko.map, 3)

# **************************************************************************** #
# ***************             poison_regression_permute.R                      #
# **************************************************************************** #

wgs_path_count=wgs.counts.l3
n.total=length(names(wgs_path_count))
n.gr1=as.numeric(table(meta.bmi$bmi_group)["NW"])
n.gr2=as.numeric(table(meta.bmi$bmi_group)["OW/Ob"])

# run model (need a bigger computer!)
# wgs.path.L3.fit=poison_regression_permute(wgs_path_count,n.total,n.gr1,n.gr2)

# **************************************************************************** #
# ***************                   dissim_compare.R                    #
# **************************************************************************** #

# Measure the Morisita Horn distance between participants

d.index <- dissim_compare(otu.normed, meta, "sample.twin_mother", "Twin", "Mom")
dissim_graph(d.index)

meta.fake <- meta ## I am making a fake data frame to compare twins by pairs
meta.fake$pairs <- c("a","b","a","b","a","b","a","b", "a","b","a","b")
d.pair.index <- dissim_pair_compare(otu.normed, meta.fake, "pairs", "a", "b")
dissim_pair_graph(d.pair.index)
