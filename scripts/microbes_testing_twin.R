
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
meta <- twin_meta
otu.phylum <- twin.18mz.phylum
otu.family <- twin.18mz.family
otu.genus <- twin.18mz.genus
wgs.counts <- twin.wgs.18mz.ko.l4

#####################################################################################################
################################        Checking data format         ################################
##################################################################################################### 

error <- meta_ID_error(meta, otu.counts); dim(error)
matches <- meta_ID_match(meta, otu.counts); dim(matches)

# Sort metadata by BMI
meta.bmi <- meta_group_sort(meta, meta$bmi_group)

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
sum(otu.normed[,2])

## Drop OTUs that occur in < 4 participants
otu.normed <- prevalence_crop(otu.normed, 0.2) ## removed 5

##Crop out bugs with low relative abundance bugs 
otu.normed <- abundance_crop(otu.normed, 0.001) ## removed 3

## Relative Abundance Comparision   
# New group compare function
all.bmi.compare <- group_compare(otu.normed, meta, meta$study_id, meta$bmi_group)

# old group compare function
all.bmi.compare2 <- otu_abundance_compare(otu.normed, meta, "bmi_group", "lean", "Obese")


## alpha_diversity_calc.R   
div.table=alpha_diversity_calc(otu.counts, meta$study_id, meta$bmi_group)
div.mean.sd = otu_abundance_compare(div.table, meta, "bmi_group", "NW", "Ob")

# **************************************************************************** #
# ***************                   category2pathway.R                         #
# **************************************************************************** #

# Reduce COG genes to COG pathways
df.wgs.pathway.counts=category2pathway(wgs.counts,category2pathway.cog)

# **************************************************************************** #
# ***************             poison_regression_permute.R                      #
# **************************************************************************** #

# Input Parameters (need to change the groupings to be consistent with paper!)
wgs.pathways=df.wgs.pathway.counts
n.total=11
n.gr1=6
n.gr2=5
twin.wgs.pathway.fit=poison_regression_permute(wgs.pathways,n.total,n.gr1,n.gr2)

# **************************************************************************** #
# ***************                   dissim_compare.R                    #
# **************************************************************************** #

# Measure the Morisita Horn distance between participants

d.index <- dissim_compare(otu.normed, meta, "bmi_group", "lean", "obese")
dissim_graph(d.index)

meta.fake <- meta ## I am making a fake data frame to compare twins by pairs
meta.fake$pairs <- c("a","b","a","b","a","b","a","b", "a","b","a","b")
d.pair.index <- dissim_pair_compare(otu.normed, meta.fake, "pairs", "a", "b")
dissim_pair_graph(d.pair.index)
