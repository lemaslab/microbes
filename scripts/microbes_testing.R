# This script is to test the microbes package using public data.

# **************************************************************************** #
# ***************             Start UP Commands                *************** #
# **************************************************************************** #



# **************************************************************************** #
# ***************             Load microbes package            *************** #
# **************************************************************************** #

library(devtools)
#install_github("dlemas/microbes")
load_all()
library(microbes)

# **************************************************************************** #
# ***************                  Import Data                 *************** #
# **************************************************************************** #

# A Core gut microbiome in obese and lean twins
# http://www.ncbi.nlm.nih.gov/pubmed/19043404

# metadata
load("twin_meta.rda")
head(twin_meta)

# 16S data
load("twin_otu_class_green.rda")
head(twin_otu_class_green)

# wgs data
load("twin_wgs_L4_cog.rda")
head(twin_wgs_L4_cog)


# **************************************************************************** #
# ***************              Reformat Variables              *************** #
# **************************************************************************** #
#study_id must be factor
#bmi_group must be factor
#twin_group must be integer
twin_meta$study_id <- as.factor(twin_meta$study_id)
twin_meta$bmi_group <- as.factor(twin_meta$bmi_group)
twin_meta$twin_group <- as.integer(twin_meta$twin)


# **************************************************************************** #
# ***************               normalize_counts.R             *************** #
# **************************************************************************** #

otu.normed=normalize_counts(twin_otu_class_green)  

# **************************************************************************** #
# ***************  TEST to see that data names match OTU Table *************** #
# **************************************************************************** #
error <- meta_ID_error (twin_meta, otu.normed); error
matches <- meta_ID_match(twin_meta, otu.normed); matches


# **************************************************************************** #
# ***************          Prep Metadata Files                     *********** #
# **************************************************************************** #

# Sort metadata according to groups
meta.grp.sort=meta_group_sort(twin_meta, twin_meta$bmi_group)

# Sort OTU table according to groups
otu.species.sort=meta_otu_sort(twin_otu_class_green, twin_meta$study_id)

# check: works
names(otu.species)
names(otu.species.sort)

# **************************************************************************** #
# ***************           alpha_diversity_calc.R                             #
# **************************************************************************** #

table=alpha_diversity_calc(otu.species.sort, meta.grp.sort$study_id, meta.grp.sort$bmi_group)
head(otu.species.sort)
head(meta.grp.sort$study_id)
head(meta.grp.sort$bmi_group)
str(meta.grp.sort)

