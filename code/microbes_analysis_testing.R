# This script is to test the microbes package using public data.

# **************************************************************************** #
# ***************             Start UP Commands                *************** #
# **************************************************************************** #

rm(list=ls())
now=Sys.Date()
today=format(now, format="%d%b%y")

# **************************************************************************** #
# ***************                   Directories/Path           *************** #
# **************************************************************************** #

#setwd("C:/Users/tomczika/Documents/GitHub/microbes/data")
setwd("C:/Users/lemasd/Documents/GitHub/microbes")
#dir="C:/Users/tomczika/Documents/GitHub/microbes/data/"
dir="C:/Users/lemasd/Documents/GitHub/microbes/data/"

list.files()

# **************************************************************************** #
# ***************                   Functions                  *************** #
# **************************************************************************** #

# Create directory variable with functions
#dir.source=c('C:/Users/tomczika/Documents/GitHub/microbes/R/')
dir.source=c('C:/Users/lemasd/Documents/GitHub/microbes/R/')
# library(devtools)
load_all()

# **************************************************************************** #
# ***************                  Import Data                 *************** #
# **************************************************************************** #

#metadata


# otu data
#load(file=otu.path)
#otu.species=otu.species.count

# wgs data



# **************************************************************************** #
# ***************              Reformat Variables              *************** #
# **************************************************************************** #
#study_id must be factor
#bmi_group must be factor
#twin_group must be integer
meta$study_id <- as.factor(meta$study_id)
meta$bmi_group <- as.factor(meta$bmi_group)
meta$twin_group <- as.integer(meta$twin_group)


# **************************************************************************** #
# ***************               normalize_counts.R             *************** #
# **************************************************************************** #

otu.normed=normalize_counts(   )  

# **************************************************************************** #
# ***************  TEST to see that data names match OTU Table *************** #
# **************************************************************************** #
error <- meta_ID_error (meta, otu.species); error
matches <- meta_ID_match(meta, otu.species); matches


# **************************************************************************** #
# ***************          Prep Metadata Files                     *********** #
# **************************************************************************** #

# Sort metadata according to groups
meta.grp.sort=meta_group_sort(meta, meta$bmi_group)

# Sort OTU table according to groups
otu.species.sort=meta_otu_sort(otu.species, meta$study_id)

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

# **************************************************************************** #
# ***************               diversity_table_compare.R                      #
# **************************************************************************** #

test=diversity_table_compare(meta.grp.sort$bmi_group,table); test
test.n= test[c("outcome","all_mean_sd","nw_mean_sd","ow.ob_mean_sd", "t.stat", "df", "p.value" )]
test.n


# **************************************************************************** #
# ***************                   category2pathway.R                         #
# **************************************************************************** #

# Mock data 
data2=df.wgs.counts
cogpathway=path.file

df.wgs.pathway.counts=category2pathway(data2,cogpathway)

# Mouse Data
setwd("C:/Users/lemasd/Documents/GitHub/microbes/Data") # Needs to be loaded with "Data script"
mouse.cog=read.table(file="MouseCOG_test.txt", header=T, row.names=1) 
load(file="COG_Pathway_File.rda")
cogpathway=category2pathway.cog
mouse.cog=category2pathway(mouse.cog,cogpathway)


# **************************************************************************** #
# ***************             poison_regression_permute.R                      #
# **************************************************************************** #

# Mock data
data3=df.wgs.pathway.counts
n.total=13
n.gr1=6
n.gr2=7
df.wgs.pathway.test=poison_regression_permute(data3,n.total,n.gr1,n.gr2)

# Mouse Data
data4=mouse.cog
n.total=5
n.gr1=3
n.gr2=2
mouse.wgs.pathway.test=poison_regression_permute(data4,n.total,n.gr1,n.gr2)



