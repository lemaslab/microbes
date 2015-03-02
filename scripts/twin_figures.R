
# Pull Latest Github repo
library(devtools)
install_github("dlemas/microbes")
library(microbes)
library(ggplot2)
library(plyr)


# Twin Study Data
meta <- twin.meta.18mz
otu.phylum <- twin.18mz.phylum
otu.family <- twin.18mz.family
otu.genus <- twin.18mz.genus
wgs.counts <- twin.wgs.18mz.ko.l4


# plot taxa
#
# otu.phylum
#-----------

# transform data
otu.norm=normalize_counts(otu.phylum)
otu.trans=as.data.frame(t(otu.norm))
otu.trans$id=row.names(otu.trans)
str(otu.trans)
otu.trans.ready=otu.trans[,c(15,1:14)]
head(otu.trans.ready)

# Reformat the data
A<- reshape(otu.trans.ready, 
            varying = names(otu.trans.ready[-1]), 
            v.names = "rel.abund",
            timevar = "bug", 
            times = names(otu.trans.ready[-1]),
            direction = "long")
row.names(A)=NULL
B=as.data.frame(A[1:3])
B$bug=as.factor(B$bug)
B$id.n=as.numeric(factor(B$id))


# plot: twin data
# plotting works
names(B)
plot.base=ggplot(B, aes(x=id.n, y=rel.abund, fill=bug))+geom_area(colour="black", size=.2, alpha=.4)
# add colors: ordering of bugs, legend, sample labels
plot1=plot.base+scale_color_hue()+scale_x_discrete(labels=B$id)
plot2=plot1+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
plot2
# add labels
plot2+labs(x="Participants", y="Relative Abundance (%)")


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
