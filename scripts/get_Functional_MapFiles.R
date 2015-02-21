# **************************************************************************** #
# ***************                 EXECUTIVE OVERVIEW           *************** #
# **************************************************************************** #
rm(list=ls())

# This script preps mapping files for Functional Annotations in MG-RAST

# NEED: API code to download mapping files (KEGG, COG, KO)


# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Working Directory
setwd("C:/Users/lemasd/Dropbox/Analysis/Microbiome/MG_RAST/")
list.files()

# work directory
#--------------
dir="C:/Users/lemasd/Dropbox/Analysis/Microbiome/MG_RAST/"

# **************************************************************************** #
# ***************                   MAPPING FILES              *************** #
# **************************************************************************** #

# 11Feb15
# -------
# NOTES:


# Read KO Mapping file
#---------------------
ko.map.file=read.table(file=paste(dir,"ko_parse_out.txt",sep=""),header=T,sep="\t",quote = "",
                       row.names = NULL, stringsAsFactors = FALSE)
names(ko.map.file)
head(ko.map.file)
dim(ko.map.file)


# **************************************************************************** #
# ***************                   REFORMAT FILES             *************** #
# **************************************************************************** #

# Create EC # ids from level 4 annotations
#----------------------------------------
ko.map.file

# Vector
ec.ids<- as.vector(unlist(strsplit(ko.map.file$level4, split = "_")))
head(ec.ids);length(ec.ids)
test=gsub(pattern = "\\[|\\]", replacement = "", x = ec.ids)
test2=gsub(pattern = "\"", replacement = "", x = test,fixed="TRUE")
head(test2)
test3=grepl(pattern="EC:", ec.ids);length(test3)
test4 <- unique(ifelse(test3==TRUE, ko.map.file$level4, NA))

# Vector with only EC #s
test5=grepl(pattern="EC", ko.map.file$level4);length(test5)
test6=ifelse(test5==TRUE, ko.map.file$level4, NA)
test7=gsub(pattern = "\\[|\\]", replacement = "", x = test6)
test8=gsub(pattern = "\"", replacement = "", x = test7,fixed="TRUE")
head(test8)
test9=sapply(strsplit(test8, split = "EC:"), "[", 2)
ko.map.file$ec.ids=test9
names(ko.map.file)
head(ko.map.file)


# Create pathways as array/list
#----------------------------
# K00 to L1
level.len=length(unique(ko.map.file$level1))
ko.map.file$level1=tolower(ko.map.file$level1)
head(ko.map.file)
index=unique(ko.map.file$level1)

output.l1 <- vector("list", level.len)
for (i in 1:level.len){
  path=index[i]
  path.vec <- ifelse(ko.map.file$level1==path, ko.map.file$accession,NA)
  path1=path.vec[!is.na(path.vec)]
  output.l1[[i]]=path1
}
str(output.l1)
names(output.l1)=index

# k00 to L2
level.len=length(unique(ko.map.file$level2))
ko.map.file$level2=tolower(ko.map.file$level2)
ko.map.file$level2=gsub(pattern = "\\\"", replacement = "", x = ko.map.file$level2)
head(ko.map.file)
index=unique(ko.map.file$level2)
level.len=length(unique(ko.map.file$level2))

output.l2 <- vector("list", level.len)
for (i in 1:level.len){
  path=index[i]
  path.vec <- ifelse(ko.map.file$level2==path, ko.map.file$accession,NA)
  path1=path.vec[!is.na(path.vec)]
  output.l2[[i]]=path1
}
str(output.l2)
names(output.l2)=index

# K00 to L3
level.len=length(unique(ko.map.file$level3))
ko.map.file$level3=tolower(ko.map.file$level3)
ko.map.file$level3=gsub(pattern = "\\\"", replacement = "", x = ko.map.file$level3)
head(ko.map.file)
index=unique(ko.map.file$level3)
level.len=length(unique(ko.map.file$level3))

output.l3 <- vector("list", level.len)
for (i in 1:level.len){
  path=index[i]
  path.vec <- ifelse(ko.map.file$level3==path, ko.map.file$accession,NA)
  path1=path.vec[!is.na(path.vec)]
  output.l3[[i]]=path1
}
str(output.l3)
names(output.l3)=index

# Combine lists
#---------------
ko.map=list(output.l1,
            output.l2,
            output.l3)

save(ko.map,file="ko_map_file.rda")

