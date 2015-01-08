# This script is used to generate ALL EXAMPLE data sets that will load with the microbes package

# Working directory
# setwd("C:/Users/lemasd/Documents/GitHub/microbes/data")
# list.files()

# **************************************************************************** #
# ***************                Libraries                     *************** #
# **************************************************************************** #
# A Core gut microbiome in obese and lean twins
# http://www.ncbi.nlm.nih.gov/pubmed/19043404
#
# matR package
#-------------
# install.packages("matR", repo="http://dunkirk.mcs.anl.gov/~braithwaite/R", type="source")
# library("matR")
# dependencies ()
# setInternet2(use = TRUE)

# Meta Data
#----------
study_id=c("TS1","TS2","TS4","TS5","TS7","TS8","TS19","TS20","TS28","TS29","TS49","TS50")
twin=c("Twin_1","Twin_1","Twin_2","Twin_2","Twin_3","Twin_3","Twin_7","Twin_7","Twin_10","Twin_10","Twin_15","Twin_15" )
bmi_group=c("lean","lean","lean","lean","lean","lean","obese","obese","obese","obese","obese","obese")
mg_rast_wgs_id=c("mgm4440452.7","mgm4440453.6","mgm4440460.5","mgm4440461.5","mgm4440823.3","mgm4440824.3","mgm4440610.3","mgm4440611.3",
                 "mgm4440613.3","mgm4440616.3","mgm4440614.3","mgm4440615.3")
mg_rast_otu_id=c("mgm4455493.3","mgm4455516.3","mgm4455571.3","mgm4455452.3","mgm4455521.3","mgm4455507.3","mgm4455405.3","mgm4455377.3",
                 "mgm4455376.3","mgm4455431.3","mgm4455653.3","mgm4455466.3")
# create data.frame
twin_meta=data.frame(study_id,twin,bmi_group,mg_rast_wgs_id,mg_rast_otu_id)
save(twin_meta, file="twin_meta.rda")

# Whole-Genome Shotgun Data
#--------------------------
  # http://metagenomics.anl.gov/linkin.cgi?project=10
    # Participant ID's
      twin_WGS_list <- c("TS1"="4440452.7",   # Twin_1-Lean-WGS
                         "TS2"="4440453.6",   # Twin_1-Lean-WGS                      
                         "TS4"="4440460.5",   # Twin_2-Lean-WGS
                         "TS5"="4440461.5",   # Twin_2-Lean-WGS                      
                         "TS7"="4440823.3",   # Twin_3-Lean-WGS
                         "TS8"="4440824.3",   # Twin_3-Lean-WGS                      
                         "TS19"="4440610.3",  # Twin_7-Obese-WGS
                         "TS20"="4440611.3",  # Twin_7-Obese-WGS                      
                         "TS28"="4440613.3",  # Twin_10-Obese-WGS
                         "TS29"="4440616.3",  # Twin_10-Obese-WGS                      
                         "TS49"="4440614.3",  # Twin_15-Obese-WGS
                         "TS50"="4440615.3"   # Twin_15-Obese-WGS
                         )   # End of list  
                         
# Functional Views
kegg.view <- list(L1=c(entry="count",source="KEGG", level="level1"),L2=c(entry="count",source="KEGG", level="level2"),L3=c(entry="count",source="KEGG", level="level3"),L4=c(entry="count",source="KEGG", level="function"))
subsystems.view <- list(L1=c(entry="count",source="Subsystems", level="level1"),L2=c(entry="count",source="Subsystems", level="level2"),L3=c(entry="count",source="Subsystems", level="level3"),L4=c(entry="count",source="Subsystems", level="function"))
cog.view <- list(L1=c(entry="count",source="COG", level="level1"),L2=c(entry="count",source="COG", level="level2"),L3=c(entry="count",source="COG", level="level3"),L4=c(entry="count",source="COG", level="function"))
                         
# COG
    twin.cog <- collection (twin_WGS_list, cog.view)
      # Create data.frame
        twin.wgs.cog=as.data.frame(twin.cog$L4) # might give error about "pending data". Wait 10 min.
        head(twin.wgs.cog)
        names(twin.wgs.cog)
      # Rename columns using metadata
        # Sort metadata according to mg_rast_wgs_id
          metadata.sort=twin.meta[order(mg_rast_wgs_id),]
        # Sort twin.wgs.cog columsn according to metadata.sort
          twin.wgs.cog.sort=twin.wgs.cog[,match(metadata.sort$mg_rast_wgs_id, names(twin.wgs.cog))]
          names(twin.wgs.cog.sort)
        # Replace data.frame names with metadata(mg_rast_wgs_id) names
          twin.wgs.cog1=twin.wgs.cog.sort
          colnames(twin.wgs.cog1)=metadata.sort$study_id
          names(twin.wgs.cog1)
      # Reorder twin.cog according to study_id
          twin.wgs.cog.s=twin.wgs.cog1[,c(1:4,11,12,5:9)]
          names(twin.wgs.cog.s)
          dim(twin.wgs.cog) # Should be 12 columns
          twin_wgs_L4_cog=twin.wgs.cog.s
      # Might need to add "pathway" variable name prior to export
      #-----------------------------------------
      # Save as R-object
        save(twin_wgs_L4_cog, file="twin_wgs_L4_cog.rda")

# 16S Amplicon Data
#------------------
  # http://metagenomics.anl.gov/linkin.cgi?project=66
    twin_16S_list= c( "TS1"="4455493.3",   # Twin_1-Lean-16S
                      "TS2"="4455516.3",   # Twin_1-Lean-16S                      
                      "TS4"="4455571.3",   # Twin_2-Lean-16S
                      "TS5"="4455452.3",   # Twin_2-Lean-16S                      
                      "TS7"="4455521.3",   # Twin_3-Lean-16S
                      "TS8"="4455507.3",   # Twin_3-Lean-16S                      
                      "TS19"="4455405.3",  # Twin_7-Obese-16S
                      "TS20"="4455377.3",  # Twin_7-Obese-16S                      
                      "TS28"="4455376.3",  # Twin_10-Obese-16S
                      "TS29"="4455431.3",  # Twin_10-Obese-16S                      
                      "TS49"="4455653.3",  # Twin_15-Obese-16S
                      "TS50"="4455466.3"   # Twin_15-Obese-16S
                      )  # End of list 

# Taxonomic Views
green.16S  <- list(class=c(entry ="counts",annot="organism",source="Greengenes", level="class"),order=c(entry ="counts",annot="organism",source="Greengenes", level="order"),family=c(entry ="counts",annot="organism",source="Greengenes", level="family"),genus=c(entry ="counts",annot="organism",source="Greengenes", level="genus"))
rdp.16S  <- list(class=c(entry ="counts",annot="organism",source="RDP", level="class"),order=c(entry ="counts",annot="organism",source="RDP", level="order"),family=c(entry ="counts",annot="organism",source="RDP", level="family"),genus=c(entry ="counts",annot="organism",source="RDP", level="genus"))
ssu.16S  <- list(class=c(entry ="counts",annot="organism",source="SSU", level="class"),order=c(entry ="counts",annot="organism",source="SSU", level="order"),family=c(entry ="counts",annot="organism",source="SSU", level="family"),genus=c(entry ="counts",annot="organism",source="SSU", level="genus"))
                      
# Green Genes
twin.16S.green <- collection (twin_16S_list, green.16S)
# Create data.frame
twin.otu.class.green=as.data.frame(twin.16S.green$class) # might give error about "pending data". Wait 10 min.
head(twin.otu.class.green)
names(twin.otu.class.green)
# Rename columns using metadata
# Sort metadata according to mg_rast_wgs_id
metadata.sort=twin.meta[order(mg_rast_otu_id),]
# Sort twin.wgs.cog columsn according to metadata.sort
twin.otu.class.green.s=twin.otu.class.green[,match(metadata.sort$mg_rast_otu_id, names(twin.otu.class.green))]
names(twin.otu.class.green.s)
# Replace data.frame names with metadata(mg_rast_otu_id) names
twin.otu.class.green1=twin.otu.class.green.s
colnames(twin.otu.class.green1)=metadata.sort$study_id
names(twin.otu.class.green1)

# Reorder twin.cog according to study_id
=twin.otu.class.green1[,c(7,9,11,5,10,8,3,2,1,4,12,6)]
names(twin.otu.class.green.s)
dim(twin.otu.class.green.s) # Should be 12 columns
twin_otu_class_green=twin.otu.class.green.s
# Might need to add "Taxa" variable name prior to export
#-----------------------------------------
# Save as R-object
save(twin_otu_class_green, file="twin_otu_class_green.rda")
