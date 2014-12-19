# Mock Metagenomic Gene Count Data

# Data Parameters
ng1 <- 6 # number in group 1
ng2 <- 7 # number in group 2
n <- ng1+ng2 # total sample size

df.wgs.counts <-  as.data.frame(rbind(c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=150)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=140)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=130)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=120)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=110)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=100)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=100)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=100)),
                  c(rpois(n=ng1, lambda=100), rpois(n=ng2, lambda=100))))
  
# Gene names
cog.number=c("COG1234","COG2345","COG3456","COG4567","COG5678","COG6789","COG7891",
             "COG8910","COG9101")
# Participant names
pat.names=c("P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","P11","P12","P13")

# Change names
colnames(df.wgs.counts)=pat.names
row.names(df.wgs.counts)=cog.number


# Mock Pathways to link gene to pathway
path.file=list( path1=c("COG1234","COG2345"),
                path2=c("COG2345","COG3456","COG4567","COG5678"),
                path3=c("COG2345","COG7891","COG9101"),
                path4=c("COG4567","COG5678","COG6789","COG7891","COG8910","COG9101"),
                path5=c("COG1234","COG2345","COG6789","COG7891","COG9101"),
                path6=c("COG1234","COG8910","COG9101"))

