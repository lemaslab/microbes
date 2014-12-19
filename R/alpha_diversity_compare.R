#' Test for group differences in alpha diversity.
#' 
#' This function tests for group differences in alpha diversity and returns a table 
#' with alpha diversity mean/sd for all samples as well as group mean/sd.
#' @param div_table data frame of alpha diversity measures for each sample
#' @param meta_group vector that contains sample groups
#' @keywords alpha diversity
#' @export
#' @examples
#' # Test
#' diversity_table_compare(table)
#' test=diversity_table_compare(meta.grp.sort$bmi_group,table); test
diversity_table_compare <- function(meta_group, div_table) {


# **************************************************************************** #
# ***************       Create Data.Frame for group comparison      ********** #
#                               w/ table_group_compare.R                       #
# **************************************************************************** #
  
# Create data.frame
table1.df= table_group_compare(meta_group)

# Create switch index from "meta_group" variable

variable=length(unique(meta_group))
group.switch=paste("T",variable,sep="")

switch(group.switch,
       T2={         
         
         # **************************************************************************** #
         # ***************         Loop Index when group=2              *************** #
         # **************************************************************************** #
         
         
         # How many diversity measures? 
         div.names=names(div_table)
         # Drop
         remove=c("participant","group")
         # names to keep
         index=div.names[!div.names %in% remove]
         myIndex<-length(index);myIndex 
         
         for (i in 1:myIndex)
         { 
           # Create outcome variable
           div=index[i]
           
           # Subset data according to groups
           div_table.grp1=subset(div_table, group==1)
           div_table.grp2=subset(div_table, group==2)
           
           # Calcualte variables
           # All
           all_mean=round(mean(div_table[,div], na.rm=T),digits=2)
           all_sd=round(sd(div_table[,div], na.rm=T),digits=2)
           all_mean_sd=paste("(",all_mean," � ",all_sd,")",sep="")
           
           # Grp1
           grp1_mean=round(mean(div_table.grp1[,div], na.rm=T),digits=2)
           grp1_sd=round(sd(div_table.grp1[,div], na.rm=T),digits=2)
           grp1_mean_sd=paste("(",grp1_mean," � ",grp1_sd,")",sep="")
           
           # Grp2
           grp2_mean=round(mean(div_table.grp2[,div], na.rm=T),digits=2)
           grp2_sd=round(sd(div_table.grp2[,div], na.rm=T),digits=2)
           grp2_mean_sd=paste("(",grp2_mean," � ",grp2_sd,")",sep="")
           
           # Compare
           stat=summary(aov(div_table[,div]~div_table$group))
           df= stat[[1]][[1]][[2]]
           t.stat=round(stat[[1]][[4]][[1]],digits=2)
           p.value=round(stat[[1]][[5]][[1]],digits=3)
           
           # Populate table    
           table1.df[i,1]=div
           table1.df[i,2]=all_mean
           table1.df[i,3]=all_sd
           table1.df[i,4]=all_mean_sd 
           # Grp1
           table1.df[i,5]=grp1_mean  
           table1.df[i,6]=grp1_sd  
           table1.df[i,7]=grp1_mean_sd  
           # Grp2
           table1.df[i,8]=grp2_mean  
           table1.df[i,9]=grp2_sd 
           table1.df[i,10]=grp2_mean_sd 
           # stats
           table1.df[i,11]=t.stat 
           table1.df[i,12]=df 
           table1.df[i,13]=p.value 
           
         } # END of LOOP
         }, # End of T2 switch    
       
       
       T3={# **************************************************************************** #
         # ***************            Loop Index when group=3           *************** #
         # **************************************************************************** #
         
         
         # How many diversity measures? 
         div.names=names(div_table)
         # Drop
         remove=c("participant","group")
         # names to keep
         index=div.names[!div.names %in% remove]
         myIndex<-length(index);myIndex 
         
         for (i in 1:myIndex)
         { 
           # Create outcome variable
           div=index[i]
           
           # Subset data according to groups
           div_table.grp1=subset(div_table, group==1)
           div_table.grp2=subset(div_table, group==2)
           div_table.grp3=subset(div_table, group==3)
           
           # Calcualte variables
           # All
           all_mean=round(mean(div_table[,div], na.rm=T),digits=2)
           all_sd=round(sd(div_table[,div], na.rm=T),digits=2)
           all_mean_sd=paste("(",all_mean," ± ",all_sd,")",sep="")
           
           # Grp1
           grp1_mean=round(mean(div_table.grp1[,div], na.rm=T),digits=2)
           grp1_sd=round(sd(div_table.grp1[,div], na.rm=T),digits=2)
           grp1_mean_sd=paste("(",grp1_mean," ± ",grp1_sd,")",sep="")
           
           # Grp2
           grp2_mean=round(mean(div_table.grp2[,div], na.rm=T),digits=2)
           grp2_sd=round(sd(div_table.grp3[,div], na.rm=T),digits=2)
           grp2_mean_sd=paste("(",grp2_mean," ± ",grp2_sd,")",sep="")
           
           # Grp3
           grp3_mean=round(mean(div_table.grp3[,div], na.rm=T),digits=2)
           grp3_sd=round(sd(div_table.grp2[,div], na.rm=T),digits=2)
           grp3_mean_sd=paste("(",grp3_mean," ± ",grp3_sd,")",sep="")
           
           # Compare
           stat=summary(aov(div_table[,div]~div_table$group))
           df= stat[[1]][[1]][[2]]
           t.stat=round(stat[[1]][[4]][[1]],digits=2)
           p.value=round(stat[[1]][[5]][[1]],digits=3)
           
           # Populate table    
           table1.df[i,1]=div
           table1.df[i,2]=all_mean
           table1.df[i,3]=all_sd
           table1.df[i,4]=all_mean_sd 
           # Grp1
           table1.df[i,5]=grp1_mean  
           table1.df[i,6]=grp1_sd  
           table1.df[i,7]=grp1_mean_sd  
           # Grp2
           table1.df[i,8]=grp2_mean  
           table1.df[i,9]=grp2_sd 
           table1.df[i,10]=grp2_mean_sd 
           # Grp2
           table1.df[i,11]=grp3_mean  
           table1.df[i,12]=grp3_sd 
           table1.df[i,13]=grp3_mean_sd 
           # stats
           table1.df[i,14]=t.stat 
           table1.df[i,15]=df 
           table1.df[i,16]=p.value 
           
         } # END of LOOP},
       }, # End of T3 switch
         
       
       T4={# **************************************************************************** #
         # ***************              Loop Index when group=4         *************** #
         # **************************************************************************** #
         
         
         # How many diversity measures? 
         div.names=names(div_table)
         # Drop
         remove=c("participant","group")
         # names to keep
         index=div.names[!div.names %in% remove]
         myIndex<-length(index);myIndex 
         
         for (i in 1:myIndex)
         { 
           # Create outcome variable
           div=index[i]
           
           # Subset data according to groups
           div_table.grp1=subset(div_table, group==1)
           div_table.grp2=subset(div_table, group==2)
           div_table.grp3=subset(div_table, group==3)
           div_table.grp4=subset(div_table, group==4)
           
           # Calcualte variables
           # All
           all_mean=round(mean(div_table[,div], na.rm=T),digits=2)
           all_sd=round(sd(div_table[,div], na.rm=T),digits=2)
           all_mean_sd=paste("(",all_mean," ± ",all_sd,")",sep="")
           
           # Grp1
           grp1_mean=round(mean(div_table.grp1[,div], na.rm=T),digits=2)
           grp1_sd=round(sd(div_table.grp1[,div], na.rm=T),digits=2)
           grp1_mean_sd=paste("(",grp1_mean," ± ",grp1_sd,")",sep="")
           
           # Grp2
           grp2_mean=round(mean(div_table.grp2[,div], na.rm=T),digits=2)
           grp2_sd=round(sd(div_table.grp2[,div], na.rm=T),digits=2)
           grp2_mean_sd=paste("(",grp2_mean," ± ",grp2_sd,")",sep="")
           
           # Grp3
           grp3_mean=round(mean(div_table.grp3[,div], na.rm=T),digits=2)
           grp3_sd=round(sd(div_table.grp3[,div], na.rm=T),digits=2)
           grp3_mean_sd=paste("(",grp3_mean," ± ",grp3_sd,")",sep="")
           
           # Grp4
           grp4_mean=round(mean(div_table.grp4[,div], na.rm=T),digits=2)
           grp4_sd=round(sd(div_table.grp4[,div], na.rm=T),digits=2)
           grp4_mean_sd=paste("(",grp4_mean," ± ",grp4_sd,")",sep="")
           
           # Compare
           stat=summary(aov(div_table[,div]~div_table$group))
           df= stat[[1]][[1]][[2]]
           t.stat=round(stat[[1]][[4]][[1]],digits=2)
           p.value=round(stat[[1]][[5]][[1]],digits=3)
           
           # Populate table    
           table1.df[i,1]=div
           table1.df[i,2]=all_mean
           table1.df[i,3]=all_sd
           table1.df[i,4]=all_mean_sd 
           # Grp1
           table1.df[i,5]=grp1_mean  
           table1.df[i,6]=grp1_sd  
           table1.df[i,7]=grp1_mean_sd  
           # Grp2
           table1.df[i,8]=grp2_mean  
           table1.df[i,9]=grp2_sd 
           table1.df[i,10]=grp2_mean_sd 
           # Grp3
           table1.df[i,11]=grp3_mean  
           table1.df[i,12]=grp3_sd 
           table1.df[i,13]=grp3_mean_sd 
           # Grp2
           table1.df[i,14]=grp4_mean  
           table1.df[i,15]=grp4_sd 
           table1.df[i,16]=grp4_mean_sd 
           # stats
           table1.df[i,17]=t.stat 
           table1.df[i,18]=df 
           table1.df[i,19]=p.value 
           
         } # END of LOOP
         }, # End of T4 swtich       
       
       stop("This function can't accomodate when meta_group>4!")
)
#########################################################

return(table1.df)

} # end of function



  
