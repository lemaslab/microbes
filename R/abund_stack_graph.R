
abund_stack_graph <- function(df2){
df2[,4] <-  -df2[,4]
df <- as.data.frame(x)
df$OTU_name <- with(df,factor(OTU_name,levels = sort(unique(OTU_name))))

df.graph <- ggplot() + 
  geom_bar(data=df,aes(x=as.factor(OTU_name), y=(df[,2]),  fill= names(df)[2]), stat="identity") +
  geom_bar(data=df,aes(x=as.factor(OTU_name), y=(df[,4]), fill=names(df)[4]), stat="identity") +
  geom_hline(yintercept=0) + coord_flip() +  scale_y_continuous(labels=abs) +
  xlab("")  + ylab("") +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  
  #theme(text = element_text(size=16), axis.text.x = element_text(size=16, color="grey1")) +
  #theme(axis.text.y = element_text(size=20, color="grey1"))   +  
  scale_fill_manual(values = c("navyblue", "orange2")) # +
  #xlim(rev(levels(x$OTU_name))) 

#a_min <- (x[,2] + (x[,3]/sqrt(1)))
#a_max <- (x[,2] - (x[,3]/sqrt(1)))
#b_min <- (x[,4] + (x[,5]/sqrt(1)))
#b_max <- (x[,4] - (x[,5]/sqrt(1)))

#x.graph <- x.graph + 
 # geom_errorbar(data=x, mapping=aes(x=OTU_name, ymin=a_min, ymax=a_max), width=0.2, size=1, color="black") +
#  geom_errorbar(data=x, mapping=aes(x=OTU_name, ymin=b_min, ymax=b_max), width=0.2, size=1, color="black")  + 
#  theme_bw()
return(df.graph)
}

