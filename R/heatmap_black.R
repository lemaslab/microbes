########################vvvvvvvvvvvvBLACKVvvvvvvvvvv#############################
#####        This is a function used in the rest of the code                #####
#####        Another version exists in the microbes package                 #####
heatmap_black <- function(df,xvar,yvar,zvar,pvar, p) {
  #####                                                                       #####
  df<- df[,c(xvar,yvar,zvar,pvar)]
  names(df) <- c("xvar","yvar","value","p.value")
  #####                                                                       #####
  subset <- as.data.frame(subset(df, df$p.value<=p))
  neg <- subset(subset, subset$value<=0)
  neg$value <- abs(neg$value)
  #neg$value <-neg$value/min(subset$value)
  pos <- subset(subset, subset$value>0)
  #pos$value <- pos$value/max(subset$value)
  #####                                                                       #####
  heat <- ggplot(df , aes(xvar, yvar)) +
    geom_tile(data = neg , fill = "red", alpha=neg$value) +
    geom_tile(data = pos, fill = "blue", alpha=pos$value) +
    geom_tile(data = subset(df, df$p.value>p) , 
              aes(colour = "insignificant"), 
              linetype = 0, fill = "white")   +
    xlab(xvar) + 
    ylab(yvar) + 
    theme_bw()
  #####                                                                       #####
  return(heat)
} # end of function
#####        This is a function used in the rest of the code                #####
#####        Another version exists in the microbes package                 #####
########################^^^^^^^^^^^^END^^^^^^^^^^^^^#############################