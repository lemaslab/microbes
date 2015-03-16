Vector_graph_data <- function( vectors) {
  
  ## These three vectors are going to be the columns in our data frame ouput.
  name <- vectors
  mean <- "blank"
  sd <- "blank"
  
  ## loop through to find the mean and sd for each vector
  for (i in 1:length(name)) {
    mean[i] <- round(mean(get(name[i])), digits=4)
    sd[i] <- round(sd(get(name[i])), digits=4)  }
  ## combine together in data frame
  fig.1.data <- as.data.frame(cbind(name, mean, sd))
  fig.1.data$name <- with(fig.1.data,factor(name,levels = (unique(name))))
  fig.1.data$mean <- as.numeric(as.character(fig.1.data$mean)) 
  return(fig.1.data) }