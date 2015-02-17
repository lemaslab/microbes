
#'
#' @param df is otu.normed
#' @keywords Morisita
#' @export
#' @examples
#' dissim_pair_graph()


## Updated 28Jan15

dissim_pair_graph <- function(d.index) {
  
  
  ## make each dissimilarity vector into a df
  df1 <- cbind(as.data.frame(d.index[1]), names(d.index[1]), mean(d.index[[1]]), sd(d.index[[1]])/(sqrt(length(d.index[[1]]  )))   )
  names(df1) <- c("dissim","group", "group.mean", "group.se")
  df2 <- cbind(as.data.frame(d.index[2]), names(d.index[2]), mean(d.index[[2]]), sd(d.index[[2]])/(sqrt(length(d.index[[2]] )))   )
  names(df2) <- c("dissim","group", "group.mean", "group.se")
  df3 <- cbind(as.data.frame(d.index[3]), names(d.index[3]), mean(d.index[[3]]), sd(d.index[[3]])/(sqrt(length(d.index[[3]]  )))    )
  names(df3) <- c("dissim","group", "group.mean", "group.se")
  df4 <- cbind(as.data.frame(d.index[4]), names(d.index[4]), mean(d.index[[4]]), sd(d.index[[4]])/(sqrt(length(d.index[[4]]  )))    )
  names(df4) <- c("dissim","group", "group.mean", "group.se")
  df5 <- cbind(as.data.frame(d.index[5]), names(d.index[5]), mean(d.index[[5]]), sd(d.index[[5]])/(sqrt(length(d.index[[5]] )))    )
  names(df5) <- c("dissim","group", "group.mean", "group.se")
  df6 <- cbind(as.data.frame(d.index[6]), names(d.index[6]), mean(d.index[[6]]), sd(d.index[[6]])/(sqrt(length(d.index[[6]] )))    )
  names(df6) <- c("dissim","group", "group.mean", "group.se")
  
  
  ## combine individual dfs into one large "df"
  df <- rbind(df1, df2, df3, df4, df5, df6)
  ## create limits for standard error bars
  limits <- aes(ymax = group.mean + group.se, ymin= group.mean - group.se)
  
  ## graph with eror bars.
  ## fun.y is really unnessesary 
  ##since I already made a "group.mean" column in df,
  ## I left it in so for future use w/o that column
  ggplot(df, aes(x=factor(group), y=group.mean)) + 
    stat_summary(fun.y="mean", 
                 geom="bar", 
                 fill=c("black","green","yellow","purple", "red", "blue"))   +
    scale_y_continuous(limits=c(0, 1)) +
    ylab("Morisita-Horn Index Index")   +
    xlab("") +
    geom_errorbar(limits, width=0.25)+theme_bw() 
  
}
