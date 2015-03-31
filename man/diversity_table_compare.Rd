\name{diversity_table_compare}
\alias{diversity_table_compare}
\title{ this function}
\description{
[text]}
\usage{
diversity_table_compare(meta_group, div_table)
}
\arguments{
  \item{meta_group}{[text]}
  \item{div_table}{[text]}
}
\details{[text]}
\value{[text]
}
\references{[text]}
\author{[text]}
\note{[text]}


\seealso{[text]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (meta_group, div_table) 
{
    table1.df = table_group_compare(meta_group)
    variable = length(unique(meta_group))
    group.switch = paste("T", variable, sep = "")
    switch(group.switch, T2 = {
        div.names = names(div_table)
        remove = c("participant", "group")
        index = div.names[!div.names \%in\% remove]
        myIndex <- length(index)
        myIndex
        for (i in 1:myIndex) {
            div = index[i]
            div_table.grp1 = subset(div_table, group == 1)
            div_table.grp2 = subset(div_table, group == 2)
            all_mean = round(mean(div_table[, div], na.rm = T), 
                digits = 2)
            all_sd = round(sd(div_table[, div], na.rm = T), digits = 2)
            all_mean_sd = paste("(", all_mean, " � ", all_sd, 
                ")", sep = "")
            grp1_mean = round(mean(div_table.grp1[, div], na.rm = T), 
                digits = 2)
            grp1_sd = round(sd(div_table.grp1[, div], na.rm = T), 
                digits = 2)
            grp1_mean_sd = paste("(", grp1_mean, " � ", grp1_sd, 
                ")", sep = "")
            grp2_mean = round(mean(div_table.grp2[, div], na.rm = T), 
                digits = 2)
            grp2_sd = round(sd(div_table.grp2[, div], na.rm = T), 
                digits = 2)
            grp2_mean_sd = paste("(", grp2_mean, " � ", grp2_sd, 
                ")", sep = "")
            stat = summary(aov(div_table[, div] ~ div_table$group))
            df = stat[[1]][[1]][[2]]
            t.stat = round(stat[[1]][[4]][[1]], digits = 2)
            p.value = round(stat[[1]][[5]][[1]], digits = 3)
            table1.df[i, 1] = div
            table1.df[i, 2] = all_mean
            table1.df[i, 3] = all_sd
            table1.df[i, 4] = all_mean_sd
            table1.df[i, 5] = grp1_mean
            table1.df[i, 6] = grp1_sd
            table1.df[i, 7] = grp1_mean_sd
            table1.df[i, 8] = grp2_mean
            table1.df[i, 9] = grp2_sd
            table1.df[i, 10] = grp2_mean_sd
            table1.df[i, 11] = t.stat
            table1.df[i, 12] = df
            table1.df[i, 13] = p.value
        }
    }, T3 = {
        div.names = names(div_table)
        remove = c("participant", "group")
        index = div.names[!div.names \%in\% remove]
        myIndex <- length(index)
        myIndex
        for (i in 1:myIndex) {
            div = index[i]
            div_table.grp1 = subset(div_table, group == 1)
            div_table.grp2 = subset(div_table, group == 2)
            div_table.grp3 = subset(div_table, group == 3)
            all_mean = round(mean(div_table[, div], na.rm = T), 
                digits = 2)
            all_sd = round(sd(div_table[, div], na.rm = T), digits = 2)
            all_mean_sd = paste("(", all_mean, " ± ", all_sd, 
                ")", sep = "")
            grp1_mean = round(mean(div_table.grp1[, div], na.rm = T), 
                digits = 2)
            grp1_sd = round(sd(div_table.grp1[, div], na.rm = T), 
                digits = 2)
            grp1_mean_sd = paste("(", grp1_mean, " ± ", grp1_sd, 
                ")", sep = "")
            grp2_mean = round(mean(div_table.grp2[, div], na.rm = T), 
                digits = 2)
            grp2_sd = round(sd(div_table.grp3[, div], na.rm = T), 
                digits = 2)
            grp2_mean_sd = paste("(", grp2_mean, " ± ", grp2_sd, 
                ")", sep = "")
            grp3_mean = round(mean(div_table.grp3[, div], na.rm = T), 
                digits = 2)
            grp3_sd = round(sd(div_table.grp2[, div], na.rm = T), 
                digits = 2)
            grp3_mean_sd = paste("(", grp3_mean, " ± ", grp3_sd, 
                ")", sep = "")
            stat = summary(aov(div_table[, div] ~ div_table$group))
            df = stat[[1]][[1]][[2]]
            t.stat = round(stat[[1]][[4]][[1]], digits = 2)
            p.value = round(stat[[1]][[5]][[1]], digits = 3)
            table1.df[i, 1] = div
            table1.df[i, 2] = all_mean
            table1.df[i, 3] = all_sd
            table1.df[i, 4] = all_mean_sd
            table1.df[i, 5] = grp1_mean
            table1.df[i, 6] = grp1_sd
            table1.df[i, 7] = grp1_mean_sd
            table1.df[i, 8] = grp2_mean
            table1.df[i, 9] = grp2_sd
            table1.df[i, 10] = grp2_mean_sd
            table1.df[i, 11] = grp3_mean
            table1.df[i, 12] = grp3_sd
            table1.df[i, 13] = grp3_mean_sd
            table1.df[i, 14] = t.stat
            table1.df[i, 15] = df
            table1.df[i, 16] = p.value
        }
    }, T4 = {
        div.names = names(div_table)
        remove = c("participant", "group")
        index = div.names[!div.names \%in\% remove]
        myIndex <- length(index)
        myIndex
        for (i in 1:myIndex) {
            div = index[i]
            div_table.grp1 = subset(div_table, group == 1)
            div_table.grp2 = subset(div_table, group == 2)
            div_table.grp3 = subset(div_table, group == 3)
            div_table.grp4 = subset(div_table, group == 4)
            all_mean = round(mean(div_table[, div], na.rm = T), 
                digits = 2)
            all_sd = round(sd(div_table[, div], na.rm = T), digits = 2)
            all_mean_sd = paste("(", all_mean, " ± ", all_sd, 
                ")", sep = "")
            grp1_mean = round(mean(div_table.grp1[, div], na.rm = T), 
                digits = 2)
            grp1_sd = round(sd(div_table.grp1[, div], na.rm = T), 
                digits = 2)
            grp1_mean_sd = paste("(", grp1_mean, " ± ", grp1_sd, 
                ")", sep = "")
            grp2_mean = round(mean(div_table.grp2[, div], na.rm = T), 
                digits = 2)
            grp2_sd = round(sd(div_table.grp2[, div], na.rm = T), 
                digits = 2)
            grp2_mean_sd = paste("(", grp2_mean, " ± ", grp2_sd, 
                ")", sep = "")
            grp3_mean = round(mean(div_table.grp3[, div], na.rm = T), 
                digits = 2)
            grp3_sd = round(sd(div_table.grp3[, div], na.rm = T), 
                digits = 2)
            grp3_mean_sd = paste("(", grp3_mean, " ± ", grp3_sd, 
                ")", sep = "")
            grp4_mean = round(mean(div_table.grp4[, div], na.rm = T), 
                digits = 2)
            grp4_sd = round(sd(div_table.grp4[, div], na.rm = T), 
                digits = 2)
            grp4_mean_sd = paste("(", grp4_mean, " ± ", grp4_sd, 
                ")", sep = "")
            stat = summary(aov(div_table[, div] ~ div_table$group))
            df = stat[[1]][[1]][[2]]
            t.stat = round(stat[[1]][[4]][[1]], digits = 2)
            p.value = round(stat[[1]][[5]][[1]], digits = 3)
            table1.df[i, 1] = div
            table1.df[i, 2] = all_mean
            table1.df[i, 3] = all_sd
            table1.df[i, 4] = all_mean_sd
            table1.df[i, 5] = grp1_mean
            table1.df[i, 6] = grp1_sd
            table1.df[i, 7] = grp1_mean_sd
            table1.df[i, 8] = grp2_mean
            table1.df[i, 9] = grp2_sd
            table1.df[i, 10] = grp2_mean_sd
            table1.df[i, 11] = grp3_mean
            table1.df[i, 12] = grp3_sd
            table1.df[i, 13] = grp3_mean_sd
            table1.df[i, 14] = grp4_mean
            table1.df[i, 15] = grp4_sd
            table1.df[i, 16] = grp4_mean_sd
            table1.df[i, 17] = t.stat
            table1.df[i, 18] = df
            table1.df[i, 19] = p.value
        }
    }, stop("This function can't accomodate when meta_group>4!"))
    return(table1.df)
  }
}


\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
