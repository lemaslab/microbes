\name{otu_abundance_compare}
\alias{otu_abundance_compare}
\title{This function compares the relative abundance for each bug by study group}
\description{[text]}
\usage{
otu_abundance_compare(otu.normed, meta, grouping_variable, group_a, group_b)
}
\arguments{
  \item{otu.normed}{
[text]}
  \item{meta}{
[text]}
  \item{grouping_variable}{
[text]}
  \item{group_a}{
[text]}
  \item{group_b}{
[text]}
}
\details{
[text]}
\value{
[text]
}
\references{
[text]}
\author{
[text]}
\note{
[text]}


\seealso{
[text]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (otu.normed, meta, grouping_variable, group_a, group_b) 
{
    otu.normed.t = setNames(data.frame(t(otu.normed[, ])), row.names(otu.normed))
    otu.normed.t$study_id <- row.names(otu.normed.t)
    meta.grp = meta[c("study_id", grouping_variable)]
    merged.data <- merge(meta.grp, otu.normed.t, by = "study_id")
    col1 = "OTU_name"
    col2 = paste(group_a, "_mean", sep = "")
    col3 = paste(group_a, "_sd", sep = "")
    col4 = paste(group_a, "_mean_sd", sep = "")
    col5 = paste(group_b, "_mean", sep = "")
    col6 = paste(group_b, "_sd", sep = "")
    col7 = paste(group_b, "_mean_sd", sep = "")
    col8 = "p.value_t.test"
    col9 = "p.value_wilcoxon"
    names <- c(col1, col2, col3, col4, col5, col6, col7, col8, 
        col9)
    TABLE1 <- data.frame(col1 = character(), col2 = numeric(), 
        col3 = numeric(), col4 = numeric(), col5 = numeric(), 
        col6 = numeric(), col7 = numeric(), col8 = numeric(), 
        col9 = numeric(), stringsAsFactors = FALSE)
    TABLE1
    names(TABLE1) <- names
    index = names(merged.data)
    myIndex <- length(index)
    for (i in 3:myIndex) {
        col = index[i]
        grp.a.data = subset(merged.data, merged.data[, 2] == 
            group_a)
        grp.b.data = subset(merged.data, merged.data[, 2] == 
            group_b)
        grp_a_mean = round(mean(grp.a.data[, col], na.rm = T), 
            digits = 4)
        grp_b_mean = round(mean(grp.b.data[, col], na.rm = T), 
            digits = 4)
        grp_a_sd = round(sd(grp.a.data[, col], na.rm = T), digits = 4)
        grp_b_sd = round(sd(grp.b.data[, col], na.rm = T), digits = 4)
        grp_a_mean_sd = paste("(", grp_a_mean, " ± ", grp_a_sd, 
            ")", sep = "")
        grp_b_mean_sd = paste("(", grp_b_mean, " ± ", grp_b_sd, 
            ")", sep = "")
        stats.t = t.test(merged.data[, col] ~ merged.data[, 2])
        p.value_t.test = round(stats.t$p.value, digits = 4)
        stats.w = wilcox.test(merged.data[, col] ~ merged.data[, 
            2])
        p.value_wilcoxon = round(stats.w$p.value, digits = 4)
        continuous_block = cbind(col, grp_a_mean, grp_a_sd, grp_a_mean_sd, 
            grp_b_mean, grp_b_sd, grp_b_mean_sd, p.value_t.test, 
            p.value_wilcoxon)
        continuous_block
        TABLE1[i, 1] = col
        TABLE1[i, 2] = grp_a_mean
        TABLE1[i, 3] = grp_a_sd
        TABLE1[i, 4] = grp_a_mean_sd
        TABLE1[i, 5] = grp_b_mean
        TABLE1[i, 6] = grp_b_sd
        TABLE1[i, 7] = grp_b_mean_sd
        TABLE1[i, 8] = p.value_t.test
        TABLE1[i, 9] = p.value_wilcoxon
    }
    TABLE1
    TABLE2 <- TABLE1[-(1:2), ]
    return(TABLE2)
  }
}

\keyword{ [text] }
\keyword{ [text] }
