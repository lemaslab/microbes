# **************************************************************************** #
# **************************************************************************** #

# normalize_counts.R

# **************************************************************************** #
# ***************              START FUNCTION                  *************** #
# **************************************************************************** #


normalize_counts <- function(otu_count) {

otu.normed =otu_count
index=names(otu.normed)
myIndex<-length(index)
n <- length(row.names(otu.normed))
m <- length(otu.normed)
# Start the Loop
for (i in 1:m)
{
  # Create column index
  col=index[i]
  n <- length(row.names(otu.normed))
  otu.normed[1:n,i] <- (otu.normed[1:n,i])/sum(otu.normed[1:n,i])
}

return(otu.normed)

} # End function

# test=normalize_counts(infant.cog2[-1])
# head(test)
# sum(test[1])

# Test
# test=normalize_counts(wgs.cog.counts)