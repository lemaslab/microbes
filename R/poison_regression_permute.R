
#' poison_regression_permute.R
#' @param A linear regression model
#' @keywords poison 
#' @export
#' @examples

# **************************************************************************** #
# ***************              START FUNCTION                  *************** #
# **************************************************************************** #

poison_regression_permute <- function(wgs_path_count, n.total, n.gr1, n.gr2) {

  
  # Start the clock!
  ptm <- proc.time()

  # Process the data
  # df.pathway.new=wgs_path_count[,-1]
  df.pathway.new=wgs_path_count
  
  rownames(df.pathway.new)=NULL
  colnames(df.pathway.new)=NULL
  data2=data.matrix(df.pathway.new)
  
  
  Y <- c(rep(1,n.gr1), rep(0,n.gr2))  # predictor, i.e., grouping variable assumes col are sorted
                                      # according to grouping variable
  I <- combinations(n=n.total, r=n.gr2, set=FALSE, repeats.allowed=FALSE)
	permY <- matrix(nrow=nrow(I),ncol=n.total,0)
	for(i in 1:nrow(permY))
		permY[i,I[i,]] <- 1 	

	# permY

# for each pathway, i.e., row of X compute the observed statistic
# and compute the statistic for all possible permutations
# save pvalue in matrix PVALUES

	PVALUES <- NULL
  BETAS   <- NULL    #new
  tot <- apply(data2,MARGIN=2,sum)     #new

	for(i in 1:nrow(data2))
	{
	  ans <- summary(glm(data2[i,] ~ Y, family=poisson(link = "log"), offset=log(tot))) #modified
		obs.LRTS <- ans$null.deviance - ans$deviance
		BETAS <- c(BETAS,ans$coefficients[2,1])      #new

		perm.LRTS <- apply(permY, MARGIN=1, function(x) {		
		    ans <- summary(glm(data2[i,] ~ x, family=poisson(link = "log"), offset=log(tot))) #modified
		                   ans$null.deviance - ans$deviance })

		PVALUES <- c(PVALUES, mean(perm.LRTS >= obs.LRTS))
	}

# Stop the clock
stop=c(proc.time() - ptm)
stop[1]

# Email Notification
#message.detail=paste("Calculation finished. Total Run Time: ",round(stop[1],digits=2)," seconds",sep="")
#email.notification=sendmail(email, subject="Notification from R: poison_regression_permute complete", 
#               message=message.detail, password="rmail")

  # Combine P.values with counts
  return(wgs_path_count.new=cbind(wgs_path_count,BETAS,PVALUES))

  # Send email notification
#  return(email.notification)

} # End of Function



