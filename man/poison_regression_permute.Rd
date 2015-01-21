\name{poison_regression_permute}
\alias{poison_regression_permute}
\title{poison_regression_permute}
\description{[TEXT]}
\usage{poison_regression_permute(wgs_path_count, n.total, n.gr1, n.gr2, email)}
\arguments{
  \item{wgs_path_count}{[TEXT]}
  \item{n.total}{[TEXT]}
  \item{n.gr1}{[TEXT]}
  \item{n.gr2}{[TEXT]}
  \item{email}{[TEXT]}
}
\details{[TEXT]}
\value{[TEXT]}
\references{[TEXT]}
\author{dlemas}
\note{no further notes}


\seealso{[TEXT]}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (wgs_path_count, n.total, n.gr1, n.gr2, email) 
{
    library(gtools)
    library(mail)
    ptm <- proc.time()
    df.pathway.new = wgs_path_count[, -1]
    rownames(df.pathway.new) = NULL
    colnames(df.pathway.new) = NULL
    data2 = data.matrix(df.pathway.new)
    Y <- c(rep(1, n.gr1), rep(0, n.gr2))
    I <- combinations(n = n.total, r = n.gr2, set = FALSE, repeats.allowed = FALSE)
    permY <- matrix(nrow = nrow(I), ncol = n.total, 0)
    for (i in 1:nrow(permY)) permY[i, I[i, ]] <- 1
    PVALUES <- NULL
    for (i in 1:nrow(data2)) {
        ans <- summary(glm(data2[i, ] ~ Y, family = poisson(link = "log")))
        obs.LRTS <- ans$null.deviance - ans$deviance
        perm.LRTS <- apply(permY, MARGIN = 1, function(x) {
            ans <- summary(glm(data2[i, ] ~ x, family = poisson(link = "log")))
            ans$null.deviance - ans$deviance
        })
        PVALUES <- c(PVALUES, mean(perm.LRTS >= obs.LRTS))
    }
    stop = c(proc.time() - ptm)
    stop[1]
    message.detail = paste("Calculation finished. Total Run Time: ", 
        round(stop[1], digits = 2), " seconds", sep = "")
    email.notification = sendmail(email, subject = "Notification from R: poison_regression_permute complete", 
        message = message.detail, password = "rmail")
    return(wgs_path_count.new = cbind(wgs_path_count, PVALUES))
    return(email.notification)
  }
}

\keyword{ poison }
