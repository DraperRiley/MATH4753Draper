#' @title A function which generates a binomial distribution a given number of times
#'
#' @param iter the desired number of iterations
#' @param n number of observations
#' @param p probability
#'
#' @return a table with n entries containing probabilities
#' @export
#'
#' @examples
#' \dontrun{mybin(iter=100, n=10, p=0.5)}
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  graphics::barplot(succ.tab/(iter), col=grDevices::rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
