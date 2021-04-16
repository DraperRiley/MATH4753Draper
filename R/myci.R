#' @title A function which creates a confidence interval for the mean
#'
#' @param x a vector of data
#'
#' @return a confidence interval
#' @export
#'
#' @examples
#' \dontrun{sam=qnorm(30,mean=10,sd=12);myci(sam)}
myci=function(x){

  #calculating the ci
  result = mean(x) + c(-1,1)*qt(1-0.05/2, length(x)-1)*sd(x)/sqrt(length(x))

  #return result
  return(result)
}
