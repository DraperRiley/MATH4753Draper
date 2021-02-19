#' @title A function which returns a vector of z-scores
#'
#' @param x a vector of data
#'
#' @return a vector of z-scores
#' @export
#'
#' @examples
#' \dontrun{x <- 1:100; getsamplezscores(x=d)}
getsamplezscores=function(x){
  z = (x - mean(x)) / stats::sd(x)
  return(z)
}
