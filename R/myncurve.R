#' @title a function which creates a curve and finds the density of a given lower tail probability
#'
#' @param mu the mean
#' @param sigma the standard devation
#' @param a the value at which the find a lower tail probability
#'
#' @return a list containing the area
#' @export
#'
#' @examples
#' \dontrun{getsamplezscores(mu=5, sigma=1, a=5)}
myncurve=function(mu, sigma, a){

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma,mu+3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a), c(0,ycurve,0),col="Red")

  area=pnorm(a, mean=mu, sd=sigma)
  return(list(area))
}
