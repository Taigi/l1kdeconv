# vim: set noexpandtab tabstop=2:
#' Plot the Fit Results of 2-Component Gaussian Mixture Model
#'
#' Plot the Fit Results of 2-Component Gaussian Mixture Model
#'
#' @param x a numeric vector
#' @param mu1 the mean of the 1st cluster
#' @param mu2 the mean of the 2nd cluster
#' @param lambda the proportion parameter
#' @param sigma the common variance of both clusters
#' @param nbins the number of bins per cluster (6*sigma)
#' @param xlim the limitation of x scale
#' @keywords distribution
#' @import ggplot2
#' @export
#' @examples
#' set.seed(0)
#' x=list(c(
#'   rnorm(150, mean=0)
#'   , rnorm(50, mean=10)
#'   ))
#' fit_res=multigmmsamedistribu(x)
#'
#' with(
#'   as.list(fit_res$par_conv)
#'   , gmmplot(x[[1]]
#'     , mu1=mu1
#'     , mu2=mu2
#'     , sigma=sigma
#'     , lambda=lambda
#'     , xlim=range(unlist(x))
#'     )
#'   )
gmmplot=function(x, mu1, mu2, sigma, lambda, nbins=15, xlim) {
	..density.. <- NULL
	ggplot(data=data.frame(x = x)) +
		geom_histogram(aes(x, ..density..), binwidth = 6*sigma/nbins, colour = 'black', fill = 'white') +
		coord_cartesian(xlim=xlim) + 
		stat_function(geom = 'line', fun = mix_comp_fun,
			args = list(mu = mu1, sigma = sigma, lambda = lambda)
			, colour = 'red', lwd = 1.5) +
		stat_function(geom = 'line', fun = mix_comp_fun,
			args = list(mu = mu2, sigma = sigma, lambda = 1-lambda)
			, colour = 'blue', lwd = 1.5) +
		ylab('Density')
}

mix_comp_fun=function(x, mu, sigma, lambda) {
	lambda * dnorm(x, mu, sigma)
}
