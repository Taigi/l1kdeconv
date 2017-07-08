# vim: set noexpandtab tabstop=2:
#' Split the input dataset into several sub list to deconvolution.  
#'
#' Due to the limitation of optimization that too many data would dramatically slow down the speed.
#'
#' @param x a list of numeric vector
#' @param grp_size the normal group size for each group
#' @param lambda_lower the lower bound of \eqn{\lambda}
#' @param lambda_upper the upper bound of \eqn{\lambda}
#' @param sigma_lower the lower bound of \eqn{\sigma}
#' @param debug enable the debug mode to show \code{par} and \code{fn}
#' @keywords distribution
#' @export
#' @examples
#' set.seed(0)
#' x1=c(rnorm(150, mean=0), rnorm(50, mean=10))
#' x2=c(rnorm(150, mean=20), rnorm(50, mean=40))
#' x3=c(rnorm(150, mean=30), rnorm(50, mean=60))
#' x4=c(rnorm(150, mean=30), rnorm(50, mean=60))
#' x5=c(rnorm(150, mean=30), rnorm(50, mean=60))
#' x6=c(rnorm(150, mean=30), rnorm(50, mean=60))
#' x=list(x1, x2, x3, x4, x5, x6)
#' multigmmmanydata(x)
multigmmmanydata = function(
	x
	, grp_size=3
	, lambda_lower = .1
	, lambda_upper = 1-lambda_lower
	, sigma_lower = 1e-2
	, debug=F
	) {
	multigmmsamedistribumulti(
		lapply(splitgrp(length(x), grp_size), function(grp) x[grp])
		, lambda_lower = lambda_lower
		, lambda_upper = lambda_upper
		, sigma_lower = sigma_lower
		, debug = debug
		)
}
