# vim: set noexpandtab tabstop=2:
#' Split the input dataset into several sub list to deconvolution.  
#'
#' Due to the limitation of optimization that too many data would dramatically slow down the speed.
#'
#' @param x a list of numeric vector
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
multigmmsamedistribumulti = function(
	x
	, lambda_lower =.1
	, lambda_upper = 1-lambda_lower
	, sigma_lower = 1e-2
	, debug=F
	) {
	tmp = lapply(
		x
		, function(u){
			multigmmsamedistribu(u, lambda_lower, lambda_upper, sigma_lower, debug)
		})

	if(any(sapply(tmp, function(x) x$convergence)!=0)) {
		stop('multigmmsamedistribumulti():optim not converged.')
	}

	lambdas = sapply(tmp, function(x) { x$par_conv$lambda })

	if(!(all(lambdas<=.5) | all(lambdas>=.5))) {
		stop('multigmmsamedistribumulti():lambdas are not all below .5 nor all above .5')
	}
	list(
		convergence = 0
		, par_conv = list(
			lambda = unlist(lapply(tmp, function(x) { x$par_conv$lambda }))
			, mu1 = unlist(lapply(tmp, function(x) { x$par_conv$mu1 }))
			, mu2 = unlist(lapply(tmp, function(x) { x$par_conv$mu2 }))
			)
		)
}
