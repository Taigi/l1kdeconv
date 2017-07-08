# vim: set noexpandtab tabstop=2:
#' Fit Multi 2-Component Gaussian Mixture Model in same distribution with a Fixed Proportion
#'
#' Fit Multi 2-Component Gaussian Mixture Model in same distribution with a Fixed Proportion
#'
#' @param x a list of numeric vector
#' @param lambda_lower the lower bound of \eqn{\lambda}
#' @param lambda_upper the upper bound of \eqn{\lambda}
#' @param sigma_lower the lower bound of \eqn{\sigma}
#' @param debug enable the debug mode to show \code{par} and \code{fn}
#' @keywords distribution
#' @export
#' @import mixtools
#' @examples
#' set.seed(0)
#' x1=c(rnorm(150, mean=0), rnorm(50, mean=10))
#' x2=c(rnorm(150, mean=20), rnorm(50, mean=40))
#' x3=c(rnorm(150, mean=30), rnorm(50, mean=60))
#' x=list(x1, x2, x3)
#' multigmmsamedistribu(x)
multigmmsamedistribu = function(
	x
	, lambda_lower =.1
	, lambda_upper = 1-lambda_lower
	, sigma_lower = 1e-2
	, debug=F
	) {
	n = length(x)
	cls = lapply(x, normalmixEM, maxrestarts=100000, epsilon = 1e-04)
	mu1s = sapply(cls, function(x) x$mu[[1]])
	mu2s = sapply(cls, function(x) x$mu[[2]])
	sigma = mean(sapply(cls,function(x) x$sigma))
	lambda = .5

	mean_mu = (mu1s+mu2s)/2
	delta_mu = ifelse(mu1s > mu2s, mu1s-mu2s, mu2s-mu1s)/2

	par = c(lambda, sigma, mean_mu, delta_mu)

	lower = c(lambda_lower, sigma_lower, rep(-Inf, n), rep(0, n))
	upper = c(lambda_upper, Inf, rep(Inf, n), rep(Inf, n))

	if(debug) {
		f = function(x) {
			f1 = multigmmsamedistribulik(x)
			print(attr(f1, 'conv_par')(x))
			res = f1(x)
			print(res)
			res
		}
	} else {
		f = multigmmsamedistribulik(x)
	}

	res=optim(
		par
		, f
		, method = 'L-BFGS-B'
		, lower = lower
		, upper = upper
		, control=list(
			fnscale=-1
			, maxit=.Machine$integer.max
			, ndeps=rep(1e-8, length(par))
			)
		)

	res$par_conv=attr(f, 'conv_par')(res$par)
	res
}
