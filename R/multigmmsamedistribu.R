# vim: set noexpandtab tabstop=2:
#' Fit Multi 2-Component Gaussian Mixture Model in same distribution with a Fixed Proportion
#'
#' Fit Multi 2-Component Gaussian Mixture Model in same distribution with a Fixed Proportion
#'
#' @param x a list of numeric vector
#' @param lower the lower bound of sigma
#' @keywords distribution
#' @export
#' @examples
#' set.seed(0)
#' x1=c(rnorm(150,mean=0),rnorm(50,mean=10))
#' x2=c(rnorm(150,mean=20),rnorm(50,mean=40))
#' x3=c(rnorm(150,mean=30),rnorm(50,mean=60))
#' x=list(x1,x2,x3)
#' multigmmsamedistribu(x)
multigmmsamedistribu = function(x, lower=0.1) {
	n = length(x)
	cls = lapply(x, kmeans, centers=2)

	mu1s = sapply(cls, function(x) x$centers[[1]])
	mu2s = sapply(cls, function(x) x$centers[[2]])
	sigma = sum(sapply(cls, function(x) x$withinss))/sum(sapply(x, length)-2)
	lambda = .5

	mean_mu = (mu1s+mu2s)/2
	delta_mu = ifelse(mu1s > mu2s, mu1s-mu2s, mu2s-mu1s)/2

	par = c(lambda, sigma, mean_mu, delta_mu)

	lower = c(0+1e-8, lower, rep(-Inf, n), rep(0, n))
	upper = c(1-1e-8, Inf, rep(Inf, n), rep(Inf, n))

	f=multigmmsamedistribulik(x)

	res=optim(
		par
		, f
		, method = 'L-BFGS-B'
		, lower = lower
		, upper = upper
		, control=list(
			fnscale=-1
			, maxit=.Machine$integer.max
			, ndeps=rep(1e-8, length(lower))
			)
		)

	res$par_conv=attr(f, 'conv_par')(res$par)
	res
}
