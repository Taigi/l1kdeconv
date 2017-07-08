# vim: set noexpandtab tabstop=2:
#' Plot the Fit Results of aggregate 2-Component Gaussian Mixture Model
#'
#' Plot the Fit Results of aggregate 2-Component Gaussian Mixture Model
#'
#' @param x a list of a numeric vector
#' @param fit_res the result of AGMM
#' @param nbins the number of bins per cluster
#' @keywords distribution
#' @export
#' @examples 
#' params=list(
#'  c(mu1=0, mu2=10, sd = 1)
#'  , c(mu1=10, mu2=20, sd = 1)
#'  )
#' 
#' set.seed(0)
#' x=lapply(
#'   params
#'   , function(v) {
#'     c(
#'       rnorm(100, mean=v[['mu1']], sd = v[['sd']])
#'       , rnorm(50, mean=v[['mu2']], sd = v[['sd']])
#'       )
#'   }
#'   )
#' multigmmplot(x, multigmmsamedistribu(x))
multigmmplot=function(x, fit_res, nbins=15) {
	with(
		as.list(fit_res$par_conv)
		, lapply(
			seq_along(x)
			, function(i) {
				gmmplot(
					x[[i]]
					, mu1 = mu1[[i]]
					, mu2 = mu2[[i]]
					, sigma = sigma
					, lambda = lambda
					, nbins = nbins
					, xlim = range(unlist(x))
					)
			}
			)
		)
}


