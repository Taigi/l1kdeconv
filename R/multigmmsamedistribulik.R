# vim: set noexpandtab tabstop=2:
#' The sum of Log-Likelihoods of 1D Multi Same Distribution Gaussian Mixture Model
#'
#' The sum of Log-Likelihoods of 1D Multi Same Distribution Gaussian Mixture Model
#'
#' @param x a list of numeric vectors. par contains lambda, sigma, and the parameters for each GMM, delta_mu and mean_mu. For example, supposing there are two GMM, the par would be (lambda, sigma ,mean_mu1, mean_mu2, delta_mu1, delta_mu2).  
#' @keywords distribution
#' @export
#' @examples
#' set.seed(0)
#' x1=c(
#'  rnorm(100, mean=0)
#'  , rnorm(100, mean=1)
#'  )
#' x=list(x1)
#' multigmmsamedistribulik(x)(c(0.5, 1, 0.5, 1))
multigmmsamedistribulik=function(x){
	n = length(x)
	lambda_index = 1L
	sigma_index = 2L
	mu_mean_index = seq_len(n) + 2L
	mu_diff_index = seq_len(n) + n + 2L

	conv_par=function(par) {
		mu_mean = par[mu_mean_index]
		mu_diff = par[mu_diff_index]
		list(
			lambda = par[[lambda_index]]
			, sigma = par[[sigma_index]]
			, mu1 = mu_mean - mu_diff
			, mu2 = mu_mean + mu_diff
			)
	}

	f=function(par) {
		res=with(
			conv_par(par)
			, sum(
				unlist(
					lapply(
						seq_len(n)
						, function(i){
							log(
								lambda * dnorm(x[[i]], mean=mu1[[i]], sd=sigma)
								+ (1-lambda) * dnorm(x[[i]], mean=mu2[[i]], sd=sigma)
								)
						}
						)
					)
				)
			)
		#print(conv_par(par))
		#print(res)
		if(is.infinite(res)) {
			res=-.Machine$double.xmax/10
		}
		res
	}

	attr(f, 'conv_par') = conv_par
	f
}
