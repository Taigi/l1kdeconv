# vim: set noexpandtab tabstop=2:
#' Get the Cluster Ranges in a Vector of 1D Coordinates
#'
#' Get the Cluster Ranges in a Vector of 1D Coordinates
#'
#' @param x a numeric vector
#' @param gap the size for the recognation of data free gaps
#' @keywords distribution
#' @export
#' @examples
#' x = c(1:3, 11:13)
#' getclusterranges(x, 3)
getclusterranges=function(x, gap) {
	is_gap = diff(x) > gap
	data.frame(
		left=x[c(T, is_gap)]
		, right=x[c(is_gap, T)]
		)
}

