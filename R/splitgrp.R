# vim: set noexpandtab tabstop=2:
#' Split a list with size n into groups with at least m elements
#'
#' Split a list with size n into groups with at least m elements
#'
#' @param n an integer indicating the total length
#' @param m the min group size
#' @keywords category
#' @export
#' @examples
#' 
#' splitgrp(1, 2)
#' splitgrp(2, 2)
#' splitgrp(3, 2)
splitgrp=function(n, m) {
	x = seq_len(n)
	if(n < m) {
		list(`1`=x)
	} else {
		grp_ids = seq_len(floor(n/m))
		grp_lens=sapply(
			suppressWarnings(split(x, grp_ids))
			, length
			)
		split(x
			, rep(grp_ids, grp_lens)
			)
	}
}
