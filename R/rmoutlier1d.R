# vim: set noexpandtab tabstop=2:
#' Remove the Outliers in a Vector of 1D Coordinates
#'
#' Remove the Outliers in a Vector of 1D Coordinates
#'
#' @param x a numeric vector
#' @param dy_thr threshold for y
#' @param clustersize_thr threshod for outlier cluster size
#' @param gapsize threshod for gap size
#' @keywords distribution
#' @export
#' @examples
#' x=c(1,10:30,50)
#' par(mfrow=c(2,1))
#' plot(density(x))
#' plot(density(rmoutlier1d(x)))
rmoutlier1d=function(
	x
	, dy_thr=dnorm(4)
	, clustersize_thr=3
	, gapsize=10
	) {

	d = density(x)
	dx = d$x
	dy = d$y

	delta = diff(dx[1:2])
	cluster_ranges = getclusterranges(
		dx[dy * delta > dy_thr]
		, gapsize * delta
		)

	raw_clusters=lapply(
		seq_len(nrow(cluster_ranges))
		, function(i) {
			x[cluster_ranges[i, 'left'] <= x & x <= cluster_ranges[i, 'right']]
		}
		)
	unlist(raw_clusters[sapply(raw_clusters, length)>=clustersize_thr])
}
