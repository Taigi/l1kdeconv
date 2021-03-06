% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multigmmmanydata.R
\name{multigmmmanydata}
\alias{multigmmmanydata}
\title{Split the input dataset into several sub list to deconvolution.}
\usage{
multigmmmanydata(x, grp_size = 3, lambda_lower = 0.1, lambda_upper = 1 -
  lambda_lower, sigma_lower = 0.01, debug = F)
}
\arguments{
\item{x}{a list of numeric vector}

\item{grp_size}{the normal group size for each group}

\item{lambda_lower}{the lower bound of \eqn{\lambda}}

\item{lambda_upper}{the upper bound of \eqn{\lambda}}

\item{sigma_lower}{the lower bound of \eqn{\sigma}}

\item{debug}{enable the debug mode to show \code{par} and \code{fn}}
}
\description{
Due to the limitation of optimization that too many data would dramatically slow down the speed.
}
\examples{
set.seed(0)
x1=c(rnorm(150, mean=0), rnorm(50, mean=10))
x2=c(rnorm(150, mean=20), rnorm(50, mean=40))
x3=c(rnorm(150, mean=30), rnorm(50, mean=60))
x4=c(rnorm(150, mean=30), rnorm(50, mean=60))
x5=c(rnorm(150, mean=30), rnorm(50, mean=60))
x6=c(rnorm(150, mean=30), rnorm(50, mean=60))
x=list(x1, x2, x3, x4, x5, x6)
multigmmmanydata(x)
}
\keyword{distribution}
