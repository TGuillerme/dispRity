\name{disparity}
\alias{dispRity}

\title{Calculates disparity from an ordinated matrix}

\description{Calculates the user specified metric on an ordinated matrix or series of matrices.}

\usage{
dispRity(data, metric, verbose=FALSE)
}

\arguments{
\item{data}{an ordinated matrix of maximal dimensions \eqn{k\times(k-1)} or a list of matrices (typically output from the functions \link{time.series} or \link{cust.series}).}
\item{metric}{a vector containing the class metric and the summary metric.}
\item{verbose}{a logical value indicating whether to be verbose or not.}
}
\details{\code{metric} should be input as a vector containing both the class and the summary metric. For example use \code{metric=c(prod, variance)} for using the product of variance as a disparity metric.

The class functions is the matrix descriptor the metric is based on (e.g. variance, range, distance from centroid, etc).

The summary function is the summary of the matrix descriptor (e.g. mean, product, etc).}

\seealso{See \link{dispRity.metric} for details on implemented metrics.}

\examples{
## Some example
1+1
}

\author{Thomas Guillerme}