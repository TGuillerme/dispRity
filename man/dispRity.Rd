\name{disparity}
\alias{dispRity}

\title{Calculates disparity from an ordinated matrix}

\description{Calculates the user specified metric on an ordinated matrix or series of matrices.}

\usage{
dispRity(data, metric, boostraps=1000, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full")
}

\arguments{
\item{data}{an ordinated matrix of maximal dimensions \eqn{k\times(k-1) or a list of matrices (typically output from the functions \link{time.series} or \link{cust.series}).}
\item{metric}{a vector containing the class metric and the summary metric.}
\item{boostraps}{the number of bootstrap replicates (default=1000).}
\item{rarefaction}{either a \code{logical} value whether to rarefy the data or a single value to be the maximum of taxa.}
\item{rm.last.axis}{either a \code{logical} value whether to remove the last axis of the ordinated matrix or not or a proportion of axis to save.}
\item{verbose}{a logical value indicating whether to be verbose or not.}
\item{boot.type}{the type of bootstrap to perform. \code{"full"}(default) re-samples the whole matrix each bootstrap, \code{"single"} re-samples only one taxa each bootstrap.}

\details{\code{metric} should be input as a vector containing both the class and the summary metric. For example use \code{metric=c(prod, variance)} for using the product of variance as a disparity metric.

The class functions is the matrix descriptor the metric is based on (e.g. variance, range, distance from centroid, etc).

The summary function is the summary of the matrix descriptor (e.g. mean, product, etc).}

\seealso{See \link{dispRity.metric} for details on implemented metrics.}

\examples{
## Some example
1+1
}

\author{Thomas Guillerme}