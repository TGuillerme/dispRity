\name{boot.matrix}
\alias{bootstrap.matrix}

\title{Bootstraps an rarefied ordinated data}

\description{Bootstraps and rarefies either a single ordinated matrix or a list of ordinated matrices.}

\usage{
boot.matrix(data, boostraps=1000, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full")
}

\arguments{
\item{data}{an ordinated matrix of maximal dimensions \eqn{k\times(k-1) or a list of matrices (typically output from the functions \link{time.series} or \link{cust.series}).}
\item{boostraps}{the number of bootstrap replicates (default=1000).}
\item{rarefaction}{either a \code{logical} value whether to rarefy the data or a single value to be the maximum of taxa.}
\item{rm.last.axis}{either a \code{logical} value whether to remove the last axis of the ordinated matrix or not or a proportion of axis to save.}
\item{verbose}{a logical value indicating whether to be verbose or not.}
\item{boot.type}{the type of bootstrap to perform. \code{"full"}(default) re-samples the whole matrix each bootstrap, \code{"single"} re-samples only one taxa each bootstrap. The type of bootstrap can also be user defined by using the \code{make.boot} function.}

\seealso{\link{make.boot}}

\examples{
## Some example
1+1
}

\author{Thomas Guillerme}