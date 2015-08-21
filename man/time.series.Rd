\name{time.series}
\alias{time.series}

\title{Separating ordinated data in time series.}

\description{Splits the ordinated data into a time series list.}

\usage{
time.series(data, tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE)
}

\arguments{
\item{data}{an ordinated matrix of maximal dimensions \eqn{k\times(k-1)}.}
\item{tree}{a \code{phylo} object matching the data and with a \code{root.time} element.}
\item{method}{the time series method: either \code{"discrete"} (or \code{"d"}) or \code{"continuous"} (or \code{"c"}).}
\item{time}{either a single \code{integer} for them number of discrete or continuous samples or a \code{vector} containing the age of each sample.}
\item{model}{one of the following models: \code{"acctran"}, \code{"deltran"}, \code{"punctuated"} or \code{"gradual"}. Is ignored if \code{method = "discrete"}.}
\item{inc.nodes}{a logical value indicating whether nodes should be included in the time series. Is ignored if \code{method = "continuous"}}
\item{FALAD}{an optional \code{data.frame} containing the first and last occurrence data.}
\item{verbose}{a logical value indicating whether to be verbose or not. Is ignored if \code{method = "discrete"}.}
}

\details{
If \code{method = "continuous"} and when the sampling is done along an edge of the tree, the ordinated data selected for the time series is:
    
    "acctran": always the one of the ancestral node.
    
    "deltran": always the one of the offspring node or tip.
    
    "punctuated": randomly selected from the ancestral node or the offspring node or tip.
    
    "gradual": either the ancestral node if the sampling point on the edge is \eqn{\< edge.length/2} else the offspring node or tip.
}

\seealso{\link{cust.series} for custom time series.}

\examples{
## Some example
1+1
}

\author{Thomas Guillerme}