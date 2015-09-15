\name{summary.dispRity}
\alias{summary.disparity}

\title{dispRity object summary}

\description{Creates a summary of a \code{dispRity} object.}

\usage{
summary(data, CI=c(50,95), cent.tend=mean, recall=FALSE, round)
}

\arguments{
\item{data}{a \code{dispRity} object (output from the functions \link{dispRity}).}
\item{CI}{a vector containing at least one confidence interval value (default = c(50,95)).}
\item{cent.tend}{a function for calculating the central tendency (default = mean).}
\item{recall}{logical, whether to recall the \code{dispRity} object attributes.}
\item{round}{optional argument for forcing the number of decimals in the summary (can be missing, see details).}
}
\details{\code{round}: if missing, the number of decimals are automatically determined such as the total number of digit in each column is equal to 4.

\seealso{\link{dispRity}.}

\examples{
## Some example
1+1
}

\author{Thomas Guillerme}