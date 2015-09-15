\name{plot.dispRity}
\alias{plot.disparity}

\title{plotting a dispRity object}

\description{Plots a \code{dispRity} object.}

\usage{
plot(data, type="c", CI=c(50,95), cent.tend=mean, ...)
}

\arguments{
\item{data}{a \code{dispRity} object (output from the functions \link{dispRity}).}
\item{type}{either "continuous" ("c"; default) or "discrete" ("d").}
\item{CI}{a vector containing at least one confidence interval value (default = c(50,95)).}
\item{cent.tend}{a function for calculating the central tendency (default = mean).}
\item{...}{any graphical options to be passed to \code{plot}.}
}
\details{\code{type}: if "continuous", disparity is plotted as a continuous measurement along the x axis; if "discrete", disparity is plotted as a boxplot along the x axis.

\seealso{\link{dispRity}, \link{summary.dispRity}}

\examples{
## Some example
1+1
}

\author{Thomas Guillerme}