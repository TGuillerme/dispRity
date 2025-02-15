% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dispRity.R
\name{dispRity}
\alias{dispRity}
\title{Calculates disparity from a matrix.}
\usage{
dispRity(
  data,
  metric,
  dimensions = NULL,
  ...,
  between.groups = FALSE,
  dist.data = NULL,
  verbose = FALSE,
  tree = NULL
)
}
\arguments{
\item{data}{A matrix or a \code{dispRity} object (see details).}

\item{metric}{A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details).}

\item{dimensions}{Optional, a vector of \code{numeric} value(s) or the proportion of the dimensions to keep.}

\item{...}{Optional arguments to be passed to the metric.}

\item{between.groups}{A \code{logical} value indicating whether to run the calculations between groups (\code{TRUE}) or not (\code{FALSE} - default) or a \code{numeric} list of pairs of groups to run (see details).}

\item{dist.data}{A \code{logical} value indicating whether to treat the data as distance data (\code{TRUE}) or not (\code{FALSE}). By default it is set to \code{NULL} and interprets whether to use distance data from \code{data}.}

\item{verbose}{A \code{logical} value indicating whether to be verbose or not.}

\item{tree}{\code{NULL} (default) or an optional \code{phylo} or \code{multiPhylo} object to be attached to the data. If this argument is not null, it will be recycled by \code{metric} when possible.}
}
\value{
This function outputs a \code{dispRity} object containing at least the following:
\item{matrix}{the multidimensional space (a list of \code{matrix}).}
\item{call}{A \code{list} containing the called arguments.}
\item{subsets}{A \code{list} containing matrices pointing to the elements present in each subsets.}
\item{disparity}{A \code{list} containing the disparity in each subsets.}

Use \link{summary.dispRity} to summarise the \code{dispRity} object.
}
\description{
Calculates disparity from a matrix, a list of matrices or subsets of a matrix, where the disparity metric can be user specified.
}
\details{
The \code{dispRity} object given to the \code{data} argument can be: a list of matrices (typically output from the functions \code{\link{chrono.subsets}} or \code{\link{custom.subsets}}), a bootstrapped matrix output from \code{\link{boot.matrix}}, a list of disparity measurements calculated from the \code{dispRity} function or a \code{matrix} object with rows as elements and columns as dimensions. In any of these cases, the data is considered as the multidimensional space and is not transformed (e.g. if ordinated with negative eigen values, no correction is applied to the matrix).

\code{metric} should be input as a vector of functions.
The functions are sorted and used by dimension-level from 3 to 1 (see \code{\link{dispRity.metric}} and \code{\link{make.metric}}).
Typically dimension-level 3 functions take a \code{matrix} and output a \code{matrix}; dimension-level 2 functions take a \code{matrix} and output a \code{vector} and dimension-level 1 functions take a \code{matrix} or a \code{vector} and output a single value.
When more than one function is input, they are treated first by dimension-level (i.e. 3, 2 and finally 1).
Note that the functions can only take one metric of each dimension-level and thus can only take a maximum of three arguments!

Some metric functions are built into the \code{dispRity} package: see \code{\link{dispRity.metric}}
For user specified metrics, please use \code{\link{make.metric}} to ensure that the metric will work.

\emph{HINT:} if using more than three functions you can always create your own function that uses more than one function (e.g. \code{my_function <- function(matrix) cor(var(matrix))} is perfectly valid and allows one to use two dimension-level 3 functions - the correlation of the variance-covariance matrix in this case).

The \code{between.groups} argument runs the disparity between groups rather within groups. If \code{between.groups = TRUE}, the disparity will be calculated using the following inputs:
\itemize{
     \item if the input is an output from \code{\link{custom.subsets}}, the series are run in a pairwise manner using \code{metric(matrix, matrix2)}. For example for a \code{custom.subset} contains 3 subsets m1, m2 and m3, the code loops through: \code{metric(m1, m2)}, \code{metric(m2, m3)} and \code{metric(m1, m3)} (looping through \code{list(c(1,2), c(2,3), c(3,1))}).
     \item if the input is an output from \code{\link{chrono.subsets}}, the series are run in a paired series manner using \code{metric(matrix, matrix2)}. For example for a \code{chrono.subsets} contains 3 subsets m1, m2, m3 and m4, the code loops through: \code{metric(m1, m2)} and \code{metric(m2, m3)} (looping through \code{list(c(1,2), c(2,3), c(3,4))}).
}
In both cases it is also possible to specify the input directly by providing the list to loop through. For example using \code{between.groups = list(c(1,2), c(2,1), c(4,8))} will apply the \code{metric} to the 1st and 2nd subsets, the 2nd and first and the 4th and 8th (in that specific order).
}
\examples{
## Load the Beck & Lee 2014 data
data(BeckLee_mat50)

## Calculating the disparity as the sum of variances from a single matrix
sum_of_variances <- dispRity(BeckLee_mat50, metric = c(sum, variances))
summary(sum_of_variances)
## Bootstrapping this value
bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps = 100)
dispRity(bootstrapped_data, metric = c(sum, variances))

## Calculating the disparity from a customised subset
## Generating the subsets
customised_subsets <- custom.subsets(BeckLee_mat50,
     list(group1 = 1:(nrow(BeckLee_mat50)/2),
          group2 = (nrow(BeckLee_mat50)/2):nrow(BeckLee_mat50)))
## Bootstrapping the data
bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 100)
## Calculating the sum of variances
sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
summary(sum_of_variances)

## Calculating disparity with different metrics of different dimension-levels
## Disparity is calculated as the distribution of the variances in each
## dimension (output are distributions)
disparity_level2 <- dispRity(BeckLee_mat50, metric = variances)
## Disparity is calculated as the mean of the variances in each dimension 
## (output are single values)
disparity_level1 <- dispRity(disparity_level2, metric = mean)
## Both disparities have the same means but dimension-level 1 has no quantiles
summary(disparity_level2)
summary(disparity_level1)


}
\seealso{
\code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}}.
}
\author{
Thomas Guillerme
}
