# ' @name dispRity.metric
# ' @aliases variances ranges centroids mode.val
# '
# ' @title Disparity metrics
# '
# ' @description Different implemented disparity metrics.
# '
# ' @param X Something.
# '
# ' @details
# ' These are inbuilt functions for calculating disparity. See \code{\link{make.metric}} for details on \code{level3.fun}, \code{level2.fun} and \code{level1.fun}.
# ' The currently implemented vector aggregate metrics (\code{level2.fun}) are:
# ' \itemize{
# '   \item \code{ranges}: calculates the range of each axis of the matrix.
# '   \item \code{variances}: calculates the variance of each axis of the matrix.
# '   \item \code{centroids}: calculates the euclidean distance between each row and the centroid of the matrix.
# ' }
# ' The currently implemented value aggregate metrics (\code{level1.fun}) are:
# ' \itemize{
# '   \item \code{mode.val}: calculates the modal value of a vector.
# '   \item \code{volume}: calculates the volume of a matrix.
# ' }
# ' See also \code{\link[base]{mean}}, \code{\link[stats]{median}}, \code{\link[base]{sum}} or \code{\link[base]{prod}} for commonly used summary metrics.
# ' 
# '
# ' @examples
# ' ## 
# '
# ' @seealso \code{\link{dispRity}} and \code{\link{make.metric}}.
# '
# ' @author Thomas Guillerme

#Calculating each axis variance
dispRity.tests <- function(X) return(X)
