#' @name dispRity.metric
#' @aliases variances ranges centroids mode.val
#'
#' @title Disparity metrics
#'
#' @description Different implemented disparity metrics.
#'
#' @usage level3.fun(matrix)
#' level2.fun(matrix)
#' level1.fun(X)
#'
#' @param matrix A matrix.
#' @param X A vector.
#'
#' @details
#' These are inbuilt functions for calculating disparity. See \code{\link{make.metric}} for details on \code{level3.fun}, \code{level2.fun} and \code{level1.fun}.
#' The currently implemented matrix aggregate metrics (\code{level3.fun}) are:
#' \itemize{
#'   \item \code{hyper.volume}: calculates the hyperdimensional volume of a matrix.
#'      \itemize{
#'          \item WARNING: this function only calculates the exact volume from MDS or PCO (PCoA) ordinations (e.g. \code{\link[stats]{cmdscale}}, \code{\link[ape]{pcoa}})
#'      }
#' }
#' The currently implemented vector aggregate metrics (\code{level2.fun}) are:
#' \itemize{
#'   \item \code{ranges}: calculates the range of each axis of the matrix.
#'   \item \code{variances}: calculates the variance of each axis of the matrix.
#'   \item \code{centroids}: calculates the euclidean distance between each row and the centroid of the matrix.
#' }
#' The currently implemented value aggregate metrics (\code{level1.fun}) are:
#' \itemize{
#'   \item \code{mode.val}: calculates the modal value of a vector.
#' }
#' See also \code{\link[base]{mean}}, \code{\link[stats]{median}}, \code{\link[base]{sum}} or \code{\link[base]{prod}} for commonly used summary metrics.
#' 
#'
#' @examples
#' ## 
#'
#' @seealso \code{\link{dispRity}} and \code{\link{make.metric}}.
#'
#' @author Thomas Guillerme

#Calculating each axis variance
variances <- function(matrix) {
    return(apply(matrix, 2, var))
}

#Calculating each axis ranges
ranges <- function(matrix) {
    #Max values
    max_values <- apply(matrix, 2, max)
    #Min values
    min_values <- apply(matrix, 2, min)
    #Ranges values
    ranges <- abs(max_values-min_values)
    return(ranges)
}

#Calculating the distance from centroid
centroids <- function(matrix) {
    #Calculating the centroid point
    centroid <- apply(matrix, 2, mean)

    #Calculating the distance from centroid
    cent.dist <- NULL
    for (j in 1:nrow(matrix)){
        cent.dist[j] <- dist(rbind(matrix[j,], centroid), method="euclidean")
    }

    return(cent.dist)
}

# Calculate the mode of a vector
mode.val <- function(X){
    return(as.numeric(names(sort(-table(X))[1])))
}

# Calculate the volume of an eigen matrix (modified from Donohue et al 2013, Ecology Letters)
hyper.volume <- function(matrix) {
    # The eigen value is equal to the sum of the variance/covariance within each axis
    # multiplied by the maximum number of dimensions (k-1) - ONLY WORKS FOR MDS OR PCO!
    eigen.value<-abs(apply(var(matrix),2, sum)*(nrow(matrix)-1))

    #volume (from Donohue et al 2013, Ecology Letters)
    volume<-pi^(ncol(matrix)/2)/gamma((ncol(matrix)/2)+1)*prod(eigen.value^(0.5))

    return(volume)
}