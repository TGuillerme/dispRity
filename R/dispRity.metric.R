#' @name dispRity.metric
#' @aliases variances ranges centroids mode.val
#'
#' @title Disparity metrics
#'
#' @description Different implemented disparity metrics.
#'
#' @usage mat.trans(matrix)
#' vec.aggr(matrix)
#' val.aggr(X)
#'
#' @param matrix A matrix.
#' @param X A vector or a matrix.
#'
#' @details
#' These are inbuilt functions for calculating disparity. See \code{\link{make.metric}} for details on \code{mat.trans}, \code{vec.aggr} and \code{vec.aggr}.
#' The currently implemented vector aggregate metrics (\code{vec.aggr}) are:
#' \itemize{
#'   \item \code{ranges}: calculates the range of each axis of the matrix.
#'   \item \code{variances}: calculates the variance of each axis of the matrix.
#'   \item \code{centroids}: calculates the euclidean distance between each row and the centroid of the matrix.
#' }
#' The currently implemented value aggregate metrics (\code{val.aggr}) are:
#' \itemize{
#'   \item \code{mode.val}: calculates the modal value of a vector.
#'   \item \code{volume}: calculates the volume of a matrix.
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
variances<-function(matrix) {
    return(apply(matrix, 2, var))
}

#Calculating each axis ranges
ranges<-function(matrix) {
    #Max values
    max_values<-apply(matrix, 2, max)
    #Min values
    min_values<-apply(matrix, 2, min)
    #Ranges values
    ranges<-abs(max_values-min_values)
    return(ranges)
}

#Calculating the distance from centroid
centroids<-function(matrix) {

    #Calculating the centroid point
    centroid<-apply(matrix, 2, mean)

    #Calculating the distance from centroid
    cent.dist<-NULL
    for (j in 1:nrow(matrix)){
        cent.dist[j] <- dist(rbind(matrix[j,], centroid), method="euclidean")
    }

    return(cent.dist)
}

# Summary functions:
# These functions should take a vector as an input and output a single numeric character

mode.val<-function(X){
    return(as.numeric(names(sort(-table(X))[1])))
}
