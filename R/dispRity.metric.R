#' @name dispRity.metric
#' @aliases variances ranges centroids mode.val ellipse.volume convhull.surface convhull.volume hyper.volume diagonal dimension.level3.fun dimension.level2.fun dimension.level1.fun
#' @title Disparity metrics
#'
#' @description Different implemented disparity metrics.
#'
#' @usage dimension.level3.fun(matrix, ...)
#' dimension.level2.fun(matrix, ...)
#' dimension.level1.fun(X, ...)
#'  
#' @param matrix A matrix.
#' @param X A vector.
#' @param ... Optional arguments to be passed to the function.
#'
#' @details
#' These are inbuilt functions for calculating disparity. See \code{\link{make.metric}} for details on \code{dimension.level3.fun}, \code{dimension.level2.fun} and \code{dimension.level1.fun}.
#' 
#' The currently implemented dimension-level 1 metrics are:
#' \itemize{
#'   \item \code{ellipse.volume}: calculates the ellipsoid volume of a matrix.
#'      \itemize{
#'          \item WARNING: this function only calculates the exact volume from MDS or PCO (PCoA) ordinations (e.g. \code{\link[stats]{cmdscale}}, \code{\link[ape]{pcoa}})
#'      }
#'   \item \code{convhull.surface}: calculates the convex hull hypersurface of a matrix.
#'   \item \code{convhull.volume}: calculates the convex hull hypervolume of a matrix.
#'      \itemize{
#'          \item Both \code{convhull} functions are based on the \code{\link[geometry]{convhulln}} function
#'          \item WARNING: both \code{convhull} functions can be computationally intensive!
#'      }
#'   \item \code{hyper.volume}: calculates the hypervolume using the \code{\link[hypervolume]{hypervolume}} algorithm. If no optional argument is given, the different arguments are set by default to:
#'      \itemize{
#'          \item \code{method = "box"} see \code{link[hypervolume]{hypervolume}} for more details
#'          \item \code{print.output = FALSE} whether to print (\code{TRUE}) or capture (\code{FALSE}) the verbose output. 
#'      }
#'   \item \code{diagonal}: calculates the longest distance in the ordinated space.
#'      \itemize{
#'          \item WARNING: This function is the generalisation of Pythagoras' theorem and thus \bold{works only if each dimensions are orthogonal to each other}.
#'      }
#'   \item \code{mode.val}: calculates the modal value of a vector.
#' }
#' 
#'  See also \code{\link[base]{mean}}, \code{\link[stats]{median}}, \code{\link[base]{sum}} or \code{\link[base]{prod}} for commonly used summary metrics.
#' 
#' The currently implemented dimension-level 2 metrics are:
#' \itemize{
#'   \item \code{ranges}: calculates the range of each axis of the matrix. An optional argument, \code{k.root}, can be set to \code{TRUE} to scale the ranges by using its \eqn{kth} root (where \eqn{k} are the number of dimensions). By default, \code{k.root = FALSE}.
#'   \item \code{variances}: calculates the variance of each axis of the matrix. This function can also take the \code{k.root} optional argument described above.
#'   \item \code{centroids}: calculates the Euclidean distance between each row and the centroid of the matrix. This function can take an optional arguments \code{centroid} for defining the centroid (if missing (default), the centroid of the matrix is used). This argument can be either a subsample of coordinates matching the matrix's dimensions (e.g. \code{c(0, 1, 2)} for a matrix with three columns) or a single value to be the coordinates of the centroid (e.g. \code{centroid = 0} will set the centroid coordinates to \code{c(0, 0, 0)} for a three dimensional matrix).
#' }
#'
#' @examples
#' ## A dummy matrix
#' dummy_matrix <- matrix(rnorm(90), 10, 9)
#' 
#' ## variances of a each column in the matrix
#' variances(dummy_matrix)
#' ## variances of a each column in the matrix corrected using the kth root
#' variances(dummy_matrix, k.root = TRUE)
#' 
#' ## ranges of each column in a matrix
#' ranges(dummy_matrix)
#' ## ranges of each column in the matrix corrected using the kth root
#' ranges(dummy_matrix, k.root = TRUE)
#' 
#' ## Distances between each row and centroid of the matrix
#' centroids(dummy_matrix)
#' ## Distances between each row and an arbitrary point
#' centroids(dummy_matrix, centroid = c(1,2,3,4,5,6,7,8,9))
#' ## Distances between each row and the origin
#' centroids(dummy_matrix, centroid = 0)
#' 
#' ## Modal value of a vector
#' mode.val(rnorm(25))
#' 
#' ## Ellipsoid volume of a matrix
#' ellipse.volume(dummy_matrix) # WARNING: only valid for MDS/PCO matrices
#' 
#' ## Convex hull hypersurface of a matrix
#' convhull.surface(dummy_matrix)
#' 
#' ## Convex hull volume of a matrix
#' convhull.volume(dummy_matrix)
#' 
#' ## Matrix hypervolume
#' hyper.volume(dummy_matrix)
#' 
#' ## Matrix diagonal
#' diagonal(dummy_matrix) # WARNING: only valid if the dimensions are orthogonal
#'
#' @seealso \code{\link{dispRity}} and \code{\link{make.metric}}.
#'
#' @author Thomas Guillerme

## kth root scaling
k.root <- function(data, dimensions){
    return(data^(1/dimensions))
}

## Calculating the variance of each axis
variances <- function(matrix, k.root) {
    if(missing(k.root)) {
        return(apply(matrix, 2, var))
    } else {
        return(k.root(apply(matrix, 2, var), ncol(matrix)))
    }
}

## Calculating the range of each axis
ranges <- function(matrix, k.root) {

    ## Initialise values
    max_values <- min_values <- ranges <- numeric(ncol(matrix))

    ## Max values
    max_values <- apply(matrix, 2, max)
    ## Min values
    min_values <- apply(matrix, 2, min)
    ## Range values
    ranges <- abs(max_values-min_values)
    if(missing(k.root)) {
        return(ranges)
    } else {
        return(k.root(ranges, ncol(matrix)))
    }
}

## Calculating the distance from centroid
centroids <- function(matrix, centroid) {

    ## Initialise values
    cent.dist <- numeric(nrow(matrix))

    if(missing(centroid)) {
        ## Calculating the centroid point
        centroid <- apply(matrix, 2, mean)
    }

    ## Euclidean distance from the centroid
    fun.dist <- function(row, centroid) {
        return(sqrt(sum((row-centroid)^2)))
    }

    cent.dist <- apply(matrix, 1, fun.dist, centroid = centroid)

    return(cent.dist)
}

## Calculate the mode of a vector
mode.val <- function(X){
    return(as.numeric(names(sort(-table(X))[1])))
}

## Calculate the ellipsoid volume of an eigen matrix (modified from Donohue et al 2013, Ecology Letters)
ellipse.volume <- function(matrix) {

    ## Initialising the variables
    ncol_matrix <- ncol(matrix)

    # The eigenvalue is equal to the sum of the variance/covariance within each axis
    # multiplied by the maximum number of dimensions (k-1) - ONLY WORKS FOR MDS OR PCO!
    eigen.value <- abs(apply(var(matrix),2, sum)*(nrow(matrix)-1))

    ## volume (from Donohue et al 2013, Ecology Letters)
    volume <- pi^(ncol_matrix/2)/gamma((ncol_matrix/2)+1)*prod(eigen.value^(0.5))

    return(volume)
}

## Calculate the convex hull hypersurface
convhull.surface <- function(matrix) {
    ## Algorithm warning
    if(any(dim(matrix) > 20)) message("WARNING: Your ordinated space is too big: convhull.surface function is likely to crash!")
    ## calculate the area
    return(geometry::convhulln(matrix, options = "FA")$area)
}

## Calculate the convex hull hyper-volume
convhull.volume <- function(matrix) {
    ## Algorithm warn
    if(any(dim(matrix) > 20)) message("WARNING: Your ordinated space is too big: convhull.surface function is likely to crash!")
    ## calculate the volume
    return(geometry::convhulln(matrix, options = "FA")$vol)
}

## Calculate the hypervolume using hypervolume::hypervolume
hyper.volume <- function(matrix, method = "box", print.output = FALSE, ...) {
    ## Calculate the volume
    output <- utils::capture.output(volume <- hypervolume::get_volume(hypervolume::hypervolume(matrix, method = method, ...)))
    # volume <- hypervolume::get_volume(hypervolume::hypervolume(matrix, method = method)) ; warning("DEBUG hyper.volume")
    names(volume) <- NULL

    if(print.output) {
        cat(output)
    }

    return(volume)
}

# # Hypervolume testing
# data <- space.maker(20, 5, rnorm)
# # estimating the bandwith
# bw <- estimate_bandwidth(data,method="silverman")
# # Calculating the hyper.volume (with 1000 replicates)
# vol <- hypervolume(data, repsperpoint = 1000, bandwidth = bw, quantile = 0.95)

# # Calculate the ellipsoid perimeter of an eigen matrix
# ellipse.perime <- function(matrix)

# # Calculate the volume of an eigen matrix (modified from Blonder et al 2014, Macroecological methods) #http://onlinelibrary.wiley.com/doi/10.1111/geb.12146/pdf
# hyper.volume <- function(matrix)


# Hypervolume distances
# hypervolume_distance

# ordihull::vegan
# convex.hull::igraph

## Diagonal
diagonal <- function(matrix) {
    ## If all the dimensions of the space are orthogonal to each other, then, following Pythagoras' theorem, the longest distance in this space is equal to the square root of sum of the distances of each dimension.
    return(sqrt(sum(ranges(matrix))))
}
