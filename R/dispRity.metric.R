#' @name dispRity.metric
#' @aliases dimension.level3.fun dimension.level2.fun dimension.level1.fun between.groups.fun variances ranges centroids mode.val ellipse.volume convhull.surface convhull.volume diagonal ancestral.dist pairwise.dist span.tree.length n.ball.volume radius neighbours displacements quantiles func.eve func.div angles deviations group.dist point.dist
#' @title Disparity metrics
#'
#' @description Different implemented disparity metrics.
#'
#' @usage dimension.level3.fun(matrix, ...)
#' dimension.level2.fun(matrix, ...)
#' dimension.level1.fun(matrix, ...)
#' between.groups.fun(matrix, matrix2, ...)
#'  
#' @param matrix A matrix.
#' @param matrix2 Optional, a second matrix for metrics between groups.
#' @param ... Optional arguments to be passed to the function. Usual optional arguments are \code{method} for specifying the method for calculating distance passed to \code{\link[vegan]{vegdist}} (e.g. \code{method = "euclidean"} - default - or \code{method = "manhattan"}) or \code{k.root} to scale the result using the eqn{kth} root. See details below for available optional arguments for each function.
#'
#' @details
#' These are inbuilt functions for calculating disparity. See \code{\link{make.metric}} for details on \code{dimension.level3.fun}, \code{dimension.level2.fun}, \code{dimension.level1.fun} and \code{between.groups.fun}. The dimensions levels (1, 2 and 3) can be seen as similar to ranks in linear algebra.
#' 
#' The currently implemented dimension-level 1 metrics are:
#' \itemize{
#'   \item \code{convhull.volume}: calculates the convex hull hypervolume of a matrix (calls \code{\link[geometry]{convhulln}(x, options = "FA")$vol}).
#'      \itemize{
#'          \item Both \code{convhull} functions call the \code{\link[geometry]{convhulln}} function with the \code{"FA"} option (computes total area and volume).
#'          \item WARNING: both \code{convhull} functions can be computationally intensive above 10 dimensions!
#'      }
#'
#'   \item \code{convhull.surface}: calculates the convex hull hypersurface of a matrix (calls \code{\link[geometry]{convhulln}(x, options = "FA")$area}).
#'
#'   \item \code{diagonal}: calculates the longest distance in the ordinated space.
#'      \itemize{
#'          \item WARNING: This function is the generalisation of Pythagoras' theorem and thus \bold{works only if each dimensions are orthogonal to each other}.
#'      }
#'
#'   \item \code{ellipse.volume}: calculates the ellipsoid volume of a matrix.
#'      \itemize{
#'          \item WARNING: this function assumes that the input matrix is ordinated and calculates the matrix' eigen values from the matrix as \code{abs(apply(var(matrix),2, sum))} (which is equivalent to \code{eigen(var(matrix))$values} but faster). These values are the correct eigen values for any matrix but differ from the ones output from \code{\link[stats]{cmdscale}} and \code{\link[ape]{pcoa}} because these later have their eigen values multiplied by the number of elements - 1 (i.e. \code{abs(apply(var(matrix),2, sum)) * nrow(matrix) -1 }). Specific eigen values can always be provided manually through \code{ellipse.volume(matrix, eigen.value = my_val)} (or \code{dispRity(matrix, metric = ellipse.volume, eigen.value = my_val)}).
#'      }
#' 
#'   \item \code{func.div}: The functional divergence (Vill'{e}ger et al. 2008): the ratio of deviation from the centroid (this is similar to \code{FD::dbFD()$FDiv}).
#' 
#'   \item \code{func.eve}: The functional evenness (Vill'{e}ger et al. 2008): the minimal spanning tree distances evenness (this is similar to \code{FD::dbFD()$FEve}). If the matrix used is not a distance matrix, the distance method can be passed using, for example \code{method = "euclidean"} (default).
#' 
#'   \item \code{mode.val}: calculates the modal value of a vector.
#'
#'   \item \code{n.ball.volume}: calculate the volume of the minimum n-ball (if \code{sphere = TRUE}) or of the ellipsoid (if \code{sphere = FALSE}).
#' 
#' }
#' 
#'  See also \code{\link[base]{mean}}, \code{\link[stats]{median}}, \code{\link[base]{sum}} or \code{\link[base]{prod}} for commonly used summary metrics.
#' 
#' The currently implemented dimension-level 2 metrics are:
#' \itemize{
#'   \item \code{ancestral.dist}: calculates the distance between each tip and node and their ancestral. This function needs either (1) \code{matrix}/\code{list} from \code{\link{nodes.coordinates}}; or a \code{tree} (\code{"phylo"}) and \code{full} (\code{"logical"}) argument to calculate the node coordinates for the direct descendants (\code{full = FALSE}) or all descendants down to the root (\code{full = TRUE}). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#' 
#'   \item \code{angles}: calculates the angles of the main axis of variation per dimension in a \code{matrix}. The angles are calculated using the least square algorithm from the \code{\link[stats]{lm}} function. The unit of the angle can be changed through the \code{unit} argument (either \code{"degree"} (default), \code{radian} or \code{slope}) and a base angle to measure the angle from can be passed through the \code{base} argument (by default \code{base = 0}, measuring the angle from the horizontal line (not that the \code{base} argument has to be passed in the same unit as \code{unit}). When estimating the slope through \code{\link[stats]{lm}}, you can use the option \code{significant} to only consider significant slopes (\code{TRUE}) or not (\code{FALSE} - default).
#' 
#'   \item \code{centroids}: calculates the distance between each row and the centroid of the matrix (Lalibert'{e} 2010). This function can take an optional arguments \code{centroid} for defining the centroid (if missing (default), the centroid of the matrix is used). This argument can be either a subset of coordinates matching the matrix's dimensions (e.g. \code{c(0, 1, 2)} for a matrix with three columns) or a single value to be the coordinates of the centroid (e.g. \code{centroid = 0} will set the centroid coordinates to \code{c(0, 0, 0)} for a three dimensional matrix). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#' \item \code{deviations}: calculates the minimal Euclidean distance between each element in and the hyperplane (or line if 2D, or a plane if 3D). You can specify equation of hyperplane of \emph{d} dimensions in the \eqn{intercept + ax + by + ... + nd = 0} format. For example the line \eqn{y = 3x + 1} should be entered as \code{c(1, 3, -1)} or the plane \eqn{x + 2y - 3z = 44} as \code{c(44, 1, 2, -3,)}. If missing the \code{hyperplane} (default) is calculated using a least square regression using a gaussian \code{\link[stats]{glm}}. Extract arguments can be passed to \code{\link[stats]{glm}} through \code{...}. When estimating the hyperplane, you can use the option \code{significant} to only consider significant slopes (\code{TRUE}) or not (\code{FALSE} - default).
#'   \item \code{displacements}: calculates the ratio between the distance to the centroid (see \code{centroids} above) and the distance from a reference (by default the origin of the space). The reference can be changed through the \code{reference} argument. NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{neighbours}: calculates the distance to a neighbour (Foote 1990). By default this is the distance to the nearest neighbour (\code{which = min}) but can be set to any dimension level - 1 function (e.g. \code{which = mean} gives the distance to the most average neighbour). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument. 
#'
#'   \item \code{pairwise.dist}: calculates the pairwise distance between elements - calls \code{vegdist(matrix, method = method, diag = FALSE, upper = FALSE, ...)} (Foote 1990). The distance type can be changed via the \code{method} argument (see \code{\link[vegan]{vegdist}} - default: \code{method = "euclidean"}). This function outputs a vector of pairwise comparisons in the following order: d(A,B), d(A,C), d(B,C) for three elements A, B and C. NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{quantiles}: calculates the quantile range of each axis of the matrix. The quantile can be changed using the \code{quantile} argument (default is \code{quantile = 95}, i.e. calculating the range on each axis that includes 95\% of the data). An optional argument, \code{k.root}, can be set to \code{TRUE} to scale the ranges by using its \eqn{kth} root (where \eqn{k} are the number of dimensions). By default, \code{k.root = FALSE}.
#'
#'   \item \code{radius}: calculates a distance from the centre of each axis. The \code{type} argument is the function to select which distance to calculate. By default \code{type = max} calculates the maximum distance between the elements and the centre for each axis (i.e. the radius for each dimensions)
#'
#'   \item \code{ranges}: calculates the range of each axis of the matrix (Wills 2001). An optional argument, \code{k.root}, can be set to \code{TRUE} to scale the ranges by using its \eqn{kth} root (where \eqn{k} are the number of dimensions). By default, \code{k.root = FALSE}.
#'
#'   \item \code{variances}: calculates the variance of each axis of the matrix (Wills 2001). This function can also take the \code{k.root} optional argument described above.
#' 
#'   \item \code{span.tree.length}: calculates the length of the minimum spanning tree (see \code{\link[vegan]{spantree}}). This function can get slow with big matrices. To speed it up, one can directly use distance matrices as the multidimensional space.
#'
#' }
#' 
#' The currently implemented between.groups metrics are:
#' \itemize{
#'    \item \code{group.dist}: calculates the distance between two groups (by default, this is the minimum euclidean vector norm distance between groups). Negative distances are considered as 0. This function must intake two matrices (\code{matrix} and \code{matrix2}) and the quantiles to consider. For the minimum distance between two groups, the 100% quantiles are considered (default: \code{probs = c(0,1)}) but this can be changed to any values (e.g. distance between the two groups accounting based on the 95% CI: \code{probs = c(0.025, 0.975)}; distance between centroids: \code{probs = c(0.5)}, etc...). This function is the linear algebra equivalent of the \code{\link[hypervolume]{hypervolume_distance}} function.
#' 
#' \item \code{point.dist}: calculates the distance between \code{matrix} and a point calculated from \code{matrix2}. By default, this point is the centroid of \code{matrix2}. This can be changed by passing a function to be applied to \code{matrix2} through the \code{point} argument (for example, for the centroid: \code{point.dist(..., point = colMeans)}). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#' 
#' 
#' 
#' }
#' 
#' When used in the \code{\link{dispRity}} function, optional arguments are declared after the \code{metric} argument: for example
#'     \code{dispRity(data, metric = centroids, centroid = 0, method = "manhattan")}
#' 
#' 
#TG: For the references, be extra careful with the fecking non-ASCII dashes ("-")
#'
#' @references Donohue I, Petchey OL, Montoya JM, Jackson AL, McNally L, Viana M, Healy K, Lurgi M, O'Connor NE, Emmerson MC. 2013. On the dimensionality of ecological stability. Ecology letters. 16(4):421-9.
#' @references Lalibert'{e} E, Legendre P. 2010. A distance-based framework for measuring functional diversity from multiple traits. Ecology, 91(1), pp.299-305.
#' @references Vill'{e}ger S, Mason NW, Mouillot D. 2008. New multidimensional functional diversity indices for a multifaceted framework in functional ecology. Ecology. 89(8):2290-301.
#' @references Wills MA. 2001. Morphological disparity: a primer. In Fossils, phylogeny, and form (pp. 55-144). Springer, Boston, MA.
#' @references Foote, M. 1990. Nearest-neighbor analysis of trilobite morphospace. Systematic Zoology, 39(4), pp.371-382.
#' 
#' 
#' 
#' @seealso \code{\link{dispRity}} and \code{\link{make.metric}}.
#'
#' @examples
#' ## A random matrix
#' dummy_matrix <- matrix(rnorm(90), 9, 10)
#'
#' ## ancestral.dist
#' ## A random tree with node labels
#' rand_tree <- rtree(5) ; rand_tree$node.label <- paste0("n", 1:4)
#' ## Adding the tip and node names to the matris
#' rownames(dummy_matrix) <- c(rand_tree$tip.label, rand_tree$node.label)
#' ## Calculating the direct ancestral nodes
#' direct_anc_centroids <- nodes.coordinates(dummy_matrix, rand_tree, full = FALSE)
#' ## Calculating all the ancestral nodes
#' all_anc_centroids <- nodes.coordinates(dummy_matrix, rand_tree, full = TRUE)
#' ## Calculating the distances from the direct ancestral nodes
#' ancestral.dist(dummy_matrix, nodes.coords = direct_anc_centroids)
#' ## Calculating the distances from all the ancestral nodes
#' ancestral.dist(dummy_matrix, nodes.coords = all_anc_centroids)
#' 
#' ## angles
#' ## The angles in degrees of each axis
#' angles(dummy_matrix)
#' ## The angles in slope from the 1:1 slope (Beta = 1)
#' angles(dummy_matrix, unit = "slope", base = 1)
#'
#' ## centroids
#' ## Distances between each row and centroid of the matrix
#' centroids(dummy_matrix)
#' ## Distances between each row and an arbitrary point
#' centroids(dummy_matrix, centroid = c(1,2,3,4,5,6,7,8,9,10))
#' ## Distances between each row and the origin
#' centroids(dummy_matrix, centroid = 0)
#'
#' ## convhull.surface
#' ## Making a matrix with more elements than dimensions (for convhull)
#' thinner_matrix <- matrix(rnorm(90), 18, 5)
#' ## Convex hull hypersurface of a matrix
#' convhull.surface(thinner_matrix)
#' 
#' ## convhull.volume
#' ## Convex hull volume of a matrix
#' convhull.volume(thinner_matrix)
#' 
#' ## deviations
#' ## The deviations from the least square hyperplane
#' deviations(dummy_matrix)
#' ## The deviations from the plane between the x and y axis
#' deviations(dummy_matrix, hyperplane = c(0,1,1,0,0,0,0,0,0,0,0))
#'
#' ## diagonal
#' ## Matrix diagonal
#' diagonal(dummy_matrix) # WARNING: only valid if the dimensions are orthogonal
#'
#' ## displacements
#' ## displacement ratios (from the centre)
#' displacements(dummy_matrix)
#' ## displacement ratios (from an arbitrary point)
#' displacements(dummy_matrix, reference = c(1,2,3,4,5,6,7,8,9,10))
#' ## displacement ratios from the centre (manhattan distance)
#' displacements(dummy_matrix, method = "manhattan")
#' 
#' ## ellipse.volume
#' ## Ellipsoid volume of a matrix
#' ellipse.volume(dummy_matrix)
#' ## Calculating the same volume with provided eigen values
#' ordination <- prcomp(dummy_matrix)
#' ## Calculating the ellipsoid volume
#' ellipse.volume(ordination$x, eigen.value = ordination$sdev^2)
#' 
#' ## func.div
#' ## Functional divergence
#' func.div(dummy_matrix)
#'
#' ## func.eve
#' ## Functional evenness
#' func.eve(dummy_matrix) 
#' ## Functional evenness (based on manhattan distances)
#' func.eve(dummy_matrix, method = "manhattan")
#'
#' ## group.dist
#' ## The distance between groups
#' dummy_matrix2 <- matrix(runif(40, min = 2, max = 4), 4, 10)
#' ## The minimum distance between both groups
#' group.dist(dummy_matrix, dummy_matrix2)
#' ## The distance between both groups' centroids
#' group.dist(dummy_matrix, dummy_matrix2, probs = 0.5)
#' ## The minimum distance between the 50% CI of each group
#' group.dist(dummy_matrix, dummy_matrix2, probs = c(0.25, 0.75))
#' 
#' ## mode.val
#' ## Modal value of a vector
#' mode.val(dummy_matrix)
#' 
#' ## neighbours
#' ## The nearest neighbour euclidean distances
#' neighbours(dummy_matrix)
#' ## The furthest neighbour manhattan distances
#' neighbours(dummy_matrix, which = max, method = "manhattan")
#'
#' ## pairwise.dist
#' ## The pairwise distance
#' pairwise.dist(dummy_matrix)
#' ## The average squared pairwise distance
#' mean(pairwise.dist(dummy_matrix)^2)
#' ## equal to:
#' geiger::disparity(data = dummy_matrix)
#' 
#' ## point.dist
#' ## The distances from the rows dummy_matrix
#' ## to the centroids of dummy_matrix2
#' point.dist(dummy_matrix, dummy_matrix2)
#' ## The average distances from dummy_matrix
#' ## to the centroids of dummy_matrix2
#' mean(point.dist(dummy_matrix, dummy_matrix2))
#' ## The manhattan distance from the rows dummy_matrix
#' ## to the standard deviation of dummy_matrix2
#' point.dist(dummy_matrix, dummy_matrix2, point = sd, method = "manhattan")
#'
#' ## quantiles
#' ## The 95 quantiles
#' quantiles(dummy_matrix)
#' ## The 100 quantiles (which are equal to the ranges)
#' quantiles(dummy_matrix, quantile = 100) == ranges(dummy_matrix) # All TRUE
#'
#' ## radius
#' ## The maximal radius of each axis (maximum distance from centre of each axis)
#' radius(dummy_matrix)
#'
#' ## ranges
#' ## ranges of each column in a matrix
#' ranges(dummy_matrix)
#' ## ranges of each column in the matrix corrected using the kth root
#' ranges(dummy_matrix, k.root = TRUE)
#'
#' ## span.tree.length
#' ## Minimum spanning tree length (default)
#' span.tree.length(dummy_matrix)
#' ## Minimum spanning tree length from a distance matrix (faster)
#' distance <- as.matrix(dist(dummy_matrix))
#' span.tree.length(distance)
#' ## Minimum spanning tree length based on Manhattan distance
#' span.tree.length(dummy_matrix, method = "manhattan")
#' span.tree.length(as.matrix(dist(dummy_matrix, method = "manhattan"))) # Same
#'
#' ## variances
#' ## variances of a each column in the matrix
#' variances(dummy_matrix)
#' ## variances of a each column in the matrix corrected using the kth root
#' variances(dummy_matrix, k.root = TRUE)
#' 
#'
#' @author Thomas Guillerme

## dimension level generic functions
dimension.level3.fun <- function(matrix, ...) {
    cat("No implemented Dimension level 3 functions implemented in dispRity!\n")
    cat("You can create your own by using: ?make.metric\n")
}

dimension.level2.fun <- function(matrix, ...) {
    cat("Dimension level 2 functions implemented in dispRity:\n")
    cat("?ancestral.dist\n")
    cat("?angles\n")
    cat("?centroids\n")
    cat("?deviations\n")
    cat("?displacements\n")
    cat("?neighbours\n")
    cat("?pairwise.dist\n")
    cat("?point.dist\n")
    cat("?ranges\n")
    cat("?radius\n")
    cat("?variances\n")
    cat("?span.tree.length\n")
}

dimension.level1.fun <- function(matrix, ...) {
    cat("Dimension level 1 functions implemented in dispRity:\n")
    cat("?convhull.surface\n")
    cat("?convhull.volume\n")
    cat("?diagonal\n")
    cat("?ellipse.volume\n")
    cat("?func.div\n")
    cat("?func.eve\n")
    cat("?group.dist\n")
    cat("?mode.val\n")
    cat("?n.ball.volume\n")
}

between.groups.fun <- function(matrix, matrix2, ...) {
    cat("Between groups functions implemented in dispRity:\n")
    cat("?group.dist # level 1\n")
    cat("?point.dist # level 2\n")
}

## kth root scaling
k.root <- function(data, dimensions){
    return(data^(1/dimensions))
}

## Calculating the variance of each axis
variances <- function(matrix, k.root = FALSE) {
    if(!k.root) {
        return(apply(matrix, 2, var, na.rm = TRUE))
    } else {
        return(k.root(apply(matrix, 2, var, na.rm = TRUE), ncol(matrix)))
    }
}

## Calculating the range of each axis
ranges <- function(matrix, k.root = FALSE) {
    if(!k.root) {
        return(as.vector(abs(diff(apply(matrix, 2, range)))))
    } else {
        return(k.root(as.vector(abs(diff(apply(matrix, 2, range)))), dimensions = ncol(matrix)))
    }
}

## Calculate the quantiles range in a matrix
quantiles <- function(matrix, quantile = 95, k.root = FALSE) {
    if(!k.root) {
        return(as.vector(abs(diff(apply(matrix, 2, quantile, prob = CI.converter(quantile), na.rm = TRUE)))))
    } else {
        return(k.root(as.vector(abs(diff(apply(matrix, 2, quantile, prob = CI.converter(quantile), na.rm = TRUE)))), dimensions = ncol(matrix)))
    }
}

## Euclidean distance from the centroid
fun.dist.euclidean <- function(row, centroid) {
    return(sqrt(sum((row-centroid)^2)))
}
## Manhattan distance from the centroid
fun.dist.manhattan <- function(row, centroid) {
    return(sum(abs(row-centroid)))
}
## Select either method
select.method <- function(method) {
    ## Switch methods
    fun.dist <- switch(method,
        euclidean = fun.dist.euclidean,
        manhattan = fun.dist.manhattan
    )
    ## Returns non-null
    if(is.null(fun.dist)) {
        stop.call(msg = "can only be \"euclidean\" or \"manhattan\".", call = "method argument ")
    } else {
        return(fun.dist)
    }
}
## Calculating the distance from centroid
centroids <- function(matrix, centroid, method = "euclidean") {

    ## Select the fun distance
    fun.dist <- select.method(method)

    ## Initialise values
    cent.dist <- numeric(nrow(matrix))

    if(missing(centroid)) {
        ## Calculating the centroid point
        centroid <- colMeans(matrix)
    } 

    ## Calculate centroid distance with a single centroid
    cent.dist <- apply(matrix, 1, fun.dist, centroid = centroid)
    return(cent.dist)
}

## Calculate the relative displacements
displacements <- function(matrix, method = "euclidean", reference = 0) {
    return(dispRity::centroids(matrix = matrix, centroid = reference, method = method)/dispRity::centroids(matrix = matrix, method = method))
}


## Calculate the neighbours distances
neighbours <- function(matrix, which = min, method = "euclidean") {
    ## Check if the matrix is a distance matrix first
    distances <- as.matrix(check.dist.matrix(matrix, method = method)[[1]])
    ## Remove the diagonals
    diag(distances) <- NA
    ## Get the selected distances for each rows
    return(unname(apply(distances, 1, which, na.rm = TRUE)))
}


## Calculate the mode of a vector
mode.val <- function(matrix){
    return(as.numeric(names(sort(-table(matrix))[1])))
}

## Calculate the ellipsoid volume of an eigen matrix (modified from Donohue et al 2013, Ecology Letters)
ellipse.volume <- function(matrix, eigen.value) {

    ## Initialising the variables
    ncol_matrix <- ncol(matrix)

    ## The eigenvalue is equal to the sum of the variance/covariance within each axis (* nrow(matrix) as used in pco/pcoa)
    if(missing(eigen.value)) {
        eigen.value <- abs(apply(var(matrix, na.rm = TRUE), 2, sum)) # * (nrow(matrix) - 1)
    } else {
        eigen.value <- eigen.value[1:ncol_matrix]
    }

    ## volume (from Donohue et al 2013, Ecology Letters)
    volume <- pi^(ncol_matrix/2)/gamma((ncol_matrix/2)+1)*prod(eigen.value^(0.5))

    return(volume)
}

## Calculate the convex hull hypersurface
convhull.surface <- function(matrix) {
    ## calculate the area
    return(geometry::convhulln(matrix, options = "FA")$area)
}

## Calculate the convex hull hyper-volume
convhull.volume <- function(matrix) {
    ## calculate the volume
    return(geometry::convhulln(matrix, options = "FA")$vol)
}

## Calculate the hypervolume using hypervolume::hypervolume
# hyper.volume <- function(matrix, method = "box", print.output = FALSE, ...) {
#     ## Calculate the volume
#     output <- utils::capture.output(volume <- hypervolume::get_volume(hypervolume::hypervolume(matrix, method = method, ...)))
#     # volume <- hypervolume::get_volume(hypervolume::hypervolume(matrix, method = method)) ; warning("DEBUG hyper.volume")
#     names(volume) <- NULL

#     if(print.output) {
#         cat(output)
#     }

#     return(volume)
# }

# Hypervolume distances
# hypervolume_distance

# ordihull::vegan
# convex.hull::igraph

## Diagonal
diagonal <- function(matrix) {
    ## If all the dimensions of the space are orthogonal to each other, then, following Pythagoras' theorem, the longest distance in this space is equal to the square root of sum of the distances of each dimension.
    return(sqrt(sum(ranges(matrix))))
}

## Calculating the distance from the ancestral nodes
ancestral.dist <- function(matrix, nodes.coords, tree, full, method = "euclidean", ...) {

    ## Select the fun distance
    fun.dist <- select.method(method)

    ## Checking if the nodes.coords is available
    if(missing(nodes.coords)) {
        if(!missing(tree) && !missing(full)) {
            nodes.coords <- nodes.coordinates(matrix, tree, full)
        } else {
            warning("Missing tree and full argument for nodes.coordinates.\nSee ?nodes.coordinates manual.\nThe centroids function was applied instead.")
            return(centroids(matrix, ...))
        }
    }
    
    switch(class(nodes.coords)[1],
        matrix = {
            ## Converting both matrix and nodes.coords in lists
            matrix <- unlist(apply(matrix, 1, list), recursive = FALSE)
            nodes.coords <- unlist(apply(nodes.coords, 1, list), recursive = FALSE)

            ## Calculate nodes.coords distance with multiple nodes.coords
            cent.dist <- mapply(fun.dist, row = matrix, centroid = nodes.coords)
            return(cent.dist)
        },
        list = {
            ## Wrapper function
            mapply.fun.dist <- function(nodes.coords, matrix, fun.dist) {
                ## Converting the matrix into a list
                nodes.coords <- unlist(apply(nodes.coords, 1, list), recursive = FALSE)
                ## Calculate nodes.coords distance with multiple nodes.coords
                cent.dist <- mapply(fun.dist, row = matrix, centroid = nodes.coords)
                return(cent.dist)
            }

            ## Convert the matrix into a list
            matrix <- unlist(apply(matrix, 1, list), recursive = FALSE)

            ## Calculate nodes.coords distance with multiple centroids
            cent.dist <- lapply(nodes.coords, mapply.fun.dist, matrix, fun.dist)
            cent.dist <- do.call(rbind, cent.dist)
            
            ## Calculate the cumulative distances
            cent.dist <- apply(cent.dist, 2, sum, na.rm = TRUE)
            return(cent.dist)
        },
        numeric = {
            ## Apply the normal centroid function
            return(centroids(matrix, centroid = nodes.coords, ...))
        }
    )
}

## Calculates the hyperbox volume (or hypercube if cube = TRUE)
span.tree.length <- function(matrix, toolong = 0, method = "euclidean") {
    ## Check if the matrix is a distance matrix first
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Get the span tree length
    return(vegan::spantree(distances, toolong)$dist)
}

## Calculates the pairwise distance between elements
pairwise.dist <- function(matrix, method = "euclidean") {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}

## Calculate the radius for each dimensions
radius <- function(matrix, type = max) {
    ## Calculate the maximum distance from the centres per axis
    differences <- mapply(function(x, y) abs(x - y), unlist(apply(matrix, 2, list), recursive = FALSE), as.list(colMeans(matrix)), SIMPLIFY = FALSE)

    ## Getting the radius
    return(unlist(lapply(differences, type)))
}

## Calculates the n-ball volume
n.ball.volume <- function(matrix, sphere = TRUE) {
    ## Dimensions
    n <- ncol(matrix)

    ## Radius
    if(sphere) {
        radius <- min(radius(matrix))
    } else {
        radius <- prod(radius(matrix))
    }

    ## Volume
    return(pi^(n/2)/gamma((n/2)+1)*radius)
}

## Minimal spanning tree distances evenness
func.eve <- function(matrix, method = "euclidean") {
    ## Distance matrix
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## weighted evenness (EW) for equal weighted species
    branch_lengths <- (distances/2)[which(as.dist(mst(distances)) != 0)]
    ## partial weighted evenness (PEW)
    rel_br_lentghs <- branch_lengths/sum(branch_lengths)
    ## Regular abundance value (1/(S-1))
    regular <- 1/(nrow(matrix) - 1)
    ## Get the minimal distances
    min_distances <- sapply(rel_br_lentghs, function(x, y) min(c(x, y)), y = regular)
    ## Return the Functional eveness
    return((sum(min_distances) - regular) / (1 - regular))
}

## Distance from centroid deviation ratio
func.div <- function(matrix) {
    ## The distance from centroid (dGi)
    dist_centroid <- centroids(matrix)
    ## The mean distance from centroid (dG)
    mean_dis_cent <- mean(dist_centroid, na.rm = TRUE)
    ## The number of observations
    obs <- length(dist_centroid)
    ## The FDiv metric
    return((sum(dist_centroid) - mean_dis_cent * (obs-1)) / ((sum(abs(dist_centroid - mean_dis_cent) + dist_centroid))/obs))
}

## Angles measurements
angles <- function(matrix, unit = "degree", base = 0, significant = FALSE) {

    ## Check the unit
    all_methods <- c("degree", "radian", "slope")
    check.method(unit, all_methods, "Angle unit")

    ## Check the base
    check.class(base, c("numeric", "integer"))

    ## Generate the base angle (slope/radian/angle = 0)
    base_angle <- seq_len(nrow(matrix))

    ## Select the right slope function
    get.slope.significant <- function(X, base_angle) {
        model <- lm(base_angle ~ X)
        return(ifelse(summary(model)[[4]][[8]] < 0.05, model$coefficients[[2]], 0))
    }
    get.slope.nonsignificant <- function(X, base_angle) {
        lm(base_angle ~ X)$coefficients[[2]]
    }
    if(significant) {
        get.slope <- get.slope.significant
    } else {
        get.slope <- get.slope.nonsignificant
    }

    ## Get all the slopes
    slopes <- apply(matrix, 2, get.slope, base_angle = base_angle)

    ## Convert the slopes
    angles <- switch(unit,
        #degree = {atan(slopes/(1 + base_slope * (base_slope + slopes))) * 180/pi},
        degree = {atan(slopes) * 180/pi},
        radian =  {atan(slopes)},
        # radian = {atan(slopes/(1 + base_slope * (base_slope + slopes)))},
        slope  = {slopes}
        )

    ## Add a base angle (if not 0)
    if(base != 0) {
        return(angles + base)
    } else {
        return(angles)
    }
}

## Deviations
deviations <- function(matrix, hyperplane, ..., significant = FALSE) {

    ## Get the dimensions
    dimensions <- ncol(matrix)

    if(missing(hyperplane)) {
        ## If the data is unidimensional
        if(ncol(matrix) == 1) {
            data <- as.data.frame(cbind(seq_along(1:nrow(matrix)), matrix))
        } else {
            data <- as.data.frame(matrix)
        }
        colnames(data) <- paste0("c", seq_along(1:ncol(data)))

        ## Calculate the hyperplane
        formula <- "c1 ~ c2"
        if(ncol(matrix) > 2) {
            formula <- paste0(formula, " + ", paste(colnames(data)[-c(1,2)], collapse = " + "))
        }

        ## Get the regression coefficients
        if(!significant) {
            equation <- glm(formula = formula, data = data, ...)$coefficients
            ## Replace NAs by zeros
            equation <- ifelse(is.na(equation), 0, equation)
        } else {
            ## Run the model
            model <- glm(formula = formula, data = data, ...)
            ## Check the coefficients p values
            equation <- ifelse(is.na(model$coefficients), 0, model$coefficients)
            ## Check p_values
            p_values <- which(ifelse(is.na(summary(model)$coefficients[,4]), 0, summary(model)$coefficients[,4]) > 0.5)
            ## Get the hyperplane definition
            if(length(p_values) != 0) {
                equation[p_values] <- 0
            }
        }
        ## Correct for the hyperplane equation to be equal to 0
        hyperplane <- c(equation[1], -1, equation[-1])
    } else {
        check.length(hyperplane, dimensions+1, paste0(" must be of length ", dimensions, "+1."))
    }
    ## Distance function
    distance <- function(point, hyperplane, dimensions) {
        ## Get the number of dimensions
        return(abs(sum(point*hyperplane[2:(dimensions+1)], hyperplane[1]))/sqrt(sum(hyperplane[2:(dimensions+1)]^2)))
    }

    ## Get all distance
    return(apply(matrix, 1, FUN = distance, hyperplane, dimensions))
}



## Functions for the group.distance function
## Function for centreing a matrix on one specific centroid
centre.matrix <- function(matrix, group) {
    centre <- colMeans(matrix[group,])
    matrix - rep(centre, rep.int(nrow(matrix), ncol(matrix)))
}
## Function for getting the projected lengths
get.proj.length <- function(point, centroid, length) {
    return(geometry::dot(centroid, point)/length)
}
## Function for getting the quantiles per groups
quantiles.per.groups <- function(group, lengths, probs) {
    return(quantile(lengths[group], probs = probs))
}

## Euclidean distance between two groups
group.dist <- function(matrix, matrix2, probs = c(0,1)) {
    ## Make the combined matrix
    combined_matrix <- as.matrix(rbind(matrix, matrix2))
    ## Get the groups IDs
    groups <- list(1:nrow(matrix), (1:nrow(matrix2)+nrow(matrix)))
    ## Centre that matrix onto the centroid of group 1
    centred_matrix <- centre.matrix(combined_matrix, groups[[1]])
    ## Get the centroid vector in the centred matrix (the centroid of group 2)
    centroid <- colMeans(centred_matrix[groups[[2]],])
    ## Get the length of each projected points on the centroid vector
    projected_lengths <- apply(centred_matrix, 1, get.proj.length, centroid = centroid, length = sqrt(sum(centroid^2)))
    ## Get the quantile of the projections for each group
    group_quantiles <- lapply(groups, quantiles.per.groups, projected_lengths, probs)

    ## Get the difference from group1 to group2 (depending on orientation along the difference axis)
    if(mean(projected_lengths[groups[[1]]]) < mean(projected_lengths[groups[[2]]])) {
        ## Group 1 is on the "left"
        distance <- ifelse(length(probs) == 1, 
                    group_quantiles[[2]] - group_quantiles[[1]],
                    group_quantiles[[2]][1] - group_quantiles[[1]][2])
    } else {
        ## Group 1 is on the "right"
        distance <- ifelse(length(probs) == 1, 
                    group_quantiles[[1]] - group_quantiles[[2]],
                    group_quantiles[[1]][2] - group_quantiles[[2]][1])
    }
    return(unname(ifelse(distance < 0, 0, distance)))
}

## Distance between two groups
point.dist <- function(matrix, matrix2, point = colMeans, method = "euclidean", ...) {
    ## Select the fun distance
    fun.dist <- select.method(method)

    ## Calculating the centroid point
    centroid <- point(matrix2)

    ## Calculate centroid distance with a single centroid
    return(apply(matrix, 1, fun.dist, centroid = centroid))
}

#' @title Nodes coordinates
#'
#' @description Calculates ancestral nodes coordinates in a format that can be passed to \code{\link{ancestral.dist}}
#'
#' @param matrix The \code{matrix} on which \code{\link{centroids}} will be applied
#' @param tree A tree topology of class \code{"phylo"}.
#' @param full Whether to get the centroids for all ancestors down to the root (\code{TRUE} - default) or only the direct ancestors (\code{FALSE})
#' 
#' @return
#' A \code{matrix} if \code{full = FALSE} or a \code{list} of matrices if \code{full = TRUE}.
#' 
#' @examples
#' ## A random matrix
#' matrix <- matrix(rnorm(90), 9, 10)
#' ## A random treee with node labels
#' tree <- rtree(5) ; tree$node.label <- paste0("n", 1:4)
#' ## Adding the tip and node names to the matris
#' rownames(matrix) <- c(tree$tip.label, tree$node.label)
#' 
#' ## Calculating the direct ancestral nodes
#' direct_anc_centroids <- nodes.coordinates(matrix, tree, full = FALSE)
#' ## Calculating all the ancestral nodes
#' all_anc_centroids <- nodes.coordinates(matrix, tree, full = TRUE)
#' 
#' ## Calculating the distances from the direct ancestral nodes
#' ancestral.dist(matrix, nodes.coords = direct_anc_centroids)
#' ## Calculating the distances from all the ancestral nodes
#' ancestral.dist(matrix, nodes.coords = all_anc_centroids)
#'
#' @seealso \code{\link{ancestral.dist}}, \code{\link{dispRity.metric}}, \code{\link{dispRity}}, \code{\link{get.ancestors}}
#' 
#' @author Thomas Guillerme
# @export
#' 
## Getting the centroid matrix for ancestor.centroids
nodes.coordinates <- function(matrix, tree, full = TRUE) {
    if(!full) {
        ## Get the direct ancestors only (if no matrix provided)
        centroids <- matrix[sapply(1:(Ntip(tree) + (Nnode(tree))), get.ancestors, tree, full = FALSE), ]
    } else {
        ## Calculate the multiple matrices
        centroids <- lapply(get.ancestors.list(tree), function(X) return(matrix[c(X), ]))
    }
    return(centroids)
}

## Applies get.ancestor to all tips and nodes and outputs a list
get.ancestors.list <- function(tree) {

    ## Get all the ancestors
    ancestors <- sapply(1:(Ntip(tree) + (Nnode(tree))), get.ancestors, tree, full = TRUE)

    ## Get the tree depth
    depth <- max(unlist(lapply(ancestors, length)))

    ## Fill NAs were necessary
    lapply.fill.NA <- function(X, depth) {
        length_X <- length(X)
        if(length_X == depth) {
            return(X)
        } else {
            return(c(X, rep(NA, depth - length_X)))
        }
    }
    ancestors <- lapply(ancestors, lapply.fill.NA, depth)

    ## Transform into a matrix
    ancestors <- matrix(unlist(ancestors), nrow = length(ancestors), byrow = TRUE)

    ## And back into a list
    return(unlist(apply(ancestors, 2, list), recursive = FALSE))
}

#' @title Get ancestors
#'
#' @description Gets the list of ancestors (parents) from a tip or a node (modified from \code{\link[phytools]{getParent}})
#'
#' @param tip A tip (or node) index.
#' @param tree A tree topology of class \code{"phylo"}.
#' @param full Whether to output the direct ancestor only (\code{FALSE}) or the full list of ancestors to the root (\code{TRUE} - default)
#' 
#' @return
#' A \code{integer} vector of ancestor(s).
#' 
#' @examples
#' ## A random tree
#' tree <- rtree(10)
#' ## Get the ancestors of the first tip
#' get.ancestors(1, tree)
#'
#' @seealso \code{\link{ancestral.dist}}, \code{\link{nodes.coordinates}}, \code{\link[phytools]{getParent}}
#' 
#' @author Thomas Guillerme
# @export
#' 
## get the list of ancestors (modified from phytools::getParent)
get.ancestors <- function(tip, tree, full = TRUE) {

    ## Getting the tree root (first node in ape edge table)
    root <- tree$edge[1,1]

    if(tip == root) {
        ## If the tip is the root, simply return
        return(root)
    }

    ## Function for getting one ancestor
    get.one.ancestor <- function(tip, tree) {
        return(ancestor <- tree$edge[which(tree$edge[, 2] == tip), 1])
    }

    ## Get the first ancestor
    ancestors <- get.one.ancestor(tip, tree)

    if(full) {
        ## If the last ancestor is not the root, continue down the tree
        while(ancestors[length(ancestors)] != root) {
            ancestors <- c(ancestors, get.one.ancestor(ancestors[length(ancestors)], tree))
        }
    }

    ## Output
    return(ancestors)
}


