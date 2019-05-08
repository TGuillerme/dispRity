#' @name dispRity.metric
#' @aliases dimension.level3.fun dimension.level2.fun dimension.level1.fun variances ranges centroids mode.val ellipse.volume convhull.surface convhull.volume diagonal ancestral.dist pairwise.dist span.tree.length n.ball.volume radius neighbours displacements quantiles
#' @title Disparity metrics
#'
#' @description Different implemented disparity metrics.
#'
#' @usage dimension.level3.fun(matrix, ...)
#' dimension.level2.fun(matrix, ...)
#' dimension.level1.fun(matrix, ...)
#'  
#' @param matrix A matrix.
#' @param ... Optional arguments to be passed to the function. Usual optional arguments are \code{method} for specifying the method for calculating distance passed to \code{\link[vegan]{vegdist}} (e.g. \code{method = "euclidean"} - default - or \code{method = "manhattan"}) or \code{k.root} to scale the result using the eqn{kth} root. See details below for available optional arguments for each function.
#'
#' @details
#' These are inbuilt functions for calculating disparity. See \code{\link{make.metric}} for details on \code{dimension.level3.fun}, \code{dimension.level2.fun} and \code{dimension.level1.fun}.
#' 
#' The currently implemented dimension-level 1 metrics are:
#' \itemize{
#'   \item \code{convhull.volume}: calculates the convex hull hypervolume of a matrix (calls \code{convhulln(x, options = "FA")$vol}).
#'      \itemize{
#'          \item Both \code{convhull} functions call the \code{\link[geometry]{convhulln}} function with the \code{"FA"} option (computes total area and volume).
#'          \item WARNING: both \code{convhull} functions can be computationally intensive above 10 dimensions!
#'      }
#'
#'   \item \code{convhull.surface}: calculates the convex hull hypersurface of a matrix (calls \code{convhulln(x, options = "FA")$area}).
#'
#'   \item \code{diagonal}: calculates the longest distance in the ordinated space.
#'      \itemize{
#'          \item WARNING: This function is the generalisation of Pythagoras' theorem and thus \bold{works only if each dimensions are orthogonal to each other}.
#'      }
#'
#'   \item \code{ellipse.volume}: calculates the ellipsoid volume of a matrix.
#'      \itemize{
#'          \item WARNING: this function calculates the matrix' eigen values from the matrix as \code{abs(apply(var(matrix),2, sum))} (which is equivalent to \code{eigen(var(matrix))$values} but faster). These values are the correct eigen values for any matrix but differ from the ones output from \code{\link[stats]{cmdscale}} and \code{\link[ape]{pcoa}} because these later have their eigen values multiplied by the number of elements - 1 (i.e. \code{abs(apply(var(matrix),2, sum)) * nrow(matrix) -1 }). Specific eigen values can always be provided manually through \code{ellipse.volume(matrix, eigen.value = my_val)} (or \code{dispRity(matrix, metric = ellipse.volume, eigen.value = my_val)}).
#'      }
#'
#'   \item \code{mode.val}: calculates the modal value of a vector.
#'
#'   \item \code{n.ball.volume}: calculate the volume of the minimum n-ball (if \code{sphere = TRUE}) or of the ellipsoid (if \code{sphere = FALSE}).
#'
#'   \item \code{span.tree.length}: calculates the length of the minimum spanning tree (see \code{\link[vegan]{spantree}}). This function can get slow with big matrices. To speed it up, one can directly use distance matrices as the multidimensional space.
#'
#' }
#' 
#'  See also \code{\link[base]{mean}}, \code{\link[stats]{median}}, \code{\link[base]{sum}} or \code{\link[base]{prod}} for commonly used summary metrics.
#' 
#' The currently implemented dimension-level 2 metrics are:
#' \itemize{
#'   \item \code{ancestral.dist}: calculates the distance between each tip and node and their ancestral. This function needs either (1) \code{matrix}/\code{list} from \code{\link{nodes.coordinates}}; or a \code{tree} (\code{"phylo"}) and \code{full} (\code{"logical"}) argument to calculate the node coordinates for the direct descendants (\code{full = FALSE}) or all descendants down to the root (\code{full = TRUE}). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{centroids}: calculates the distance between each row and the centroid of the matrix. This function can take an optional arguments \code{centroid} for defining the centroid (if missing (default), the centroid of the matrix is used). This argument can be either a subset of coordinates matching the matrix's dimensions (e.g. \code{c(0, 1, 2)} for a matrix with three columns) or a single value to be the coordinates of the centroid (e.g. \code{centroid = 0} will set the centroid coordinates to \code{c(0, 0, 0)} for a three dimensional matrix). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{displacements}: calculates the ratio between the distance to the centroid (see \code{centroids} above) and the distance from a reference (by default the origin of the space). The reference can be changed through the \code{reference} argument. NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{neighbours}: calculates the distance to a neighbour. By default this is the distance to the nearest neighbour (\code{which = min}) but can be set to any dimension level - 1 function (e.g. \code{which = mean} gives the distance to the most average neighbour). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{pairwise.dist}: calculates the pairwise distance between elements - calls \code{vegdist(matrix, method = method, diag = FALSE, upper = FALSE, ...)}. The distance type can be changed via the \code{method} argument (see \code{\link[vegan]{vegdist}} - default: \code{method = "euclidean"}). This function outputs a vector of pairwise comparisons in the following order: d(A,B), d(A,C), d(B,C) for three elements A, B and C. NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#'   \item \code{quantiles}: calculates the quantile range of each axis of the matrix. The quantile can be changed using the \code{quantile} argument (default is \code{quantile = 95}, i.e. calculating the range on each axis that includes 95% of the data). An optional argument, \code{k.root}, can be set to \code{TRUE} to scale the ranges by using its \eqn{kth} root (where \eqn{k} are the number of dimensions). By default, \code{k.root = FALSE}.
#'
#'   \item \code{radius}: calculates a distance from the centre of each axis. The \code{type} argument is the function to select which distance to calculate. By default \code{type = max} calculates the maximum distance between the elements and the centre for each axis (i.e. the radius for each dimensions)
#'
#'   \item \code{ranges}: calculates the range of each axis of the matrix. An optional argument, \code{k.root}, can be set to \code{TRUE} to scale the ranges by using its \eqn{kth} root (where \eqn{k} are the number of dimensions). By default, \code{k.root = FALSE}.
#'
#'   \item \code{variances}: calculates the variance of each axis of the matrix. This function can also take the \code{k.root} optional argument described above.
#'
#' }
#' 
#' When used in the \code{\link{dispRity}} function, optional arguments are declared after the \code{metric} argument: for example
#'     \code{dispRity(data, metric = centroids, centroid = 0, method = "manhattan")}
#' 
#' @seealso \code{\link{dispRity}}
#'
#' @examples
#' ## A random matrix
#' dummy_matrix <- matrix(rnorm(90), 9, 10)
#'
#' ## ancestral.dist
#' ## A random treee with node labels
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
#' ## centroids
#' ## Distances between each row and centroid of the matrix
#' centroids(dummy_matrix)
#' ## Distances between each row and an arbitrary point
#' centroids(dummy_matrix, centroid = c(1,2,3,4,5,6,7,8,9,10))
#' ## Distances between each row and the origin
#' centroids(dummy_matrix, centroid = 0)
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
#' ## ellipse.volume
#' ## Ellipsoid volume of a matrix
#' ellipse.volume(dummy_matrix)
#' ## Calculating the same volume with provided eigen values
#' ordination <- prcomp(dummy_matrix)
#' ## Calculating the ellipsoid volume
#' ellipse.volume(ordination$x, eigen.value = ordination$sdev^2)
#'
#' ## neighbours
#' ## The nearest neighbour euclidean distances
#' neighbours(dummy_matrix)
#' ## The furthest neighbour manhattan distances
#' neighbours(dummy_matrix, which = max, method = "manhattan")
#'
#' ## mode.val
#' ## Modal value of a vector
#' mode.val(dummy_matrix)
#'
#' ## pairwise.dist
#' ## The pairwise distance
#' pairwise.dist(dummy_matrix)
#' ## The average squared pairwise distance
#' mean(pairwise.dist(dummy_matrix)^2)
#' ## equal to:
#' geiger::disparity(data = dummy_matrix)
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
#' @seealso \code{\link{dispRity}} and \code{\link{make.metric}}.
#'
#' @author Thomas Guillerme

## dimension level generic functions
dimension.level3.fun <- function(matrix, ...) {
    cat("No implemented Dimension level 3 functions implemented in dispRity!\n")
    cat("You can create your own by using: ?make.metric\n")
}

dimension.level2.fun <- function(matrix, ...) {

    cat("Dimension level 2 functions implemented in dispRity:\n")
    cat("?ranges\n")
    cat("?variances\n")
    cat("?centroids\n")
    cat("?ancestral.dist\n")
    cat("?pairwise.dist\n")
    cat("?radius\n")
}

dimension.level1.fun <- function(matrix, ...) {
    cat("Dimension level 1 functions implemented in dispRity:\n")
    cat("?ellipse.volume\n")
    cat("?convhull.surface\n")
    cat("?convhull.volume\n")
    cat("?diagonal\n")
    cat("?mode.val\n")
    cat("?span.tree.length\n")
    cat("?n.ball.volume\n")
}

## kth root scaling
k.root <- function(data, dimensions){
    return(data^(1/dimensions))
}

## Calculating the variance of each axis
variances <- function(matrix, k.root = FALSE) {
    if(!k.root) {
        return(apply(matrix, 2, var))
    } else {
        return(k.root(apply(matrix, 2, var), ncol(matrix)))
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
        return(as.vector(abs(diff(apply(matrix, 2, quantile, prob = CI.converter(quantile))))))
    } else {
        return(k.root(as.vector(abs(diff(apply(matrix, 2, quantile, prob = CI.converter(quantile))))), dimensions = ncol(matrix)))
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

## Calculating the distance from centroid
centroids <- function(matrix, centroid, method = "euclidean") {

    ## Select the fun distance
    switch(method,
        euclidean = {fun.dist <- fun.dist.euclidean},
        manhattan = {fun.dist <- fun.dist.manhattan}
    )

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
    ## Get the distance matrix
    distances <- as.matrix(vegan::vegdist(matrix, method))
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
        eigen.value <- abs(apply(var(matrix),2, sum)) # * (nrow(matrix) - 1)
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
    switch(method,
        euclidean = {fun.dist <- fun.dist.euclidean},
        manhattan = {fun.dist <- fun.dist.manhattan}
    )

    ## Checking if the nodes.coords is available
    if(missing(nodes.coords)) {
        if(!missing(tree) && !missing(full)) {
            nodes.coords <- nodes.coordinates(matrix, tree, full)
        } else {
            warning("Missing tree and full argument for nodes.coordinates.\nSee ?nodes.coordinates manual.\nThe centroids function was applied instead.")
            return(centroids(matrix, ...))
        }
    }
    
    switch(class(nodes.coords),
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
            mapply.fun.dist <- function(nodes.coords, matrix) {
                ## Converting the matrix into a list
                nodes.coords <- unlist(apply(nodes.coords, 1, list), recursive = FALSE)
                ## Calculate nodes.coords distance with multiple nodes.coords
                cent.dist <- mapply(fun.dist, row = matrix, centroid = nodes.coords)
                return(cent.dist)
            }

            ## Convert the matrix into a list
            matrix <- unlist(apply(matrix, 1, list), recursive = FALSE)

            ## Calculate nodes.coords distance with multiple centroids
            cent.dist <- lapply(nodes.coords, mapply.fun.dist, matrix)
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
    if(ncol(matrix) == nrow(matrix)) {
        ## Something like that for testing the triangularity
        if(all(sort(matrix[upper.tri(matrix)]) == sort(matrix[lower.tri(matrix)]))) {
            span_tree <- vegan::spantree(matrix, toolong = toolong)
        } else {
            span_tree <- vegan::spantree(vegan::vegdist(matrix, method = method), toolong = toolong)
        }
    } else {
        span_tree <- vegan::spantree(vegan::vegdist(matrix, method = method), toolong = toolong)
    }

    ## Output the span tree length
    return(sum(span_tree$dist))
}

## Calculates the pairwise distance between elements
pairwise.dist <- function(matrix, method = "euclidean", ...) {
    return(as.vector(vegan::vegdist(matrix, method = method, diag = FALSE, upper = FALSE, ...)))
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
#' @seealso \code{\link{ancestral.dist}}, \code{\link{dispRity.metric}}, \code{\link{dispRity}}
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


