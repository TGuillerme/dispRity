#' @name dispRity.metric
#' @aliases dimension.level3.fun dimension.level2.fun dimension.level1.fun between.groups.fun variances ranges centroids mode.val ellipse.volume edge.length.tree convhull.surface convhull.volume diagonal ancestral.dist pairwise.dist span.tree.length n.ball.volume radius neighbours displacements quantiles func.eve func.div angles deviations group.dist point.dist projections projections.tree projections.between
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
#'   \item \code{ellipse.volume}: calculates the ellipsoid volume of a matrix. This function tries to determine the nature of the input matrix and uses one of these following methods to calculate the volume. You can always specify the method using \code{method = "my_choice"} to overrun the automatic method choice.
#'      \itemize{
#'             \item \code{"eigen"}: this method directly calculates the eigen values from the input matrix (using \code{\link{eigen}}). This method is automatically selected if the input matrix is "distance like" (i.e. square with two mirrored triangles and a diagonal).
#'             \item \code{"pca"}: this method calculates the eigen values as the sum of the variances of the matrix (\code{abs(apply(var(matrix),2, sum))}). This is automatically selected if the input matrix is NOT "distance like". Note that this method is faster than \code{"eigen"} but only works if the input matrix is an ordinated matrix from a PCA, PCO, PCoA, NMDS or MDS.
#'             \item \code{"axes"}: this method calculates the actual semi axes length using the input matrix. It is never automatically selected. By default this method calculates the length of the major axes based on the 0.95 confidence interval ellipse but this can be modified by providing additional arguments from \code{\link{axis.covar}}.
#'             \item \code{<a numeric vector>}: finally, you can directly provide a numeric vector of eigen values. This method is never automatically selected and overrides any other options.
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
#'   \item \code{ancestral.dist}: calculates the distance between each elements coordinates in the matrix and their ancestors' coordinates (if \code{to.root = FALSE}; default) or to the root coordinates (if \code{to.root = TRUE}) for a given \code{tree}. The distance is calculate as Euclidean by default but can be changed through the \code{methods} argument (\code{method = "euclidean"}; default). Note that the matrix must contain data for both tips and nodes in the \code{tree}, otherwise you must provide a matrix to the argument \code{reference.data} that contains them. Note that if the function is used in \code{\link{dispRity}}, both the \code{tree} and \code{reference.data} can be automatically recycled from the \code{dispRity} object (if present).
#' 
#'   \item \code{angles}: calculates the angles of the main axis of variation per dimension in a \code{matrix}. The angles are calculated using the least square algorithm from the \code{\link[stats]{lm}} function. The unit of the angle can be changed through the \code{unit} argument (either \code{"degree"} (default), \code{radian} or \code{slope}) and a base angle to measure the angle from can be passed through the \code{base} argument (by default \code{base = 0}, measuring the angle from the horizontal line (note that the \code{base} argument has to be passed in the same unit as \code{unit}). When estimating the slope through \code{\link[stats]{lm}}, you can use the option \code{significant} to only consider significant slopes (\code{TRUE}) or not (\code{FALSE} - default).
#' 
#'   \item \code{centroids}: calculates the distance between each row and the centroid of the matrix (Lalibert'{e} 2010). This function can take an optional arguments \code{centroid} for defining the centroid (if missing (default), the centroid of the matrix is used). This argument can be either a subset of coordinates matching the matrix's dimensions (e.g. \code{c(0, 1, 2)} for a matrix with three columns) or a single value to be the coordinates of the centroid (e.g. \code{centroid = 0} will set the centroid coordinates to \code{c(0, 0, 0)} for a three dimensional matrix). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#'
#' \item \code{deviations}: calculates the minimal Euclidean distance between each element in and the hyperplane (or line if 2D, or a plane if 3D). You can specify equation of hyperplane of \emph{d} dimensions in the \eqn{intercept + ax + by + ... + nd = 0} format. For example the line \eqn{y = 3x + 1} should be entered as \code{c(1, 3, -1)} or the plane \eqn{x + 2y - 3z = 44} as \code{c(44, 1, 2, -3)}. If missing the \code{hyperplane} (default) is calculated using a least square regression using a gaussian \code{\link[stats]{glm}}. Extra arguments can be passed to \code{\link[stats]{glm}} through \code{...}. When estimating the hyperplane, you can use the option \code{significant} to only consider significant slopes (\code{TRUE}) or not (\code{FALSE} - default).
#'   \item \code{displacements}: calculates the ratio between the distance to the centroid (see \code{centroids} above) and the distance from a reference (by default the origin of the space). The reference can be changed through the \code{reference} argument. NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#' 
#'   \item \code{edge.length.tree}: calculates the edge length from a given tree for each elements present in the matrix. Each edge length is either measured between the element and the root of the tree (\code{to.root = TRUE} ; default) or between the element and its last ancestor (\code{to.root = FALSE}))
#' 
#'
#'   \item \code{neighbours}: calculates the distance to a neighbour (Foote 1990). By default this is the distance to the nearest neighbour (\code{which = min}) but can be set to any dimension level - 1 function (e.g. \code{which = mean} gives the distance to the most average neighbour). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument. 
#'
#'   \item \code{pairwise.dist}: calculates the pairwise distance between elements - calls \code{vegdist(matrix, method = method, diag = FALSE, upper = FALSE, ...)}. The distance type can be changed via the \code{method} argument (see \code{\link[vegan]{vegdist}} - default: \code{method = "euclidean"}). This function outputs a vector of pairwise comparisons in the following order: d(A,B), d(A,C), d(B,C) for three elements A, B and C. NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#' 
#'   \item \code{projections}: projects each element on a vector defined as (\code{point1}, \code{point2}) and measures some aspect of this projection. The different aspects that can be measured are:
#'  \itemize{
#'      \item \code{measure = "position"} (default), the distance of each element \emph{on} the vector (\code{point1}, \code{point2}). Negative values means the element projects on the opposite direction of the vector (\code{point1}, \code{point2}).
#'      \item \code{measure = "distance"}, the euclidean distance of each element \emph{from} the vector (\code{point1}, \code{point2}).
#'      \item \code{measure = "degree"}, the angle between the vector (\code{point1}, \code{point2}) and any vector (\code{point1}, \code{element}) in degrees.
#'      \item \code{measure = "radian"}, the angle between the vector (\code{point1}, \code{point2}) and any vector (\code{point1}, \code{element}) in radians.
#'  }
#' By default, \code{point1} is the centre of the space (coordinates \code{0, 0, 0, ...}) and \code{point2} is the centroid of the space (coordinates \code{colMeans(matrix)}). Coordinates for \code{point1} and \code{point2} can be given as a single value to be repeated (e.g. \code{point1 = 1} is translated into \code{point1 = c(1, 1, ...)}) or a specific set of coordinates.
#' Furtheremore, by default, the space is scaled so that the vector (\code{point1}, \code{point2}) becomes the unit vector (distance (\code{point1}, \code{point2}) is set to 1; option \code{scale = TRUE}; default). You can use the unit vector of the space using the option \code{scale = FALSE}.
#'
#'   \item \code{projections.tree}: calculates the \code{projections} metric but drawing the vectors from a phylogenetic tree. This metric can intake any argument from \code{projections} (see above) but for \code{point1} and \code{point2} that are replaced by the argument \code{type}. \code{type} is a \code{vector} or a \code{list} of two elements that designates which vector to draw and can be any pair of the following options (the first element being the origin of the vector and the second where the vector points to):
#'      \itemize{
#'          \item \code{"root"}: the root of the tree (the first element in tree$node.label);
#'          \item \code{"ancestor"}: the element's most recent ancestor;
#'          \item \code{"tips"}: the centroid of the tips;
#'          \item \code{"nodes"}: the centroid of all the nodes;
#'          \item \code{"livings"}: the centroid of the tips the furthest from the root;
#'          \item \code{"fossils"}: the centroid of all the tips that are not the furthest from the root;
#'          \item any numeric values that can be interpreted as \code{point1} and \code{point2} in \code{\link{projections}};
#'          \item or a user defined function that with the inputs \code{matrix} and \code{tree} and \code{row} (the element's ID, i.e. the row number in \code{matrix}). 
#'      }
#' \emph{NOTE:} the elements to calculate the origin and end points of the vector are calculated by default on the provided input \code{matrix} which can be missing data from the tree if used with \code{\link{custom.subsets}} or \code{\link{chrono.subsets}}. You can always provide the full matrix using the option \code{reference.data = my_matrix}.
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
#'    \item \code{group.dist}: calculates the distance between two groups (by default, this is the minimum euclidean vector norm distance between groups). Negative distances are considered as 0. This function must intake two matrices (\code{matrix} and \code{matrix2}) and the quantiles to consider. For the minimum distance between two groups, the 100th quantiles are considered (default: \code{probs = c(0,1)}) but this can be changed to any values (e.g. distance between the two groups accounting based on the 95th CI: \code{probs = c(0.025, 0.975)}; distance between centroids: \code{probs = c(0.5)}, etc...). This function is the linear algebra equivalent of the \code{\link[hypervolume]{hypervolume_distance}} function.
#' 
#' \item \code{point.dist}: calculates the distance between \code{matrix} and a point calculated from \code{matrix2}. By default, this point is the centroid of \code{matrix2}. This can be changed by passing a function to be applied to \code{matrix2} through the \code{point} argument (for example, for the centroid: \code{point.dist(..., point = colMeans)}). NOTE: distance is calculated as \code{"euclidean"} by default, this can be changed using the \code{method} argument.
#' 
#' \item \code{projections.between}: calculates the projection of the major axis between two matrices. It allows the same arguments as \code{projections}. If \code{point1} and \code{point2} are not provided (default), this function measures the major axis from both input matrices, centre their origins and projects the end of the vector of \code{matrix} onto the vector from \code{matrix2}. Which axis to measure can be changed with the option \code{axis} (for the major axis, \code{axis = 1}; default) and the confidence interval can be changed using \code{level} (for the 95 confidence interval, \code{level = 0.95}; default - see \code{\link{axis.covar}} for more details). If and input vector is provided through \code{point1} and \code{point2}, both matrices major axes are projected on the input vector.
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
#' ## Calculating the distances to the ancestors
#' ancestral.dist(dummy_matrix, tree = rand_tree)
#' ## Calculating the manhattan distances to the root
#' ancestral.dist(dummy_matrix, tree = rand_tree,
#'                to.root = TRUE, method = "manhattan")
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
#' ## edge.length.tree
#' ## Making a dummy tree with node labels
#' dummy_tree <- makeNodeLabel(rtree((nrow(dummy_matrix)/2)+1))
#' ## Naming the elements in the matrix
#' named_matrix <- dummy_matrix
#' rownames(named_matrix) <- c(dummy_tree$tip.label,
#'                             dummy_tree$node.label)
#' ## The total edge length of each element in the matrix (to the root)
#' edge.length.tree(named_matrix, tree = dummy_tree)
#' 
#' ## The edge lengths for each edge leading to the elements in the matrix
#' edge.length.tree(named_matrix, tree = dummy_tree, to.root = FALSE)
#' 
#' ## ellipse.volume
#' ## Ellipsoid volume of a matrix
#' ellipse.volume(dummy_matrix)
#' ## Calculating the same volume with provided eigen values
#' ordination <- prcomp(dummy_matrix)
#' ## Calculating the ellipsoid volume by providing your own eigen values
#' ellipse.volume(ordination$x, method = ordination$sdev^2)
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
#' ## projections
#' ## The distances on the vector defined from the centre of
#' ## the matrix to its centroid (default)
#' projections(dummy_matrix)
#' ## The distances from the vector defined from the third
#' ## element of the matrix to the point of coordinated
#' ## c(1,1,1, ...) the matrix to its centroid (default)
#' projections(dummy_matrix, measure = "distance",
#'             point1 = dummy_matrix[3, ],
#'             point2 = 1)
#'
#' ## projections.tree
#' ## Making a dummy tree with node labels
#' dummy_tree <- makeNodeLabel(rtree((nrow(dummy_matrix)/2)+1))
#' ## Naming the elements in the matrix
#' named_matrix <- dummy_matrix
#' rownames(named_matrix) <- c(dummy_tree$tip.label,
#'                             dummy_tree$node.label)
#' ## The projection on the vector defined from the root of
#' ## the tree to the ancestor of each element in the matrix
#' projections.tree(named_matrix, dummy_tree,
#'                   type = c("root", "ancestor"))
#' ## The rejection from the vector defined from the centroid
#' ## of the nodes to the centroids of the tips
#' projections.tree(named_matrix, dummy_tree,
#'                   type = c("nodes", "tips"),
#'                   measure = "distance")
#' ## A user function that define coordinates based on the 
#' ## centroid of the three first nodes
#' user.fun <- function(matrix, tree, row = NULL) {
#'      return(colMeans(matrix[tree$node.label[1:3], ]))
#' }
#' ## The projection on the vector defined by the coordinates
#' ## 0,0,0 and a user defined function
#' projections.tree(named_matrix, dummy_tree,
#'                   type = c(0, user.fun))
#'
#' ## projections.between
#' ## Two dummy matrices
#' matrix_1 <- matrix(rnorm(16), 4, 4)
#' matrix_2 <- matrix(rnorm(16), 4, 4)
#' ## Projecting the major axis of matrix_2 onto the one from matrix_1
#' projections.between(matrix_1, matrix_2)
#' ## Projecting both second major 0.75 axes
#' ## and getting the rejections (see projections() for option details)
#' projections.between(matrix_1, matrix_2,
#'                     measure = "distance",
#'                     axis = 2, level = 0.75)
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
    cat("Dimension level 2 functions implemented in dispRity:")
    cat("\n?ancestral.dist")
    cat("\n?angles")
    cat("\n?centroids")
    cat("\n?deviations")
    cat("\n?displacements")
    cat("\n?edge.length.tree")
    cat("\n?neighbours")
    cat("\n?pairwise.dist")
    cat("\n?point.dist")
    cat("\n?projections")
    cat("\n?projections.tree")
    cat("\n?ranges")
    cat("\n?radius")
    cat("\n?variances")
    cat("\n?span.tree.length")
}

dimension.level1.fun <- function(matrix, ...) {
    cat("Dimension level 1 functions implemented in dispRity:")
    cat("\n?convhull.surface")
    cat("\n?convhull.volume")
    cat("\n?diagonal")
    cat("\n?ellipse.volume")
    cat("\n?func.div")
    cat("\n?func.eve")
    cat("\n?group.dist")
    cat("\n?mode.val")
    cat("\n?n.ball.volume")
}

between.groups.fun <- function(matrix, matrix2, ...) {
    cat("Between groups functions implemented in dispRity:")
    cat("\n?group.dist # level 1")
    cat("\n?point.dist # level 2")
    cat("\n?projections.between # level 2")
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
mode.val <- function(matrix, ...){
    return(as.numeric(names(sort(-table(matrix))[1])))
}

## Calculate the ellipse volume of matrix
ellipse.volume <- function(matrix, method, ...) {

    ## Initialising the variables
    ncol_matrix <- ncol(matrix)

    ## Calculating the semi axes
    if(missing(method)) {
        ## Detect the method
        is_dist <- check.dist.matrix(matrix, just.check = TRUE)
        if(is_dist) {
            ## Use the eigen method
            method <- "eigen"
        } else {
            ## Use the pca method
            method <- "pca"
        }
    }

    if(is(method, "character")) {
        ## Select the semi axes
        semi_axes <- switch(method, 
            ## Simply get the eigen values
            "eigen" = {sqrt(eigen(matrix)$values)},
            ## The eigenvalue is equal to the sum of the variance/covariance within each axis (* nrow(matrix) as used in pco/pcoa)
            "pca"   = {sqrt(abs(apply(var(matrix, na.rm = TRUE), 2, sum)))},
            ## Calculate the 
            "axes"  = {(sapply(1:ncol(matrix), function(dim, VCV) {dist(get.one.axis(VCV, axis = dim, ...))}, VCV = matrix))/2})            
    } else {
        semi_axes <- method[1:ncol_matrix]
    }


    ## Volume (from https://keisan.casio.com/exec/system/1223381019)
    return(pi^(ncol_matrix/2)/gamma((ncol_matrix/2)+1)*prod(semi_axes))
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

## Get the distance for each row
get.ancestor.dist <- function(row, tree, data, fun.dist) {
    ## Get the ancestor
    ancestor <- c(tree$tip.label, tree$node.label)[tree$edge[tree$edge[,2] %in% which(c(tree$tip.label, tree$node.label) %in% row), 1]]
    ## Get the distance
    return(fun.dist(data[row, ], data[ancestor, ]))
}
get.root.dist <- function(row, tree, data, fun.dist) {
    ## Get the distance
    return(fun.dist(data[row, ], data[tree$node.label[1], ]))
}
ancestral.dist <- function(matrix, tree, to.root = FALSE, method = "euclidean", reference.data = matrix) {
    if(!to.root) {
        ## Get the distance between each tip/node and their ancestor
        return(sapply(rownames(matrix), get.ancestor.dist, tree = tree, data = reference.data, fun.dist = select.method(method)))
    } else {
        ## Get the distance between each tip/node and the root
        return(sapply(rownames(matrix), get.root.dist, tree = tree, data = reference.data, fun.dist = select.method(method)))
    }
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


## Select the right slope function
get.slope.significant <- function(X, base_angle) {
    model <- lm(base_angle ~ X)
    return(ifelse(summary(model)[[4]][[8]] < 0.05, model$coefficients[[2]], 0))
}
get.slope.nonsignificant <- function(X, base_angle) {
    lm(base_angle ~ X)$coefficients[[2]]
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

## Edge length tree
edge.length.tree <- function(matrix, tree, to.root = TRUE) {
    if(to.root) {
        ## Get the distances to the root
        out <- castor::get_all_distances_to_root(tree)[match(rownames(matrix), c(tree$tip.label, tree$node.label))]
    } else {
        ## Get just the edge length
        out <- tree$edge.length[match(match(rownames(matrix), c(tree$tip.label, tree$node.label)), tree$edge[,2])]
    }
    return(ifelse(is.na(out),0, out))
}

## Functions for the group.distance function
## Function for centreing a matrix on one specific centroid
centre.matrix <- function(matrix, group) {
    centre <- colMeans(matrix[group, , drop = FALSE])
    return(matrix - rep(centre, rep.int(nrow(matrix), ncol(matrix))))
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
    centroid <- colMeans(centred_matrix[groups[[2]], , drop = FALSE])
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

## Angle between two vectors
vector.angle <- function(v1, v2){
    return(acos(geometry::dot(v1, v2, d = 1) / (sqrt(sum(v1^2))*sqrt(sum(v2^2)))) *180/pi)
}
## Rotate a matrix along one axis (y)
get.rotation.matrix <- function(x, y){
    ## This magic comes from https://stackoverflow.com/questions/42520301/find-rotation-matrix-of-one-vector-to-another-using-r/42542385#42542385
    ## following: https://math.stackexchange.com/questions/598750/finding-the-rotation-matrix-in-n-dimensions
    ## Also this: http://wscg.zcu.cz/wscg2004/Papers_2004_Short/N29.pdf
    u <- x/sqrt(sum(x^2))

    v <- y-sum(u*y)*u
    v <- v/sqrt(sum(v^2))

    cost <- sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))
    sint <- sqrt(1-cost^2);

    return(diag(length(x)) - u %*% t(u) - v %*% t(v) + cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v)))
}
## Projection of elements on an axis
projections <- function(matrix, point1 = 0, point2 = colMeans(matrix), measure = "position", scaled = TRUE) {
    ## IMPORTANT: edits in this function must also be copy/pasted to dispRity.covar.projections_fun.R/projections.fast
    
    ## Get the point1 and point2
    if(length(point1) != ncol(matrix)) {
        point1 <- rep(point1, ncol(matrix))[1:ncol(matrix)]
    }
    if(length(point2) != ncol(matrix)) {
        point2 <- rep(point2, ncol(matrix))[1:ncol(matrix)]
    }

    ## Get the base vector
    base_vector <- rbind(point1, point2)

    ## Get all the space (with the two last rows being the base vectors)
    space <- rbind(matrix, base_vector)

    ## Centre the matrix on point1
    if(sum(point1) != 0) {
        ## Centre all the space
        space <- space - rep(point1, rep.int(nrow(space), ncol(space)))
        ## Re-attribute the centred variables
        matrix <- space[1:nrow(matrix), , drop = FALSE]
        base_vector <- space[-c(1:nrow(matrix)), , drop = FALSE]
    }

    ## Scale the space
    if(scaled) {
        ## The scaled space
        space <- space/dist(space[-c(1:nrow(matrix)), , drop = FALSE])
    }

    ## Get the base vector axis (x) and the projection vector (former unit vector; y)
    x <- base_vector[2, ]
    y <- c(sqrt(sum(base_vector[2,]^2)), rep(0, (ncol(matrix)-1)))
    ## If the base vector and the unit vector are different...
    if(any(x != y)) {
        ## ...rotate the matrix on the x-axis
        space <- space %*% get.rotation.matrix(x, y)
    }
    
    ## Re-attributing the matrix and the vector
    matrix <- space[1:nrow(matrix), , drop = FALSE]
    base_vector <- space[-c(1:nrow(matrix)), , drop = FALSE]

    ## Project the vectors
    projections <- t(apply(matrix, 1, geometry::dot, y = base_vector[2,], d = 2))
    ## Calculate the angles
    if(measure == "degree" || measure == "radian") {
        angles <- t(t(apply(matrix, 1, vector.angle, base_vector[2,])))
        angles <- ifelse(is.nan(angles), 0, angles)
    }

    # "position" #distance on
    # "distance" #distance from
    # "angle"    #angle between

    ## Measure the thingy
    values <- switch(measure,
        "position" = { #distance on
            ## Measure the position on the vectors and their orientation
            projections[,1]
        },
        "distance" = { #distance from
            ## Get the rejection distance
            apply(matrix - projections, 1, function(row) sqrt(sum(row^2)))
        },
        "degree"  = {
            c(angles)
        },
        "radian"  = {
            c(angles/180*pi)
        })

    return(unname(values))
}

## Projections between covar matrices
projections.between <- function(matrix, matrix2, axis = 1, level = 0.95, measure = "position", scaled = TRUE) {

    ## Get the main axes from the VCV matrices
    # source("covar.utilities_fun.R")
    base_vector  <- get.one.axis(matrix2, axis, level, dimensions = 1:length(diag(matrix2)))
    projected_vector <- get.one.axis(matrix, axis, level, dimensions = 1:length(diag(matrix)))

    ## Translating into projections format
    matrix <- projected_vector
    point1 <- base_vector[1,]
    point2 <- base_vector[2,]

    ## Moving the two axes so that there origins are the same:
    ## 1 - Get the translation vector to point1
    translation_vector <- point1 - matrix[1,]
 
    ## 2 - Align the matrix with point1
    matrix <- rbind(matrix[1,] + translation_vector,
                    matrix[2,] + translation_vector)

    ## Measure the projection
    return(projections(matrix, point1 = point1, point2 = point2, measure = measure, scaled = scaled)[-1]) #[-1] because the first value is the projection of the origin on the origin. Can be sometimes not equal to 0 though (but like something )
}

## Select the root coords
get.root <- function(matrix, tree, row = NULL) {
    return(matrix[tree$node.label[1], ])
}
## Select the elements' ancestor coords
get.ancestor <- function(matrix, tree, row) {
    ## Get the row name
    row_name <- rownames(matrix)[row]
    if(row_name == tree$node.label[1]) {
        ## If the row is the root, return itself
        return(matrix[tree$node.label[1], ])
    } else {
        ## Return the ancestor
        return(matrix[tree$node.label[tree$edge[tree$edge[,2] %in% which(c(tree$tip.label, tree$node.label) %in% row_name), 1] - Ntip(tree)], ])
    }
}
## Select the tips centroid coords
get.tips <- function(matrix, tree, row = NULL) {
    return(colMeans(matrix[tree$tip.label, ]))
}
## Select the nodes centroid coords
get.nodes <- function(matrix, tree, row = NULL) {
    return(colMeans(matrix[tree$node.label, ]))
}
## Select the livings centroid coords
get.livings <- function(matrix, tree, row = NULL) {
    elements_depths <- node.depth.edgelength(tree)
    return(colMeans(matrix[which(elements_depths == max(elements_depths)), , drop = FALSE]))
}
## Select the fossils centroid coords
get.fossils <- function(matrix, tree, row = NULL) {
    elements_depths <- node.depth.edgelength(tree)
    return(colMeans(matrix[which(elements_depths != max(elements_depths)), , drop = FALSE]))
}
## sapply wrapper for projections.tree
sapply.projections <- function(row, matrix, tree, from, to, reference.data = matrix
    , ...) {
    return(projections(matrix[row, , drop = FALSE], point1 = from(reference.data, tree, row), point2 = to(reference.data, tree, row), ...))
}

## Projection of elements on an axis
projections.tree <- function(matrix, tree, type = c("root","ancestor"), reference.data = matrix, ...) {
 
    ## Tracking whether the from_to is invariable or not
    invariables <- c(FALSE, FALSE)
    ## Select the to and from vectors
    from_to <- list()
    for(i in 1:2) {
        if(is(type[[i]], "character")) {
            invariables[[i]] <- ifelse(type[i] == "ancestor", FALSE, TRUE)
            ## Type is an inbuilt function
            from_to[[i]] <- switch(type[i],
                        "root"     = get.root,
                        "ancestor" = get.ancestor,
                        "tips"     = get.tips,
                        "nodes"    = get.nodes,
                        "livings"  = get.livings,
                        "fossils"  = get.fossils)
        } else {
            if(is(type[[i]], "function")) {
                ## Type is a user function
                from_to[[i]] <- type[[i]]
                ## Assume the user function is variable
                invariables[[i]] <- FALSE
            } else {
                if(is(type[[i]], "numeric") || is(type[[i]], "integer")) {
                    n_dim <- ncol(matrix)
                    var_out <- rep(type[[i]], n_dim)[1:n_dim]
                    ## Type is a number: functionise type
                    from_to[[i]] <- function(matrix, tree, row) {
                        return(var_out)
                    }
                    invariables[[i]] <- TRUE
                }
                # else {
                #     stop("type must be a list of containing functions, numeric values or one of the following inbuilt: \"root\", \"ancestor\", \"tips\", \"nodes\", \"livings\" or \"fossils\".", call. = FALSE)
                # }
            }
        }
    }

    if(all(invariables)) {
        ## Point1 and point2 are invariant
        return(projections(matrix, point1 = from_to[[1]](reference.data, tree), point2 = from_to[[2]](reference.data, tree), ...))
    } else {
        ## Apply the to and from to each row
        return(sapply(1:nrow(matrix), sapply.projections, matrix = matrix, tree = tree, from = from_to[[1]], to = from_to[[2]], reference.data, ...))
    }
}

