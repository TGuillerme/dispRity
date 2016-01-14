#' @name space.maker
#'
#' @title Creating some multidimensional spaces
#'
#' @description Creates a multidimensional space with a given number of elements and dimensions
#'
#' @param elements An \code{numeric} value.
#' @param dimensions An \code{numeric} value smaller than \code{elements}.
#' @param distribution One or more \code{function} to determine the distribution of the \code{elements} along each \code{dimensions}. The function must have a single input: \code{elements}.
#' @param ... Optional arguments to be passed to function.
#'
##' @details
#' 
#'
#' @examples
#' ## A square space
#' plot(space.maker(5000, 2, runif), pch=20)
#'
#' ## A circular space
#' plot(space.maker(5000, 2, rnorm), pch=20)
#'
#' ## A 2D cilindrical space
#' plot(space.maker(5000, 2, c(rnorm, runif)), pch=20)
#'
#' ## Not run:
#' ## A cube space
#' require(scatterplot3d)
#' scatterplot3d(space.maker(5000, 3, runif), pch=20)
#'
#' ## A sphere space
#' scatterplot3d(space.maker(5000, 3, rnorm), pch=20)
#'
#' ## A 3D cilindrical space
#' scatterplot3d(space.maker(5000, 3, c(rnorm, rnorm, runif)), pch=20)
#'
#' ## End(Not run)
#'
##' @seealso \code{\link{dispRity}} and \code{\link{make.metric}}.
#'
#' @author Thomas Guillerme

space.maker <- function(elements, dimensions, distribution, ...) {
    # SANITZING

    # elements
    check.class(elements, "numeric")
    check.length(elements, 1, msg = "'elements' must be a single numeric value.")

    # dimensions
    check.class(dimensions, "numeric")
    check.length(dimensions, 1, msg = "'dimensions' must be a single numeric value.")
    # must be smaller than elements
    if(dimensions > elements) stop("'dimensions' must be smaller than 'elements'")

    # distribution
    if(length(distribution) == 1) {
        uni_distribution <- TRUE
        check.class(distribution, "function")
    } else {
        uni_distribution <- FALSE
        lapply(as.list(distribution), check.class, "function")
        # if more distributions than dimensions, ignore the last ones
        if(length(distribution) > dimensions) warning(paste("There are more distributions than dimensions.\nOnly the first ", dimensions, "distributions will be used.", sep=""))
    }

    # CREATE THE SPACE
    # with only one distribution
    if(uni_distribution == TRUE) {
        space <- replicate(dimensions, distribution(elements, ...))
    } else {
    # with more than one distribution
        space <- as.matrix(lapply(distribution[1:dimensions], function(fun) return(fun(elements, ...))))
        #space <- as.matrix(lapply(distribution[1:dimensions], function(fun) return(fun(elements)))) ; warning("DEBUG")
        space <- matrix(unlist(space), nrow=elements, byrow=FALSE)
    }

    # Also introduce variance/covariance decrease?

    #output
    return(space)
}


# To get a perfect sphere
#space <- sqrt(3)*space/drop(sqrt((space^2) %*% rep(1, 3))) # see example in convhulln::geometry