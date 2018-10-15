#' @name space.maker
#'
#' @title Creating multidimensional spaces
#'
#' @description Creates a multidimensional space with a given number of elements and dimensions
#'
#' @param elements An \code{numeric} value.
#' @param dimensions An \code{numeric} value smaller than \code{elements}.
#' @param distribution One or more \code{functions} to determine the distribution of the \code{elements} along each \code{dimension}. The function must have a single input: \code{elements}.
#' @param arguments Optional \code{list} of arguments to be passed to the distributions functions in the order they appear (\code{default = NULL}, see details).
#' @param cor.matrix An optional correlation \code{matrix} of size \code{dimensions * dimensions} (\code{default = NULL}, see details).
#' @param scree An optional proportional \code{numeric} vector for approximating the \code{dimensions} variance (\code{default = NULL}, see details).
#'
#' @details
#' When passing additional arguments to different distributions, these must be given as a \code{list} to each function in the order they appear.
#' For example if \code{distribution = c(runif, rnorm, rgamma)} and one wants the distributions to be \code{runif(elements, min = 1, max = 10)}, \code{rnorm(elements, mean = 8)} and \code{rgamma(elements, shape = 1, log = TRUE)}, the additional arguments should be passed as
#' \code{c(list(min = 1, max = 10), list(mean = 8), list(shape = 1, log = TRUE)}. If no arguments have to be passed to a certain function, it can be left as \code{NULL} (e.g. \code{c(list(min = 1, max = 10), list(NULL), list(shape = 1, log = TRUE)}).
#'
#' The \code{cor.matrix} argument should be a correlation matrix between the dimensions.
#' If not \code{NULL}, the multidimensional space is multiplied by the the Choleski decomposition (\code{\link[base]{chol}}) of the correlation matrix.
#' The \code{scree} argument is simply a value multiplier for each dimension to adjust their variance to approximate the \code{scree} one. Its sum must be equal to 1.
#' 
#'
#' @examples
#' ## A square space
#' plot(space.maker(5000, 2, runif), pch = 20)
#'
#' ## A circular space
#' plot(space.maker(5000, 2, rnorm), pch = 20)
#'
#' ## A 2-dimensional cylindrical space
#' plot(space.maker(5000, 2, c(rnorm, runif)), pch = 20)
#'
#' ## A 4-dimensional space with different distributions
#' space.maker(5, 4, c(runif, runif, rnorm, rgamma),
#'      arguments = list(list(min = 1, max = 10), list(min = 1, max = 2),
#'      list(mean = 8), list(shape = 1)))
#' 
#' ## A 3-dimensional correlated space
#' cor_matrix <- matrix(cbind(1, 0.8 ,0.2, 0.8, 1, 0.7, 0.2, 0.7, 1), nrow = 3)
#' space <- space.maker(10000, 3, rnorm, cor.matrix = cor_matrix)
#' round(cor(space), 1) ; cor_matrix ## Both should be really similar matrices
#' 
#' ## A 3-dimensional space with a priori approximated variance for each dimension
#' space <- space.maker(10000, 3, rnorm, scree = c(0.6, 0.3, 0.1))
#' ## The resulting screeplot
#' barplot(apply(space, 2, var))
#' 
#' \dontrun{
#' require(scatterplot3d)
#' ## A cube space
#' scatterplot3d(space.maker(5000, 3, runif), pch = 20)
#' 
#' ## A plane space
#' scatterplot3d(space.maker(5000, 3, c(runif, runif, runif),
#'      arguments = list(list(min = 0, max = 0), NULL, NULL)), pch = 20)
#'
#' ## A sphere space
#' scatterplot3d(space.maker(5000, 3, rnorm), pch = 20)
#'
#' ## A 3D cylindrical space
#' scatterplot3d(space.maker(5000, 3, c(rnorm, rnorm, runif)), pch = 20)
#' 
#' ## Generating a doughnut space
#' doughnut <- space.maker(5000, 3, c(rnorm, random.circle),
#'      arguments = list(list(mean = 0), list(runif, inner = 0.5, outer = 1)))
#' ## Reodering the axis for projecting the dougnut in 2D
#' scatterplot3d(doughnut[,c(2,1,3)], pch = 20)
#' }
#'
#' @seealso \code{\link{null.test}}, \code{\link{test.dispRity}}.
#'
#' @author Thomas Guillerme

# #testing
# source("sanitizing.R")
# source("space.maker_fun.R")
# elements = 100
# dimensions = 3
# distribution = rnorm
# arguments = NULL
# cor.matrix = matrix(cbind(1,0.8,0.2, 0.8,1,0.7, 0.2,0.7,1), nrow = 3)
# scree = c(0.6, 0.3, 0.1)


# elements <- 5
# dimensions <- 3
# distribution <- c(random.circle, runif)
# arguments = list(list(distribution = runif, inner = 0.5, outer = 1), list(min = 0, max = 1))


# elements <- 5
# dimensions <- 3
# distribution <- random.circle
# arguments = list(list(distribution = runif, inner = 0.5, outer = 1))



# space <- space.maker(1000, 3, rnorm, cor.matrix = cor_matrix)
# space2 <- space.maker(1000, 3, rnorm, cor.matrix = NULL)

# scatterplot3d(space, pch = 20)


space.maker <- function(elements, dimensions, distribution, arguments = NULL, cor.matrix = NULL, scree = NULL) {
    ## SANITZING

    match_call <- match.call()

    ## elements
    check.class(elements, c("numeric", "integer")) -> silent
    check.length(elements, 1, msg = "'elements' must be a single numeric value.")

    ## dimensions
    check.class(dimensions, c("numeric", "integer")) -> silent
    check.length(dimensions, 1, msg = "'dimensions' must be a single numeric value.")
    # must be smaller than elements - No...
    #if(dimensions > elements) stop("'dimensions' must be smaller than 'elements'")

    ## distribution
    if(length(distribution) == 1) {
        uni_distribution <- TRUE
        check.class(distribution, "function")
    } else {
        uni_distribution <- FALSE
        lapply(as.list(distribution), check.class, "function")
        # if more distributions than dimensions, ignore the last ones
        if(length(distribution) > dimensions) warning(paste("There are more distributions than dimensions.\nOnly the first ", dimensions, "distributions will be used.", sep=""))
    }

    ## Random circle checks
    circle_fun <- ifelse(length(grep("random.circle", match_call$distribution)), TRUE, FALSE)

    ## Single function
    if(circle_fun && uni_distribution && dimensions %% 2 == 1) {
        dimensions <- dimensions - 1
        warning(paste0("random.circle function requires only an even number of dimensions.\nNumber of dimensions has been change to ", dimensions, "."))
    }

    ## Multiple functions
    if(circle_fun && !uni_distribution) {
        number_circle <- length(grep("random.circle", match_call$distribution))
        number_other <- length(distribution) - number_circle
        tmp <- number_circle * 2
        if((tmp + number_other) != dimensions) {
            dimensions <- tmp + number_other
            warning(paste0("random.circle function requires an even number of dimensions.\nNumber of dimensions has been change to ", dimensions, "."))
        }
    }

    ## arguments
    if(!is.null(arguments)) {
        # Must be a list
        check.class(arguments, "list")
        # Of same length as distribution
        check.length(arguments, length(distribution), msg = " must be a list of arguments of the same length as distribution.")
        # Add the $n elements to the list
        for(n in 1:length(arguments)) {
            arguments[[n]]$n <- elements
        }
    } else {
        if(circle_fun) {
            stop.call("", "random.circle requires arguments!")
        }
    }

    ## cor.matrix
    if(!is.null(cor.matrix)) {
        check.class(cor.matrix, "matrix")
        if(any(dim(cor.matrix) != dimensions)) {
            stop.call("", paste0("cor.matrix must be a square matrix of size ", dimensions, "*", dimensions, "."))
        }
    }

    ## scree
    if(!is.null(scree)) {
        check.class(scree, "numeric")
        check.length(scree, dimensions, msg = " must be of the same length as the dimensions argument", errorif = FALSE)
        if(sum(scree) != 1) {
            stop.call("", "scree argument must be a numeric vector summing to 1.")
        }
    }

    ## CREATE THE SPACE
    ## with only one distribution
    if(uni_distribution == TRUE) {

        if(!is.null(arguments)) {
            ## Setting the n argument
            arguments <- unlist(arguments, recursive = FALSE)
            ## Generating the space
            if(!circle_fun) {
                space <- replicate(dimensions, do.call(distribution, arguments))
            } else {
                space <- replicate(dimensions/2, do.call(distribution, arguments), simplify = FALSE)
                space <- do.call(cbind, space)
            }
        } else {
            ## Generate the space without arguments
            space <- replicate(dimensions, distribution(elements))    
        }

    } else {

        if(!is.null(arguments)) {
            ## Mapply the distribution with the arguments
            space <- mapply(do.call, distribution, arguments)
            if(circle_fun) {space <- do.call(cbind, space)}
        } else {
            ## Applying the function to the space
            space <- as.matrix(lapply(distribution[1:dimensions], function(fun) return(fun(elements))))
            space <- matrix(unlist(space), nrow=elements, byrow=FALSE)
        }
    }

    ## Apply the correlation matrix to the space (if !NULL)
    if(!is.null(cor.matrix)) {
        ## Choleski decomposition
        choleski_decomposition <- t(chol(cor.matrix))
        ## Multiply the matrices (transpose space)
        space <- choleski_decomposition %*% t(space)
        ## Transpose space again
        space <- t(space)
    }

    ## Modify the variance for each dimensions
    if(!is.null(scree)) {
        ## Variance corrector
        effective_var <- apply(space, 2, var)
        var_modifier <- effective_var * scree
        space <- t(t(space) * var_modifier)
    }

    #output
    return(space)
}

#' @title Random circle
#'
#' @description Creates coordinates for a random circle
#'
#' @param n The number of pairs x,y of coordinates.
#' @param distribution The distribution from which the coordinates are sampled.
#' @param inner Optional, the radius for an empty inner circle.
#' @param outer Optional, the maximum radius for the circle.
#' @param ... Any additional argument to be passed to \code{distribution}.
#' 
#' @examples
#' ## A simple uniform circle
#' plot(random.circle(1000, runif), pch = 20)
#' 
#' ## A normal ring with inner and outer boundaries
#' plot(random.circle(1000, rnorm, inner = 0.5, outer = 5), pch = 20)
#'
#' @seealso 
#' \code{\link{space.maker}}
#' 
#' @author Thomas Guillerme
#' @export

## Function for creating a rand
random.circle <- function(n, distribution, inner = 0, outer = Inf, ...) {
    return(t(replicate(n = n, rand.circle(distribution = distribution, inner = inner, outer = outer, ...))))
}

