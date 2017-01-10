#' @name space.maker
#'
#' @title Creating some multidimensional spaces
#'
#' @description Creates a multidimensional space with a given number of elements and dimensions
#'
#' @param elements An \code{numeric} value.
#' @param dimensions An \code{numeric} value smaller than \code{elements}.
#' @param distribution One or more \code{function} to determine the distribution of the \code{elements} along each \code{dimensions}. The function must have a single input: \code{elements}.
#' @param arguments Optional \code{list} of arguments to be passed to the distributions functions in the order they appear (\code{default = NULL}, see details).
#' @param cor.matrix An optional correlation \code{matrix} of size \code{dimensions * dimensions} (\code{default = NULL}, see details).
#'
#' @details
#' For passing some additional arguments to different distributions, they must be given as a \code{list} to each function in the order they appear.
#' For example if \code{distribution = c(runif, rnorm, rgamma)} and one wants the distributions to be \code{runif(elements, min = 1, max = 10)}, \code{rnorm(elements, mean = 8)} and \code{rgamma(elements, shape = 1, log = TRUE)}, the additional arguments sould be passed as
#' \code{c(list(min = 1, max = 10), list(mean = 8), list(shape = 1, log = TRUE)}. If no arguments have to be passed to a certain function, it can be left as \code{NULL} (e.g. \code{c(list(min = 1, max = 10), list(NULL), list(shape = 1, log = TRUE)}).
#'
#' The \code{cor.matrix} argument should be a correlation matrix between the dimensions.
#' If not \code{NULL}, the multidimensional space is multiplied by the the Choleski decomposition (\code{\link[base]{chol}}) of the correlation matrix.
#' 
#'
#' @examples
#' ## A square space
#' plot(space.maker(5000, 2, runif), pch = 20)
#'
#' ## A circular space
#' plot(space.maker(5000, 2, rnorm), pch = 20)
#' 
#'
#' ## A 2D cilindrical space
#' plot(space.maker(5000, 2, c(rnorm, runif)), pch = 20)
#'
#' ## A 4-dimensional space with different distributions
#' space.maker(5, 4, c(runif, runif, rnorm, rgamma),
#'      arguments = list(list(min = 1, max = 10), list(min = 1, max = 2),
#'      list(mean = 8), list(shape = 1)))
#' 
#' ## A 3 dimensional correlated space
#' cor_matrix <- matrix(cbind(1,0.8,0.2, 0.8,1,0.7, 0.2,0.7,1), nrow = 3)
#' space <- space.maker(10000, 3, rnorm, cor.matrix = cor_matrix)
#' round(cor(space), 1) ; cor_matrix ## Both should be really similar matrices
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
# cor_matrix = matrix(cbind(1,0.8,0.2, 0.8,1,0.7, 0.2,0.7,1), nrow = 3)

# space <- space.maker(1000, 3, rnorm, cor.matrix = cor_matrix)
# space2 <- space.maker(1000, 3, rnorm, cor.matrix = NULL)

# scatterplot3d(space, pch = 20)


space.maker <- function(elements, dimensions, distribution, arguments = NULL, cor.matrix = NULL) {
    # SANITZING

    # elements
    check.class(elements, c("numeric", "integer")) -> silent
    check.length(elements, 1, msg = "'elements' must be a single numeric value.")

    # dimensions
    check.class(dimensions, c("numeric", "integer")) -> silent
    check.length(dimensions, 1, msg = "'dimensions' must be a single numeric value.")
    # must be smaller than elements - No...
    #if(dimensions > elements) stop("'dimensions' must be smaller than 'elements'")

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

    # arguments
    if(!is.null(arguments)) {
        # Must be a list
        check.class(arguments, "list")
        # Of same length as distribution
        check.length(arguments, length(distribution), msg = " must be a list of arguments list of the same length as distribution.")
        # Add the $n elements to the list
        for(n in 1:length(arguments)) {
            arguments[[n]]$n <- elements
        }
    }

    #cor.matrix
    if(!is.null(cor.matrix)) {
        check.class(cor.matrix, "matrix")
        if(any(dim(cor.matrix) != dimensions)) {
            stop("cor.matrix must be a square matrix of size 'dimensions' * 'dimensions'.")
        }
    }

    # CREATE THE SPACE
    # with only one distribution
    if(uni_distribution == TRUE) {

        if(!is.null(arguments)) {
            #Setting the n argument
            arguments <- unlist(arguments, recursive = FALSE)
            #Generating the space
            space <- replicate(dimensions, do.call(distribution, arguments))
        } else {
            #Generate the space without arguments
            space <- replicate(dimensions, distribution(elements))    
        }

    } else {

        if(!is.null(arguments)) {
            #Mapply the distribution with the arguments
            space <- mapply(do.call, distribution, arguments)
        } else {
            #Applying the function to the space
            space <- as.matrix(lapply(distribution[1:dimensions], function(fun) return(fun(elements))))
            space <- matrix(unlist(space), nrow=elements, byrow=FALSE)
        }
    }

    #Apply the correlation matrix to the space (if !NULL)
    if(!is.null(cor.matrix)) {
        #Choleski decomposition
        choleski_decomposition <- t(chol(cor.matrix))
        #Multiply the matrices (transpose space)
        space <- choleski_decomposition %*% t(space)
        #Transpose space again
        space <- t(space)
    }

    #output
    return(space)
}