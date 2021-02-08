#' @title Reduce space
#'
#' @description Remove elements from a multidimensional space
#'
#' @param space the trait space
#' @param type how to reduce the space (either \code{"random"}, \code{"size"}, \code{"density"}, \code{"evenness"} or \code{"position"})
#' @param remove the proportion of elements to be removed (in probability)
#' @param parameters the parameter(s) for removal selection (see details). If left empty, the \code{parameters} is estimated to reach the amount set by \code{remove}.
#' @param tuning Optinal parameters for tuning the parameter estimations (if remove is required and parameters is missing) a list of three parameters: "max" for the maximum of operations, "tol" for the tuning (e.g. 0.1 close), "inc.steps" for the initial increment value during optimisation (default = 2 - the bigger the value, the slower the increment).
#' @param verbose wether to be verbose or not
#' @param return.optim logical, whether to also return the optimal value.
#' 
#' @details
#' 
#' The type of reductions algorithms select the proportion of elements to remove (from the \code{remove} parameter). The different algorithms are:
#' 
#' \itemize{
#'      \item \code{"random"} for randomly selecting a proportion of data points (using \code{sample(..., replace = FALSE)}).
#'      \item \code{"size"} for selecting the proportion of data points closer to the centre.
#'      \item \code{"density"} for selecting the proportion of data points with the lower nearest neigbhour distances.
#'      \item \code{"evenness"} for randomly selecting the proportion of data points from the regions with most density.
#' }
#' 
#' The parameters for each reduction type algorithms are:
#' \itemize{
#'      \item \code{"size"} parameters: a list of \code{parameters$centre}, the centre from which to count the radius (if missing, is set to \code{0}); and \code{parameters$radius}, the radius for removal.
#'      \item \code{"density"} parameters: a list of \code{parameters$what} "close" (default) for close neighbours or "distant" for distant ones; \code{parameters$diameter} the diameter for considering closeness or distance; \code{parameters$output} either "singles" or "pairs" to return the pairs of neighbours or one of them only (the first).
#'      \item \code{"position"} parameters: a list of \code{parameters$value}, value the threshold value from which to remove elements.
#'      \item \code{"evenness"} parameters: a list of \code{parameters$bw}, a bandwith selector function (\code{\link[stats]{bw.nrd0}} by default); and \code{parameters$power} a scaling factor for exaggerating the flatting/narrowing of the curve (the counts are set to this parameter exponent: default is \code{1}).
#' }
#' 
#' See Guillerme et al. 2020 and https://github.com/TGuillerme/moms for details.
#' 
#' 
#' @returns
#' A vector of \code{logical} values of the rows to remove selected by the function. \code{TRUE} corresponds to the following (and \code{FALSE} to the opposite):
#' \itemize{
#'      \item{"random"}: the randomly selected points.
#'      \item{"size"}: the points closer to the centre of the space.
#'      \item{"density"}: the points closer to each other.
#'      \item{"position"}: the points on the "positive" side of the space (typically upper right corner in 2D).
#'      \item{"evenness"}: the randomly select points from the higher density regions.
#' }
#' 
#' @examples
#' set.seed(1)
#' ## Creating a two dimensional space
#' space <- space.maker(100, 2, distribution = stats::rnorm)
#' 
#' ## Generating the four types of reductions
#' random <- reduce.space(space, "random", remove = 0.5)
#' size <- reduce.space(space, "size", remove = 0.5)
#' density <- reduce.space(space, "density", remove = 0.5)
#' position <- reduce.space(space, "position", remove = 0.5)
#' evenness <- reduce.space(space, "evenness", remove = 0.5)
#' 
#' ## Plotting the four different results
#' par(mfrow = c(3,2))
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(random)],
#'      main = "Random removal") 
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(size)],
#'      main = "Size removal")
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(density)],
#'      main = "Density removal")
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(position)],
#'      main = "Position removal")
#' plot(space, pch = 19, col = c("grey", "black")[as.factor(evenness)],
#'      main = "Evenness removal")
#' 
#' ## The space reduction with specific parameters:
#' # Using the point with coordinates (2,2) as the centre 
#' # Running over a maximum of 300 iterations
#' # With a tolerance of 0.05 (5%)
#' reduce.space(space, "size", remove = 0.2,
#'              parameters = list("centre" = c(2,2)), 
#'              tuning = list("max" = 300, "tol" = 0.05))
#' 
#' ## Remove a specific amount to match a specific parameter
#' reduce.space(space, type = "size", parameters = list("radius" = 1.206866))
#' 
#' 
#' @seealso \code{\link{test.metric}} \code{\link{dispRity}}
#' 
#' @author Thomas Guillerme
#' @references
#' Guillerme T, Puttick MN, Marcy AE, Weisbecker V. \bold{2020} Shifting spaces: Which disparity or dissimilarity measurement best summarize occupancy in multidimensional spaces?. Ecol Evol. 2020;00:1-16. (doi:10.1002/ece3.6452)
#' 

reduce.space <- function(space, type, remove, parameters, tuning, verbose = FALSE, return.optim = FALSE) {

    match_call <- match.call()

    ## Sanitizing
    ## space
    check.class(space, c("matrix", "data.frame"))

    ## type
    type_available <- c("random", "size", "position", "evenness", "density")
    check.method(type, type_available, "Reduction type")

    ## remove
    if(!missing(remove)) {
        check.class(remove, c("numeric", "integer"))
        check.length(remove, 1, " must be a single numeric value (probability or percentage).")
        if(remove >= 0) {
            if(remove > 1) {
                if(remove < 100) {
                    remove <- remove/100
                } else {
                    stop("remove must be a probability or a percentage.")
                }
            }
        } else {
            stop("remove must be a probability or a percentage.")
        }

        ## Straight returns if remove = 0 or 1
        if(remove == 1) {
            return(rep(TRUE, nrow(space)))
        }
        if(remove == 0) {
            return(rep(FALSE, nrow(space)))
        }
    }

    ## parameters
    if(missing(parameters)) {
        parameters <- list()
    } else {
        check.class(parameters, "list", " must be a named list of parameters.")
        if(is.null(names(parameters))) {
            stop.call(match_call$parameters, msg = " must be a named list of parameters.")
        }
    }

    ## Tolerance
    if(missing(tuning)) {
        tuning <- list()
    } else {
        check.class(tuning, "list", " must be a named list of tuning parameters.")
        if(is.null(names(tuning))) {
            stop.call(match_call$tuning, msg = " must be a named list of tuning parameters.")
        }
    }
    if(is.null(tuning$max)) {
        tuning$max <- 100
    }
    if(is.null(tuning$tol)) {
        tuning$tol <- 0.01
    }
    if(is.null(tuning$inc.steps)) {
        tuning$inc.steps <- 2
    }

    ## verbose and optim
    check.class(verbose, "logical")
    check.class(return.optim, "logical")


    ## Select the reduction type algorithm
    switch(type,
        random = {
            ## Number of elements
            elements <- nrow(space)
            ## Return a portion of the space
            to_remove <- sample(1:elements, elements*remove)
            return(1:elements %in% to_remove)
        },
        size = {
            ## Type function
            fun <- run.size.removal
            ## Parameters
            if(is.null(parameters$centre)) {
                parameters$centre <- apply(space, 2, mean)
            } 
            if(is.null(parameters$radius)) {
                parameters$radius <- diff(range(space[,1]/2))
            }
            ## Parameter to optimise
            parameters$optimise <- parameters$radius
            ## List of arguments
            args <- list("space" = space, "parameters" = parameters)
        },
        position = {
            ## Type function
            fun <- run.size.removal
            ## Parameters
            if(is.null(parameters$centre)) {
                parameters$centre <- apply(space, 2, max)
            } 
            if(is.null(parameters$radius)) {
                parameters$radius <- 1
            }
            ## Parameter to optimise
            parameters$optimise <- parameters$radius
            ## List of arguments
            args <- list("space" = space, "parameters" = parameters)
        },
        density = {
            ## Type function
            fun <- run.density.removal
            ## Parameters
            if(is.null(parameters$distance)) {
                parameters$distance <- as.matrix(dist(space))
            }
            if(is.null(parameters$diameter)) {
                parameters$diameter <- 0.5
            }  
            ## Parameter to optimise
            parameters$optimise <- parameters$diameter
        },
        evenness = {
            ## Parameters
            if(is.null(parameters$bw)) {
                parameters$bw <- bw.nrd0
            }
            if(is.null(parameters$power)) {
                parameters$power <- 1
            }
            ## Make the probability vector
            prob_vector <- get.prob.vector(space, bw = parameters$bw, power = parameters$power)

            ## Number of elements
            elements <- nrow(space)

            ## Return a portion of the space
            to_remove <- sample(1:elements, elements*remove,
                                prob = prob_vector)
            return(1:elements %in% to_remove)
        }
    )

    ## List of arguments
    args <- list("space" = space, "parameters" = parameters)
    ## Run the complex removal
    to_remove <- do.call(fun, args)

    ## Optimise the function (if necessary)
    if(!missing(remove)) {

        ## Get out of the corner case of all being TRUE or FALSE
        if(all(to_remove) || all(!to_remove)) {
            args$parameters$optimise <- runif(1)
            to_remove <- do.call(fun, args)
        }

        ## Optimise
        to_remove <- optimise.results(to_remove, fun = fun, remove = remove, args = args, tuning = tuning, verbose = verbose, space = space, return.optim = return.optim)
    }

    if(!return.optim) {
        return(to_remove)
    } else {
        return(list(remove = to_remove$remove, optim = to_remove$optim))
    }
}