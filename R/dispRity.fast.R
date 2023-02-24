#' @title Fast dispRity
#'
#' @description Fast disparity calculations. THIS FUNCTION IS LESS SAFE TO USE THAN \code{\link{dispRity}} (see details).
#'
#' @param group a logical vector for grouping
#' @param space a matrix
#' @param metric a metric dispRity style (up to two levels)
#' @param ... any additional arguments to be passed to \code{metric}
#' 
#' @details
#' \emph{IN DOUBT, USE \code{\link{dispRity}} INSTEAD OF THIS FUNCTION.}
#' This function should only be used in very specific cases requiring advanced optimisation or embedded customised functions.
#' This function is simply applying \code{metric(space[group,])} for each group and returns a list of results.
#' It does not check the validity of the data, metric and groups.
#' It does not handle specific data (e.g. multiple matrices), specific metrics (e.g. no optional arguments), does not produce meaningful error messages and does not intake nor returns a dispRity object.
#' 
#' @examples
#' ## A random space
#' space <- matrix(rnorm(25), 5, 5)
#' 
#' ## A metric
#' metric <- c(sum, variances)
#' 
#' ## A group of four observations
#' group <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
#' 
#' ## The disparity
#' dispRity.fast(group, space, metric)
#'
#' 
#' @author Thomas Guillerme

dispRity.fast <- function(group, space, metric, ...) {
    ## Setting up the default args
    args <- list(matrix = space[group, , drop = FALSE], ...)
    ## Simple level 1 metric
    if(length(metric) == 1) {
        return(do.call(metric, args))
    } 
    ## Simple level 2 + 1 metric
    if(is.null(names(metric))) {
        return(metric[[1]](do.call(metric[[2]], args)))
    }
    # ## Handle the named arguments
    # args <- c(args, metric[-1])

    # ## Level 1 metric + args
    # if(length(metric[[1]]) == 1) {
    #     return(do.call(metric[[1]], args))
    # } 
    # ## Level 2 + 1 metric + args
    # return(metric[[1]][[1]](do.call(metric[[1]][[2]], args)))
}