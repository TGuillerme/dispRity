#' @title Disparity through time.
#'
#' @description Performs a disparity through time analysis.
#'
#' @param data An ordinated \code{matrix}.
#' @param tree A \code{phylo} object.
#' @param time A \code{numeric} value for the number of subsets to create.
#' @param metric A vector containing one to three functions (default = \code{c(median, centroids)}) (see \code{\link{dispRity}} for details).
#' @param ... Optional arguments to be passed to \code{\link{chrono.subsets}}, \code{\link{boot.matrix}} and \code{\link{dispRity}}.
#'
#' @details
#' By default the time subsets use \code{method = "discrete"}, the matrix is bootstrapped 100 times.
#' 
#' Note that this is a wrapper function that allows users to run a basic disparity-through-time analysis without too much effort. 
#' As such it has a lot of defaults described in the functions that make up the analysis. 
#' See \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}} for more details of the defaults used in each of these functions.
#' Note that any of these defaults can be changed within the \code{disparity.through.time} function.
#' 
#' @return
#' A \code{dispRity} object that can be passed to \code{summary} or \code{plot}.
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50) ; data(BeckLee_tree)
#' 
#' ## Run a simple disparity through time analysis (with three time bins)
#' result <- dispRity.through.time(BeckLee_mat50, BeckLee_tree, 3)
#' summary(result) ; plot(result)
#' 
#' ## This is equivalent to run the following decomposed code
#' dispRity(boot.matrix(chrono.subsets(BeckLee_mat50, BeckLee_tree, time = 3, method = "discrete"),
#'                      bootstraps = 100),
#'          metric = c(median, centroids))
#' 
#' @seealso \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme


dispRity.through.time <- function(data, tree, time, metric = c(median, centroids), ...) {

    ## Sanitising is passed to the individual functions

    ## Dealing with missing chrono.subsets arguments
    if(!methods::hasArg(method)) method <- "discrete"

    ## Creating the subsets
    data_subsetd <- chrono.subsets(data, tree, method, time = time, ...)

    ## Bootstrapping the matrixs
    data_bootstrapped <- boot.matrix(data_subsetd, ...)

    ## Measuring disparity
    return(dispRity(data_bootstrapped, metric = metric, ...))
}


#' @title Disparity in different groups.
#'
#' @description Performs a disparity analysis between groups.
#'
#' @param data An ordinated \code{matrix}.
#' @param group A \code{list} of row numbers for each group.
#' @param metric A vector containing one to three functions (default = \code{c(median, centroids)}) (see \code{\link{dispRity}} for details).
#' @param ... Optional arguments to be passed to \code{\link{custom.subsets}}, \code{\link{boot.matrix}} and \code{\link{dispRity}}.
#' 
#' @details
#' Note that this is a wrapper function that allows users to run a basic disparity among groups analysis without too much effort. 
#' As such it has a lot of defaults described in the functions that make up the analysis. 
#' See \code{\link{custom.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}} for more details of the defaults used in each of these functions.
#' Note that any of these defaults can be changed within the \code{disparity.through.time} function.
#'
#' @return
#' A \code{dispRity} object that can be passed to \code{summary} or \code{plot}.
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' 
#' ## Run a simple disparity per group analysis comparing stem and crown mammals
#' result <- dispRity.per.group(BeckLee_mat50, list(crown = c(16, 19:41, 45:50),
#'                              stem = c(1:15, 17:18, 42:44)))
#' summary(result) ; plot(result)
#' 
#' ## This is equivalent to run the following decomposed code
#' dispRity(boot.matrix(custom.subsets(BeckLee_mat50, list(crown = c(16, 19:41, 45:50),
#'                                                         stem = c(1:15, 17:18, 42:44))),
#'                      bootstraps = 100),
#'          metric = c(median, centroids))
#' 
#' @seealso \code{\link{custom.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme


dispRity.per.group <- function(data, group, metric = c(median, centroids), ...) {

    ## Sanitising is passed to the individual functions

    ## Creating the subsets
    data_subsetd <- custom.subsets(data, group, ...)

    ## Bootstrapping the matrix
    data_bootstrapped <- boot.matrix(data_subsetd, ...)

    ## Measuring disparity
    return(dispRity(data_bootstrapped, metric = metric, ...))
}