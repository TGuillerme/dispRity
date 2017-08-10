#' @title Calculates disparity from an ordinated matrix.
#'
#' @description Calculates disparity on an ordinated matrix or subsamples of a matrix, where the disparity metric can be user specified.
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)}, or a \code{dispRity} object (see details).
#' @param metric A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details).
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param ... Optional arguments to be passed to the metric.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
# @param parallel An optional vector containing the number of parallel threads and the virtual connection process type to run the function in parallel (requires \code{snow} package; see \code{\link[snow]{makeCluster}} function).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the observed and bootstrapped matrices.}
#' \item{disparity}{A \code{list} of disparity values (containing the observed disparity and the bootstrapped disparities).}
#' \item{elements}{A \code{vector} containing all the names of the elements from the original matrix.}
#' \item{subsamples}{A \code{vector} containing the name of the subsamples (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstrapping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @details  
#' The \code{dispRity} object given to the \code{data} argument can be: a list of matrices (typically output from the functions \code{\link{time.subsamples}} or \code{\link{cust.subsamples}}), a bootstrapped matrix output from \code{\link{boot.matrix}} or a list of disparity measurements calculated from the \code{dispRity} function.
#' 
#' \code{metric} should be input as a vector of functions.
#' The functions are sorted and used by dimension-level from 3 to 1 (see \code{\link{dispRity.metric}} and \code{\link{make.metric}}).
#' Typically dimension-level 3 functions take a \code{matrix} and output a \code{matrix}; dimension-level 2 functions take a \code{matrix} and output a \code{vector} and dimension-level 1 functions take a \code{matrix} or a \code{vector} and output a single value.
#' When more than one function is input, they are treated first by dimension-level (i.e. 3, 2 and finally 1).
#' Note that the functions can only take one metric of each dimension-level and thus can only take a maximum of three arguments!
#' 
#' Some metric functions are built into the \code{dispRity} package: see \code{\link{dispRity.metric}}
#' For user specified metrics, please use \code{\link{make.metric}} to ensure that the metric will work.
#' 
#' \emph{HINT:} if using more than three functions you can always create your own function that uses more than one function (e.g. \code{my_function <- function(matrix) cor(var(matrix))} is perfectly valid and allows one to use two dimension-level 3 functions - the correlation of the variance-covariance matrix in this case).
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity as the sum of variances from a single matrix
#' sum_of_variances <- dispRity(BeckLee_mat50, metric = c(sum, variances))
#' summary(sum_of_variances)
#' ## Bootstrapping this value
#' bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps = 100)
#' dispRity(bootstrapped_data, metric = c(sum, variances))
#'
#' ## Calculating the disparity from a customised subsample
#' ## Generating the subsamples
#' customised_subsamples <- custom.subsamples(BeckLee_mat50,
#'      list(group1 = 1:(nrow(BeckLee_mat50)/2),
#'           group2 = (nrow(BeckLee_mat50)/2):nrow(BeckLee_mat50)))
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#' summary(sum_of_variances)
#' 
#' ## Calculating disparity with different metrics of different dimension-levels
#' ## Disparity is calculated as the distribution of the variances in each
#' ## dimension (output are distributions)
#' disparity_level2 <- dispRity(BeckLee_mat50, metric = variances)
#' ## Disparity is calculated as the mean of the variances in each dimension 
#' ## (output are single values)
#' disparity_level1 <- dispRity(disparity_level2, metric = mean)
#' ## Both disparities have the same means but dimension-level 1 has no quantiles
#' summary(disparity_level2)
#' summary(disparity_level1)
#'
# \dontrun{
# ## Calculating disparity using one thread
# system.time(dispRity(bootstrapped_data, metric = c(sum, variances)))
# ## Bootstrapping a subsample of matrices using four threads
# system.time(dispRity(bootstrapped_data, metric = c(sum, variances),
#      parallel = c(4, "SOCK")))
# ## System time is significantly longer! Using parallel is only an improvement
# ## for big datasets.
# }
#' 
#' @seealso \code{\link{custom.subsamples}}, \code{\link{time.subsamples}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG dispRity.R")
# library(dispRity)
# source("sanitizing.R")
# source("dispRity_fun.R")
# source("dispRity.metric.R")
# source("dispRity.utilities.R")
# source("boot.matrix.R") ; source("boot.matrix_fun.R")
# source("time.subsamples.R") ; source("time.subsamples_fun.R")
# source("cust.subsamples.R") ; source("cust.subsamples_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data_simple <- BeckLee_mat50
# data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
# data_subsamples_simple <- time.subsamples(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
# data_subsamples_boot <- boot.matrix(data_subsamples_simple, bootstraps = 11, rarefaction = c(5,6))
# metric = c(sum, variances)
# verbose = TRUE
# data <- data_subsamples_boot

dispRity <- function(data, metric, dimensions, ..., verbose = FALSE) { #parallel
    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    
    ## Saving the call
    match_call <- match.call()

    # warning("DEBUG") ; return(match_call)

    ## Check data class
    if(class(data) != "dispRity") {
        check.class(data, "matrix")
        ## Create the dispRity object
        data <- fill.dispRity(make.dispRity(data = data))
    } else {
        ## Making sure matrix exist
        if(is.null(data$matrix)) {
            stop(paste(match_call$data, "must contain a matrix."))
        }
        ## Make sure dimensions exist in the call
        if(is.null(data$call$dimensions)) {
            data$call$dimensions <- ncol(data$matrix)
        }
    }

    ## Get the metric list
    metrics_list <- get.dispRity.metric.handle(metric, match_call)

    ## Stop if data already contains disparity and metric is not level1
    if(!is.null(metrics_list$level3.fun) && length(data$call$disparity$metric) != 0) {
        stop("Impossible to apply a dimension-level 3 metric on disparity data.")
    }

    ## Dimensions
    if(!missing(dimensions)) {
        ## Else must be a single numeric value (proportional)
        check.class(dimensions, "numeric", " must be logical or a proportional threshold value.")
        check.length(dimensions, 1, " must be logical or a proportional threshold value.", errorif = FALSE)
        if(dimensions < 0) stop("Number of dimensions to remove cannot be less than 0.")
        if(dimensions < 1) dimensions <- round(dimensions * ncol(data$matrix))
        if(dimensions > ncol(data$matrix)) stop("Number of dimensions to remove cannot be more than the number of columns in the matrix.")
        data$call$dimensions <- dimensions
    }

    ## VERBOSE
    check.class(verbose, "logical")

    ## Parallel
    # if(missing(parallel)) {
        do_parallel <- FALSE
    # } else {
    #     do_parallel <- FALSE
    #     warning("parallel option not implemented yet.")
    #     # check.length(parallel, 2, " must be a vector containing the number of threads and the virtual connection process type.")
    #     # check.class(as.numeric(parallel[1]), "numeric", " must be a vector containing the number of threads and the virtual connection process type.")
    #     # check.class(parallel[2], "character", " must be a vector containing the number of threads and the virtual connection process type.")
    #     # cluster <- makeCluster(as.numeric(parallel[1]), parallel[2])
    # }

    ## ----------------------
    ## CALCULTING DISPARITY
    ## ----------------------

    ## Set matrix decomposition
    if(length(data$call$disparity$metrics) == 0) {
        ## Data call had no metric calculated yet
        matrix_decomposition <- TRUE
        ## Lapply through the subsamples
        lapply_loop <- data$subsamples
    } else {
        ## Data has already been decomposed
        matrix_decomposition <- FALSE
        ## Lapply through the disparity scores (serried)
        lapply_loop <- data$disparity
    }
    
    # if(!do_parallel) {
        if(verbose) message("Calculating disparity", appendLF = FALSE)
        disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, ...)
        #disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose) ; warning("DEBUG dispRity.R")
        if(verbose) message("Done.", appendLF = FALSE)
    # } else {
    #     disparity <- parLapply(clust, lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, ...)
    #     stopCluster(cluster)
    # }

    ## Update the disparity
    data$disparity <- disparity

    ## Update the call
    data$call$disparity$metrics <- c(data$call$disparity$metrics, match_call$metric)

    return(data)
}

