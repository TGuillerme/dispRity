#' @title Calculates disparity from a matrix.
#'
#' @description Calculates disparity on a matrix or subsets of a matrix, where the disparity metric can be user specified.
#'
#' @param data A \code{matrix} or a \code{dispRity} object (see details).
#' @param metric A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details).
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param ... Optional arguments to be passed to the metric.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#          @param parallel Optional, either a \code{logical} argument whether to parallelise calculations (\code{TRUE}; the numbers of cores is automatically selected to n-1) or not (\code{FALSE}) or a single \code{numeric} value of the number of cores to use.
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{matrix}{the multidimensional space (a \code{matrix}).}
#' \item{call}{A \code{list} containing the called arguments.}
#' \item{subsets}{A \code{list} containing matrices pointing to the elements present in each subsets.}
#' \item{disparity}{A \code{list} containing the disparity in each subsets.}
#'
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @details  
#' The \code{dispRity} object given to the \code{data} argument can be: a list of matrices (typically output from the functions \code{\link{chrono.subsets}} or \code{\link{custom.subsets}}), a bootstrapped matrix output from \code{\link{boot.matrix}}, a list of disparity measurements calculated from the \code{dispRity} function or a \code{matrix} object with rows as elements and columns as dimensions. In any of these cases, the data is considered as the multidimensional space and is not transformed (e.g. if ordinated with negative eigen values, no correction is applied to the matrix).
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
#' ## Calculating the disparity from a customised subset
#' ## Generating the subsets
#' customised_subsets <- custom.subsets(BeckLee_mat50,
#'      list(group1 = 1:(nrow(BeckLee_mat50)/2),
#'           group2 = (nrow(BeckLee_mat50)/2):nrow(BeckLee_mat50)))
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 100)
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
# ## Bootstrapping a subset of matrices using four threads
# system.time(dispRity(bootstrapped_data, metric = c(sum, variances),
#      parallel = c(4, "SOCK")))
# ## System time is significantly longer! Using parallel is only an improvement
# ## for big datasets.
# }
#' 
#' @seealso \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}}.
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
# source("chrono.subsets.R") ; source("chrono.subsets_fun.R")
# source("custom.subsets.R") ; source("custom.subsets_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data_simple <- BeckLee_mat50
# data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
# data_subsets_simple <- chrono.subsets(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
# data_subsets_boot <- boot.matrix(data_subsets_simple, bootstraps = 11, rarefaction = c(5,6))
# metric = c(sum, variances)
# verbose = TRUE
# data <- data_subsets_boot

dispRity <- function(data, metric, dimensions, ..., verbose = FALSE){#, parallel) {
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
            stop.call(match_call$data, " must contain a matrix.")
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
        stop.call("", "Impossible to apply a dimension-level 3 metric on disparity data.")
    }

    ## Dimensions
    if(!missing(dimensions)) {
        ## Else must be a single numeric value (proportional)
        silent <- check.class(dimensions, c("numeric", "integer"), " must be a number or proportion of dimensions to keep.")
        check.length(dimensions, 1, " must be a number or proportion of dimensions to keep.", errorif = FALSE)
        if(dimensions < 0) {
            stop.call("", "Number of dimensions to remove cannot be less than 0.")
        }
        if(dimensions < 1) dimensions <- round(dimensions * ncol(data$matrix))
        if(dimensions > ncol(data$matrix)) {
            warning(paste0("Dimension number too high: set to ", ncol(data$matrix), "."))
            dimensions <- ncol(data$matrix)
        }
        data$call$dimensions <- dimensions
    }

    ## VERBOSE
    check.class(verbose, "logical")

    ## Parallel
    # if(missing(parallel)) {
    #     do_parallel <- FALSE
    # } else {
    #     do_parallel <- FALSE
    #     if(class(parallel) == "logical") {
    #         do_parallel <- parallel
    #     } else {
    #         if(class(parallel) == "numeric") {
    #             check.length(parallel, 1, msg = "Parallel must be either logical or a number of cores to use.")
    #             do_parallel <- TRUE
    #         } else {
    #             stop("Parallel must be either logical or a number of cores to use.")
    #         }
    #     }
    # }

    do_parallel <- FALSE

    ## ----------------------
    ## CALCULTING DISPARITY
    ## ----------------------

    ## Set matrix decomposition
    if(length(data$call$disparity$metrics) == 0) {
        ## Data call had no metric calculated yet
        matrix_decomposition <- TRUE

        ## Remove empty subsets or with only one data point
        elements <- unlist(lapply(lapply(data$subsets, lapply, nrow), `[[`, 1))
        elements_keep <- which(elements > 1)
        removed_elements <- ifelse(length(elements_keep) != length(elements), TRUE, FALSE)

        ## Lapply through the subsets
        lapply_loop <- data$subsets[elements_keep]

    } else {
        ## Data has already been decomposed
        matrix_decomposition <- FALSE
        ## Lapply through the disparity scores (serried)
        lapply_loop <- data$disparity

        ## No removed elements
        removed_elements <- FALSE
    }


    ## Select the elements if probabilities are used
    if(ncol(data$subsets[[1]]$elements) > 1 && matrix_decomposition) {
        ## Sample the elements
        selected_elements <- lapply(lapply_loop, function(X) elements.sampler(X$elements))
        for(subset in 1:length(selected_elements)) {
            lapply_loop[[subset]]$elements <- matrix(selected_elements[[subset]], ncol = 1)
        }
    }
    

    ## Initialising the cluster
    # if(do_parallel) {
    #     ## Selecting the number of cores
    #     cores <- ifelse(parallel == TRUE, parallel::detectCores() - 1, parallel)
    #     ## Initialise the cluster
    #     cluster <- parallel::makeCluster(cores)
    #     ## Checking for eventual additional arguments to export
    #     # additional_args <- list(...)
    #     # if(length(additional_args) > 0) {
    #     #     additional_args <- NULL
    #     # }
    
    #     ## Get the current environement
    #     current_env <- environment()

    #     ## Export from this environment
    #     parallel::clusterExport(cluster, c("data", "lapply_loop", "metrics_list", "matrix_decomposition", "parLapply.wrapper", "get.first.metric", "apply.decompose.matrix", "disparity.bootstraps.silent"), envir = current_env) #, "additional_args"
    # }


    # if(!do_parallel) {
        if(verbose) message("Calculating disparity", appendLF = FALSE)
        disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, ...)
        if(verbose) message("Done.", appendLF = FALSE)
    # } else {
    #     cat("Enter parlapply\n")
    #     disparity <- lapply(lapply_loop, parLapply.wrapper, cluster)
    #     cat("Exit parlapply\n")
    #     ## Stopping the cluster
    #     parallel::stopCluster(cluster)
    #     rm(cluster)
    # }

    ## Adding the removed elements as NAs
    if(removed_elements) {
        ## Creating empty disparity subsets
        empty_disparity <- lapply(data$subsets[which(elements <= 1)], lapply, function(x) ifelse(x, NA, NA))

        ## Merging the two subsets
        disparity <- c(disparity, empty_disparity)
        disparity <- disparity[match(names(data$subsets), names(disparity))]

        ## Prepare a warning message
        empty_group_names <- paste(names(which(elements <= 1)), collapse = ", ")
        subset <- ifelse(length(which(elements <=1)) > 1, "subsets", "subset")
        warning(paste("Disparity not calculated for", subset, empty_group_names, "(not enough data)."))
    }

    ## Update the disparity
    data$disparity <- disparity

    ## Update the call
    data$call$disparity$metrics <- c(data$call$disparity$metrics, match_call$metric)

    return(data)
}

