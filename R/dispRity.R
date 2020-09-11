#' @title Calculates disparity from a matrix.
#'
#' @description Calculates disparity from a matrix, a list of matrices or subsets of a matrix, where the disparity metric can be user specified.
#'
#' @param data A matrix or a \code{dispRity} object (see details).
#' @param metric A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details).
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param ... Optional arguments to be passed to the metric.
#' @param between.groups A \code{logical} value indicating whether to run the calculations between groups (\code{TRUE}) or not (\code{FALSE} - default) or a \code{numeric} list of pairs of groups to run (see details).
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
#' The \code{between.groups} argument runs the disparity between groups rather within groups. If \code{between.groups = TRUE}, the disparity will be calculated using the following inputs:
#' \itemize{
#'      \item if the input is an output from \code{\link{custom.subsets}}, the series are run in a pairwise manner using \code{metric(matrix, matrix2)}. For example for a \code{custom.subset} contains 3 subsets m1, m2 and m3, the code loops through: \code{metric(m1, m2)}, \code{metric(m2, m3)} and \code{metric(m1, m3)} (looping through \code{list(c(1,2), c(2,3), c(3,1))}).
#'      \item if the input is an output from \code{\link{chrono.subsets}}, the series are run in a paired series manner using \code{metric(matrix, matrix2)}. For example for a \code{chrono.subsets} contains 3 subsets m1, m2, m3 and m4, the code loops through: \code{metric(m1, m2)} and \code{metric(m2, m3)} (looping through \code{list(c(1,2), c(2,3), c(3,4))}).
#' }
#' In both cases it is also possible to specify the input directly by providing the list to loop through. For example using \code{between.groups = list(c(1,2), c(2,1), c(4,8))} will apply the \code{metric} to the 1st and 2nd subsets, the 2nd and first and the 4th and 8th (in that specific order).
#' 
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

dispRity <- function(data, metric, dimensions, ..., between.groups = FALSE, verbose = FALSE){#, parallel) {
    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    
    ## Saving the call
    match_call <- match.call()
    dots <- list(...)

    # warning("DEBUG") ; return(match_call)

    ## Check data input
    if(!is(data, "dispRity")) {
        data <- fill.dispRity(make.dispRity(data = check.dispRity.data(data)))
    } else {
        ## Making sure matrix exist
        if(is.null(data$matrix[[1]])) {
            stop.call(match_call$data, " must contain a matrix or a list of matrices.")
        }
        ## Make sure dimensions exist in the call
        if(is.null(data$call$dimensions)) {
            data$call$dimensions <- ncol(data$matrix[[1]])
        }
    }

    ## Get the metric list
    metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data$matrix[[1]]), ...)
    # metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data$matrix[[1]]))
    metric_is_between.groups <- unlist(metrics_list$between.groups)
    metrics_list <- metrics_list$levels

    ## Stop if data already contains disparity and metric is not level1
    if(!is.null(metrics_list$level3.fun) && length(data$call$disparity$metric) != 0) {
        stop.call("", "Impossible to apply a dimension-level 3 metric on disparity data.")
    }

    ## Check if metrics are already present whether metrics can be applied
    if(!is.null(data$call$disparity$metrics$fun)) {
        ## Check which level of metrics have already been applied
        if(length(data$call$disparity$metrics$fun) == 1) {
            applied_levels <- make.metric(data$call$disparity$metrics$fun, silent = TRUE)
        } else {
            applied_levels <- unlist(lapply(data$call$disparity$metrics$fun, make.metric, silent = TRUE))
        }

        ## Can maybe not take a level 2 or 3 metric
        if(any(applied_levels == "level1") && (!is.null(metrics_list$level3.fun) || !is.null(metrics_list$level2.fun))) {
            stop.call(msg.pre = "At least one metric dimension level 1 was already calculated for ", call = match_call$data, msg = ".\nImpossible to apply a metric higher than dimension level 1.")
        }
        ## Check if the metric is not between groups but the previous was.
        if(between.groups == FALSE && data$call$disparity$metrics$between.groups) {
            warning("The disparity calculation (metric = ", as.expression(match_call$metric), ") is not calculated between groups (between.groups = FALSE) but the input data (", as.expression(match_call$data), ") contained a between groups calculation. The metric is thus only applied to the groups (not between them). If this is not the desired behaviour, use the following option:\n    dispRity(..., between.groups = TRUE)" )
            ## Change the between groups behaviour
            data$call$disparity <- NULL
            data$disparity <- NULL
        }
        ## Check the opposite
        if(between.groups == TRUE && !data$call$disparity$metrics$between.groups) {
            warning("The disparity calculation (metric = ", as.expression(match_call$metric), ") is calculated between groups (between.groups = TRUE) but the input data (", as.expression(match_call$data), ") contained no between groups calculation. The metric is thus only applied between the groups (not to the previously calculated disparity). If this is not the desired behaviour, use the following option:\n    dispRity(..., between.groups = FALSE)" )
            ## Change the between groups behaviour
            data$call$disparity <- NULL
            data$disparity <- NULL
        }
    }

    ## Check if the subsets contains probabilities or not
    has_probabilities <- ifelse(length(grep("\\.split", data$call$subsets)) == 0, FALSE, TRUE)

    ## Dimensions
    if(!missing(dimensions)) {
        ## Else must be a single numeric value (proportional)
        silent <- check.class(dimensions, c("numeric", "integer"), " must be a number or proportion of dimensions to keep.")
        check.length(dimensions, 1, " must be a number or proportion of dimensions to keep.", errorif = FALSE)
        if(dimensions < 0) {
            stop.call("", "Number of dimensions to remove cannot be less than 0.")
        }
        if(dimensions < 1) dimensions <- round(dimensions * ncol(data$matrix[[1]]))
        if(dimensions > ncol(data$matrix[[1]])) {
            warning(paste0("Dimension number too high: set to ", ncol(data$matrix[[1]]), "."))
            dimensions <- ncol(data$matrix[[1]])
        }
        data$call$dimensions <- dimensions
    }

    ## VERBOSE
    check.class(verbose, "logical")

    ## Serial
    is_between.groups <- FALSE
    between.groups_class <- check.class(between.groups, c("logical", "list"), " must be logical or a list of pairs of comparisons.")
    ## Check whether logical class can be applied
    if(between.groups_class == "logical") {
        if(between.groups) {
            if(!any(metric_is_between.groups)) {
                stop.call(msg.pre = "The provided metric (", match_call$metric, msg = ") cannot be applied between groups. \"between.groups\" metrics must have at least \"matrix\" and \"matrix2\" as inputs.")
            }
            ## Make the series
            if(is.null(data$call$subsets)) {
                stop.call(msg.pre = "The provided \"between.groups\" metric (", match_call$metric, msg = ") cannot be applied to a dispRity object with no subsets. Use chrono.subsets or custom.subsets to create some.")                
            } else {
                if(data$call$subsets[[1]] == "customised") {
                    ## Make default pairwise comparisons
                    list_of_pairs <- unlist(apply(combn(1:length(data$subsets), 2), 2, list), recursive = FALSE)
                } else {
                    ## Make default sequential comparisons
                    list_of_pairs <- unlist(apply(set.sequence(length(data$subsets)), 2, list), recursive = FALSE)
                }
                is_between.groups <- TRUE
            }
        } 
    } else {
        if(!any(metric_is_between.groups)) {
            stop.call(msg.pre = "The provided metric (", match_call$metric, msg = ") cannot be applied between groups. \"between.groups\" metrics must have at least \"matrix\" and \"matrix2\" as inputs.")
        }
        ## Serial is a list, check if it contains the right information (pairs of things that exist)
        pairs <- unique(unlist(lapply(between.groups, length))) 
        if(length(pairs) > 1 || pairs != 2 || max(unlist(between.groups)) > length(data$subsets)) {
            stop("The provided list of groups (between.groups) must be a list of pairs of subsets in the data.")
        }
        list_of_pairs <- between.groups
        is_between.groups <- TRUE
    }

    ## Not implemented level 3 + level 2/1 metric
    if(is_between.groups && any(metric_is_between.groups) && !is.null(metrics_list$level3.fun) && !metric_is_between.groups[1]) {
        ## Stop.call
        stop(paste0("Impossible to apply a dimension-level 3 metric that is not a between group metric with a dimension-level1 or 2 metric that is. You can try to integrate that dimension-level 3 metric directly in the definition of the other metrics."), call. = FALSE)
    }


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
        ## Lapply through the disparity scores (serialed)
        lapply_loop <- data$disparity

        ## No removed elements
        removed_elements <- FALSE
    }

    ## Select the elements if probabilities are used
    if(has_probabilities && ncol(data$subsets[[1]]$elements) > 1 && matrix_decomposition) {
        ## Sample the elements
        # lapply_loop <- lapply(lapply_loop, function(X) return(list("elements" = elements.sampler(X$elements))))
        selected_elements <- lapply(lapply_loop, function(X) elements.sampler(X$elements))

        ## Reorder them in the right format
        for(subset in 1:length(selected_elements)) {
            lapply_loop[[subset]]$elements <- matrix(selected_elements[[subset]], ncol = ncol(data$subsets[[subset]]$elements)/3)
        }
    }
    
    ## Check if the data is bound
    is_bound <- ifelse(!is.null(data$call$subsets) && data$call$subsets[[1]] == "continuous", as.logical(data$call$subsets[["bind"]]), FALSE)

    ## Make the lapply loop into between.groups loops
    if(is_between.groups) {
        ## Combine the pairs of elements/bs/rare into a lapply loop containing the data for each pair
        lapply_loop <- lapply(list_of_pairs, combine.pairs, lapply_data = lapply_loop)
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

    ## Running the multiple matrix mode
    # if(is_bound || length(data$matrix) > 1) {
    if(is_bound || (length(data$matrix) > 1 && matrix_decomposition && is.null(data$call$subsets["trees"]))) {

        ## Get the number of treesdata
        n_trees <- ifelse(is.null(data$call$subsets["trees"]), 1, as.numeric(data$call$subsets["trees"]))

        ## Make the lapply loops
        lapply_loops <- split.lapply_loop(lapply_loop, n_trees)

        ## Make the matrix list
        matrices_data <- split.data(data)

        ## mapply this
        disparities <- mapply(mapply.wrapper, lapply_loops, matrices_data, 
                            MoreArgs = list(metrics_list, matrix_decomposition, verbose, ...),
                            SIMPLIFY = FALSE)
        # disparities <- mapply(mapply.wrapper, lapply_loops, matrices_data, MoreArgs = list(metrics_list, matrix_decomposition, verbose), SIMPLIFY = FALSE) ; warning("DEBUG dispRity")
        
        ## Reformat to normal disparity object
        disparity <- unlist(lapply(as.list(1:ifelse(is.null(data$call$subsets["trees"]), n_trees, length(disparities[[1]]))),
                                  function(X, disp) recursive.merge(lapply(disp, `[[`, X)), disparities),
                            recursive = FALSE)
        names(disparity) <- names(disparities[[1]])
    } else {
        ## Normal disparity lapply
        disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, metric_is_between.groups, ...)

    }

    # }
    if(verbose) message("Done.\n", appendLF = FALSE)

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

    ## Rename the disparity groups
    if(is_between.groups) {
        ## Remove ":" for pairs of names (: is reserved for the function)
        subset_names <- names(data$subsets)
        if(length(to_correct <- grep(":", subset_names)) > 0) {
            warning(paste0("The subset name", ifelse(length(to_correct) > 1, "s", ""), ": ", paste(subset_names[to_correct], collapse = ", "), ifelse(length(to_correct) > 1, " were ", " was "), "changed to ", paste(gsub(":", ";", subset_names)[to_correct], collapse = ", "), ". The \":\" character is reserved for between groups comparisons."))
            subset_names <- paste(gsub(":", ";", subset_names))
        }
        names(disparity) <- unlist(lapply(list_of_pairs, function(pair, names) paste0(names[pair], collapse = ":"), names = subset_names))
    }

    ## Update the disparity
    data$disparity <- disparity

    ## Update the call
    data$call$disparity$metrics$name <- c(data$call$disparity$metrics$name, match_call$metric)
    if(!is.null(data$call$disparity$metrics$fun)) {
        data$call$disparity$metrics$fun <- list(unlist(data$call$disparity$metrics$fun, recursive = FALSE), metric)
    } else {
        data$call$disparity$metrics$fun <- metric
    }

    ## Adding the between groups
    data$call$disparity$metrics$between.groups <- ifelse(is_between.groups, TRUE, FALSE)

    if(!is.null(data$call$disparity$metrics$args)) {
        if(length(dots) != 0) {
            data$call$disparity$metrics$args <- list(unlist(data$call$disparity$metrics$args, recursive = FALSE), dots)
        }
    } else {
        if(length(dots) != 0) {
            data$call$disparity$metrics$args <- dots
        }
    }

    return(data)
}

