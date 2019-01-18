#FUNCTIONS FOR DISPRITY
get.dispRity.metric.handle <- function(metric, match_call, ...) {
    level3.fun <- NULL
    level2.fun <- NULL
    level1.fun <- NULL

    length_metric <- length(metric)

    ## Get the metric handle
    if(length_metric == 1) {
        if(class(metric) != "list") {
            ## Metric was fed as a single element
            check.class(metric, "function")
        } else {
            ## Metric was still fed as a list
            check.class(metric[[1]], "function")
            metric <- metric[[1]]
        }
        ## Which level is the metric?
        level <- make.metric(metric, silent = TRUE, ...)
        if(level == "level3") {
            stop(paste(as.expression(match_call$metric), " metric must contain at least a dimension-level 1 or a dimension-level 2 metric.\nFor more information, see ?make.metric.", sep = ""))
        } else {
            level3.fun <- NULL
            if(level == "level2") {
                level2.fun <- metric
                level1.fun <- NULL
            } else {
                level2.fun <- NULL
                level1.fun <- metric
            }
        }
    } else {
        ## Check all the metrics
        for(i in 1:length_metric) {
            if(class(metric[[i]]) != "function") stop(paste("in metric argument: ",match_call$metric[[i+1]], " is not a function!", sep = ""))
        }
        ## Sorting the metrics by levels
        ## getting the metric levels
        levels <- unlist(lapply(metric, make.metric, silent=TRUE))
        ## can only unique levels
        if(length(levels) != length(unique(levels))) stop("Some functions in metric are of the same dimension-level.\nTry combining them in a single function.\nFor more information, see:\n?make.metric()")

        ## At least one level 1 or level 2 metric is required
        if(length(levels) == 1 && levels[[1]] == "level3") {
            stop("At least one metric must be dimension-level 1 or dimension-level 2\n.For more information, see:\n?make.metric()")
        }
        
        ## Get the level 1 metric
        if(!is.na(match("level1", levels))) {
            level1.fun <- metric[[match("level1", levels)]]
        }

        ## Get the level 2 metric
        if(!is.na(match("level2", levels))) {
            level2.fun <- metric[[match("level2", levels)]]
        }

        ## Get the level 3 metric
        if(!is.na(match("level3", levels))) {
            level3.fun <- metric[[match("level3", levels)]]
        }
    }

    return(list("level3.fun" = level3.fun, "level2.fun" = level2.fun, "level1.fun" = level1.fun))
}


## Combining the disparity results with the elements
# combine.disparity <- function(one_disparity_subsets, one_bootstrap_subsets) {
#     return(c(one_bootstrap_subsets[1], one_disparity_subsets))
# }

## Getting the first metric
get.first.metric <- function(metrics_list_tmp) {
    ## Initialise
    metric <- 1
    counter <- 1

    ## Select the first metric
    while(is.null(metrics_list_tmp[[metric]]) & counter < 3) {
        metric <- metric + 1
        counter <- counter + 1
    }

    ## output
    metric_out <- metrics_list_tmp[[metric]]
    metrics_list_tmp[metric] <- list(NULL)
    return(list(metric_out, metrics_list_tmp, metric))
}

# ## Generates the output vector for the decomposition function
# generate.empty.output <- function(one_subsets_bootstrap, data, level) {
#     if(level == 3) {
#         ## Return an array
#         return(array(data = numeric(1), dim = c(data$call$dimensions, data$call$dimensions, ncol(one_subsets_bootstrap))))
#     }
#     if(level == 2) {
#         ## Return a matrix
#         return(matrix(numeric(1), nrow = data$call$dimensions, ncol = ncol(one_subsets_bootstrap)))
#     }
#     if(level == 1) {
#         ## Return a vector
#         return(numeric(ncol(one_subsets_bootstrap)))
#     }
# }

## Apply decompose matrix
apply.decompose.matrix <- function(one_bs_matrix, fun, data, use_array, ...) {
    ## Calculates disparity from a bootstrap table
    decompose.matrix <- function(one_bootstrap, fun, data, ...) {
        return(fun( data$matrix[one_bootstrap, 1:data$call$dimensions], ...))
    }

    ## Decomposing the matrix
    if(use_array) {
        return(array(apply(one_bs_matrix, 2, decompose.matrix, fun = fun, data = data, ...), dim = c(data$call$dimensions, data$call$dimensions, ncol(one_bs_matrix))))
    } else {
        return(apply(one_bs_matrix, 2, decompose.matrix, fun = fun, data = data, ...))
    }
}


## Calculating the disparity for a bootstrap matrix 
disparity.bootstraps.silent <- function(one_subsets_bootstrap, metrics_list, data, matrix_decomposition, ...){# verbose, ...) {
    ## 1 - Decomposing the matrix (if necessary)
    if(matrix_decomposition) {
        ## Find out whether to output an array
        use_array <- !is.null(metrics_list$level3.fun)
        ## Getting the first metric
        first_metric <- get.first.metric(metrics_list)
        level <- first_metric[[3]]
        metrics_list <- first_metric[[2]]
        first_metric <- first_metric[[1]]
        ## Decompose the metric using the first metric
        disparity_out <- apply.decompose.matrix(one_subsets_bootstrap, fun = first_metric, data = data, use_array = use_array, ...)
    } else {
        disparity_out <- one_subsets_bootstrap
    }

    ## 2 - Applying the metrics to the decomposed matrix
    if(!is.null(metrics_list$level3.fun)) {
        disparity_out <- apply(disparity_out, 2, metrics_list$level3.fun, ...)
    }

    if(!is.null(metrics_list$level2.fun)) {
        disparity_out <- apply(disparity_out, 3, metrics_list$level2.fun, ...)
    }

    if(!is.null(metrics_list$level1.fun)) {
        margin <- ifelse(class(disparity_out) != "array", 2, 3)
        disparity_out <- apply(disparity_out, margin, metrics_list$level1.fun, ...)
        disparity_out <- t(as.matrix(disparity_out))
    }

    return(disparity_out)
}


disparity.bootstraps.verbose <- function(one_subsets_bootstrap, metrics_list, data, matrix_decomposition, ...){# verbose, ...) {
    message(".", appendLF = FALSE)
    ## 1 - Decomposing the matrix (if necessary)
    if(matrix_decomposition) {
        ## Find out whether to output an array
        use_array <- !is.null(metrics_list$level3.fun)
        ## Getting the first metric
        first_metric <- get.first.metric(metrics_list)
        level <- first_metric[[3]]
        metrics_list <- first_metric[[2]]
        first_metric <- first_metric[[1]]
        ## Decompose the metric using the first metric
        disparity_out <- apply.decompose.matrix(one_subsets_bootstrap, fun = first_metric, data = data, use_array = use_array, ...)
    } else {
        disparity_out <- one_subsets_bootstrap
    }

    ## 2 - Applying the metrics to the decomposed matrix
    if(!is.null(metrics_list$level3.fun)) {
        disparity_out <- apply(disparity_out, 2, metrics_list$level3.fun, ...)
    }

    if(!is.null(metrics_list$level2.fun)) {
        disparity_out <- apply(disparity_out, 3, metrics_list$level2.fun, ...)
    }

    if(!is.null(metrics_list$level1.fun)) {
        margin <- ifelse(class(disparity_out) != "array", 2, 3)
        disparity_out <- apply(disparity_out, margin, metrics_list$level1.fun, ...)
        disparity_out <- t(as.matrix(disparity_out))
    }

    return(disparity_out)
}


# ## Lapply wrapper for disparity.bootstraps function
lapply.wrapper <- function(subsets, metrics_list, data, matrix_decomposition, verbose, ...) {
    if(verbose) {
        disparity.bootstraps <- disparity.bootstraps.verbose
    } else {
        disparity.bootstraps <- disparity.bootstraps.silent
    }

    return(lapply(subsets, disparity.bootstraps, metrics_list, data, matrix_decomposition, ...))
}


# ## Parallel versions
# parLapply.wrapper <- function(i, cluster) {
#     ## Running the parallel apply
#     parallel::parLapply(cluster, i, 
#         function(j) {
#             disparity.bootstraps.silent(j, metrics_list, data, matrix_decomposition)#, additional_args)
#         }
#     )
# }
