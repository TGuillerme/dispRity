get.dispRity.metric.handle <- function(metric, match_call, ...) {
    level3.fun <- level2.fun <- level1.fun <- NULL

    length_metric <- length(metric)

    ## Get the metric handle
    if(length_metric == 1) {
        if(!is(metric, "list")) {
            ## Metric was fed as a single element
            check.class(metric, "function")
        } else {
            ## Metric was still fed as a list
            check.class(metric[[1]], "function")
            metric <- metric[[1]]
        }
        ## Which level is the metric?
        level <- make.metric(metric, silent = TRUE, ...)
        # warning("DEBUG dispRity_fun") ; level <- make.metric(metric, silent = TRUE)

        switch(level,
            level3 = {
                stop.call(match_call$metric, " metric must contain at least a dimension-level 1 or a dimension-level 2 metric.\nFor more information, see ?make.metric.")
            },
            level2 = {
                level2.fun <- metric
            },
            level1 = {
                level1.fun <- metric
            }
        )
    } else {
        ## Check all the metrics
        for(i in 1:length_metric) {
            if(!is(metric[[i]], "function")) {
                stop.call(msg.pre = "metric argument ", call = match_call$metric[[i + 1]], msg = " is not a function.")
            }
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


## Prefix version of the `[` function with automatic column selector
get.row.col <- function(x, row, col = NULL) {
    `[`(x, row, 1:`if`(is.null(col), ncol(x), col))
}

## Apply decompose matrix
apply.decompose.matrix <- function(one_subsets_bootstrap, fun, data, use_array, ...) {
    ## Calculates disparity from a bootstrap table
    decompose.matrix <- function(one_subsets_bootstrap, fun, data, ...) {

        ## Apply the fun, bootstrap and dimension on each matrix
        return(unlist(lapply(data$matrix,
            function(X, bootstrap, dimensions, fun, ...) fun(X[bootstrap, dimensions], ...),
                bootstrap = na.omit(one_subsets_bootstrap),
                dimensions = 1:data$call$dimensions,
                fun,
                ...), recursive = FALSE))#, use.names = FALSE))

                # return(fun( data$matrix[na.omit(one_subsets_bootstrap), 1:data$call$dimensions], ...))
    }

    ## Decomposing the matrix
    if(use_array) {
        return(array(apply(one_subsets_bootstrap, 2, decompose.matrix, fun = fun, data = data, ...), dim = c(data$call$dimensions, data$call$dimensions, ncol(one_subsets_bootstrap))))
    } else {

        ## one_subsets_bootstrap is a list (in example)
        results_out <- apply(one_subsets_bootstrap, 2, decompose.matrix, fun = fun, data = data, ...)

        ## Return the results
        if(is(results_out, "matrix")) {
            return(results_out)
        } else {
            ## Make the results into a matrix with the same size
            return(do.call(cbind,
                lapply(results_out, function(x, max) {length(x) <- max ; return(x)},
                        max = max(unlist(lapply(results_out, length)))
                        )
                )
            )
        }

        # return(matrix(apply(one_bs_matrix, 2, decompose.matrix, fun = fun, data = data, ...), ncol = ncol(one_bs_matrix)))
    }
}

## Calculating the disparity for a bootstrap matrix 
disparity.bootstraps <- function(one_subsets_bootstrap, metrics_list, data, matrix_decomposition, ...){# verbose, ...) {
    ## 1 - Decomposing the matrix (if necessary)
    verbose_place_holder <- NULL
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
        margin <- length(dim(disparity_out))
        disparity_out <- apply(disparity_out, margin, metrics_list$level1.fun, ...)
        disparity_out <- t(as.matrix(disparity_out))
    }

    return(disparity_out)
}


## Lapply wrapper for disparity.bootstraps function
lapply.wrapper <- function(subsets, metrics_list, data, matrix_decomposition, verbose, ...) {
    if(verbose) {
        ## Making the verbose version of disparity.bootstraps
        body(disparity.bootstraps)[[2]] <- substitute(message(".", appendLF = FALSE))
    }
    return(lapply(subsets, disparity.bootstraps, metrics_list, data, matrix_decomposition, ...))
}
mapply.wrapper <- function(lapply_loop, data, metrics_list, matrix_decomposition, verbose, ...) {
    return(lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, ...))
}




## Split the lapply_loop for bound tree/matrices
split.lapply_loop <- function(lapply_loop, n_trees) {

    split.matrix <- function(matrix, n_trees) {
        ncol_out  <- ncol(matrix)/n_trees
        return(lapply(split(as.vector(matrix), rep(1:n_trees, each = ncol_out * nrow(matrix)) ), matrix, ncol = ncol_out))
    }

    ## Combine them in lapply loops
    return(lapply(as.list(1:n_trees), function(tree, splits) lapply(splits, lapply, `[[`, tree),
            splits = lapply(lapply_loop, lapply, split.matrix, n_trees)))
}

## Split the data for bound tree/matrices
split.data <- function(data) {
    ## Splitting the different matrices
    return(lapply(data$matrix, function(X)
        list("matrix" = list(X),
             "call" = list("dimensions" = data$call$dimensions))))
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
