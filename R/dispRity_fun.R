get.dispRity.metric.handle <- function(metric, match_call, data.dim, tree = NULL, ...) {
    level3.fun <- level2.fun <- level1.fun <- NULL
    tree.metrics <- between.groups <- rep(FALSE, 3)
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
        test_level <- make.metric(metric, silent = TRUE, check.between.groups = TRUE, data.dim = data.dim, tree = tree, ...)
        # warning("DEBUG dispRity_fun") ; test_level <- make.metric(metric, silent = TRUE, check.between.groups = TRUE, data.dim = data.dim)
        level <- test_level$type
        between.groups[as.numeric(gsub("level", "", test_level$type))] <- test_level$between.groups
        tree.metrics[as.numeric(gsub("level", "", test_level$type))] <- test_level$tree

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
        test_level <- lapply(metric, make.metric, silent = TRUE, data.dim = data.dim, check.between.groups = TRUE, tree = tree)
        levels <- unlist(lapply(test_level, `[[` , 1))
        btw_groups <- unlist(lapply(test_level, `[[` , 2))
        tree_metrics <- unlist(lapply(test_level, `[[` , 3))

        ## can only unique levels
        if(length(levels) != length(unique(levels))) stop("Some functions in metric are of the same dimension-level.\nTry combining them in a single function.\nFor more information, see:\n?make.metric()", call. = FALSE)

        ## At least one level 1 or level 2 metric is required
        if(length(levels) == 1 && levels[[1]] == "level3") {
            stop("At least one metric must be dimension-level 1 or dimension-level 2\n.For more information, see:\n?make.metric()", call. = FALSE)
        }
        
        ## Get the level 1 metric
        if(!is.na(match("level1", levels))) {
            level1.fun <- metric[[match("level1", levels)]]
            between.groups[1] <- btw_groups[match("level1", levels)]
            tree.metrics[1] <- tree_metrics[match("level1", levels)]
        }

        ## Get the level 2 metric
        if(!is.na(match("level2", levels))) {
            level2.fun <- metric[[match("level2", levels)]]
            between.groups[2] <- btw_groups[match("level2", levels)]
            tree.metrics[2] <- tree_metrics[match("level2", levels)]
        }

        ## Get the level 3 metric
        if(!is.na(match("level3", levels))) {
            level3.fun <- metric[[match("level3", levels)]]
            between.groups[3] <- btw_groups[match("level3", levels)]
            tree.metrics[3] <- tree_metrics[match("level3", levels)]
        }
    }

    return(list(levels = list("level3.fun" = level3.fun, "level2.fun" = level2.fun, "level1.fun" = level1.fun), between.groups = between.groups, tree.metrics = tree.metrics))
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


## Applying the function to one matrix (or two if nrow is not null)
decompose <- function(one_matrix, bootstrap, dimensions, fun, nrow, ...) {
    if(is.null(nrow)) {
        ## Normal decompose
        return(fun(one_matrix[bootstrap, dimensions, drop = FALSE], ...))
    } else {
        ## Serial decompose
        return(
            fun(matrix  = one_matrix[bootstrap[1:nrow], dimensions, drop = FALSE],
                matrix2 = one_matrix[bootstrap[-c(1:nrow)], dimensions, drop = FALSE],
                ...)
            )
    }
}
## Same as decompose but including the tree argument
decompose.tree <- function(one_matrix, one_tree, bootstrap, dimensions, fun, nrow, ...) {
    if(is.null(nrow)) {
        ## Normal decompose
        return(fun(one_matrix[bootstrap, dimensions, drop = FALSE], tree = one_tree, ...))
    } else {
        ## Serial decompose
        return(
            fun(matrix  = one_matrix[bootstrap[1:nrow], dimensions, drop = FALSE],
                matrix2 = one_matrix[bootstrap[-c(1:nrow)], dimensions, drop = FALSE],
                tree    = one_tree, ...)
            )
    }
}

## Calculates disparity from a bootstrap table
decompose.matrix <- function(one_subsets_bootstrap, fun, data, nrow, use_tree, ...) {

    ## Return NA if no data
    if(length(na.omit(one_subsets_bootstrap)) < 2) {
        return(NA)
    }

    ## Some compactify/decompactify thingy can happen here for a future version of the package where lapply(data$matrix, ...) can be lapply(decompact(data$matrix), ...)

    if(!use_tree) {
        ## Apply the fun, bootstrap and dimension on each matrix
        return(unlist(lapply(data$matrix, decompose,
                            bootstrap  = na.omit(one_subsets_bootstrap),
                            dimensions = 1:data$call$dimensions,
                            fun        = fun,
                            nrow       = nrow,
                            ...),
                      recursive = FALSE))
    } else {
        ## Check whether the number of trees and matrices match
        ## PLACEHOLDER for decompact(matrix)
        matrices <- data$matrix
        trees    <- data$tree
        if((n_matrices <- length(matrices)) != (n_trees <- length(trees))) {
            ## Match the trees and the matrices by multiplying them
            matrices <- unlist(replicate(n_trees, matrices, simplify = FALSE), recursive = FALSE)
            trees <- unlist(replicate(n_matrices, trees, simplify = FALSE), recursive = FALSE)
        }
        return(unlist(mapply(decompose.tree, matrices, trees,
                   MoreArgs = list(bootstrap  = na.omit(one_subsets_bootstrap),
                                   dimensions = 1:data$call$dimensions,
                                   fun        = fun,
                                   nrow       = nrow,
                                   ...),
                   SIMPLIFY = FALSE),
               recursive = FALSE))

    }
}

## Apply decompose matrix
decompose.matrix.wrapper <- function(one_subsets_bootstrap, fun, data, use_array, use_tree = FALSE, ...) {
   
    if(is(one_subsets_bootstrap)[[1]] == "list") {
        ## Isolating the matrix into it's two components if the "matrix" is actually a list
        nrow <- one_subsets_bootstrap$nrow[1]
        one_subsets_bootstrap <- one_subsets_bootstrap$data
    } else {
        nrow <- NULL
    }

    ## Decomposing the matrix
    if(use_array) {
        return(array(apply(one_subsets_bootstrap, 2, decompose.matrix, fun = fun, data = data, nrow = nrow, use_tree = use_tree, ...), dim = c(data$call$dimensions, data$call$dimensions, ncol(one_subsets_bootstrap))))
 
    } else {

        ## one_subsets_bootstrap is a list (in example) on a single matrix
        results_out <- apply(one_subsets_bootstrap, 2, decompose.matrix, fun = fun, data = data, nrow = nrow, use_tree = use_tree, ...)

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
disparity.bootstraps <- function(one_subsets_bootstrap, metrics_list, data, matrix_decomposition, metric_has_tree = rep(FALSE, length(metrics_list)), ...){# verbose, ...) {
    ## 1 - Decomposing the matrix (if necessary)
    verbose_place_holder <- NULL
    if(matrix_decomposition) {
        ## Find out whether to output an array
        use_array <- !is.null(metrics_list$level3.fun)
        ## Getting the first metric
        first_metric <- get.first.metric(metrics_list)
        metrics_list <- first_metric[[2]]
        use_tree     <- metric_has_tree[first_metric[[3]]]
        first_metric <- first_metric[[1]]

        ## Decompose the metric using the first metric
        disparity_out <- decompose.matrix.wrapper(one_subsets_bootstrap, fun = first_metric, data = data, use_array = use_array, use_tree = use_tree, ...)
    } else {
        disparity_out <- one_subsets_bootstrap
    }

    #TODO: handle tree metrics after the matrix decomposition

    ## 2 - Applying the metrics to the decomposed matrix
    if(!is.null(metrics_list$level3.fun)) {
        if(!metric_has_tree[3]) {
            disparity_out <- apply(disparity_out, 2, metrics_list$level3.fun, ...)
        } else {
            disparity_out <- apply(disparity_out, 2, metrics_list$level3.fun, tree = data$tree, ...)
        }
    }

    if(!is.null(metrics_list$level2.fun)) {
        if(!metric_has_tree[2]) {
            disparity_out <- apply(disparity_out, 3, metrics_list$level2.fun, ...)
        } else {
            disparity_out <- apply(disparity_out, 3, metrics_list$level2.fun, tree = data$tree, ...)
        }
    }

    if(!is.null(metrics_list$level1.fun)) {
        if(!metric_has_tree[1]) {
            disparity_out <- apply(disparity_out, MARGIN = length(dim(disparity_out)), metrics_list$level1.fun, ...)
        } else {
            disparity_out <- apply(disparity_out, MARGIN = length(dim(disparity_out)), metrics_list$level1.fun, tree = data$tree, ...)
        }
        disparity_out <- t(as.matrix(disparity_out))
    }

    return(disparity_out)
}


## Lapply wrapper for disparity.bootstraps function
lapply.wrapper <- function(subsets, metrics_list, data, matrix_decomposition, verbose, metric_has_tree = rep(FALSE, length(metrics_list)), ...) {
    if(verbose) {
        ## Making the verbose version of disparity.bootstraps
        body(disparity.bootstraps)[[2]] <- substitute(message(".", appendLF = FALSE))
    }
    return(lapply(subsets, disparity.bootstraps, metrics_list, data, matrix_decomposition, metric_has_tree, ...))
}
mapply.wrapper <- function(lapply_loop, data, metrics_list, matrix_decomposition, verbose, metric_has_tree, ...) {
    return(lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, metric_has_tree, ...))
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
## Merge the data for bound tree/matrices
recursive.merge <- function(list, bind = cbind) {
    while(length(list) > 1) {
        list[[1]] <- mapply(bind, list[[1]], list[[2]], SIMPLIFY = FALSE)
        list[[2]] <- NULL
    }
    return(list)
}

## Combine the pairs of elements/bs/rare into a lapply loop containing the data for each pair
combine.pairs <- function(pairs, lapply_data) {
    ## Making the lists the same lengths
    pair_lengths <- unlist(lapply(lapply_data[pairs], length))
    if(pair_lengths[1] != pair_lengths[2]) {
        if(pair_lengths[1] > pair_lengths[2]) {
            pair_one <- lapply_data[pairs][[1]]
            pair_two <- c(lapply_data[pairs][[2]], rep(lapply_data[pairs][[2]][pair_lengths[2]], abs(diff(pair_lengths))))
        } else {
            pair_two <- lapply_data[pairs][[2]]
            pair_one <- c(lapply_data[pairs][[1]], rep(lapply_data[pairs][[1]][pair_lengths[1]], abs(diff(pair_lengths))))
        }
    } else {
        pair_one <- lapply_data[pairs][[1]]
        pair_two <- lapply_data[pairs][[2]]
    }
    ## Combine both pairs
    combined_pairs <- mapply(rbind, pair_one, pair_two, SIMPLIFY = FALSE)
    nrow_pairs <- mapply(function(x, y) c(nrow(x), nrow(y)), pair_one, pair_two, SIMPLIFY = FALSE)
    ## Add the row number
    return(mapply(function(combined, nrow) return(list("nrow" = nrow, "data" = combined)), combined = combined_pairs, nrow = nrow_pairs, SIMPLIFY = FALSE))
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

