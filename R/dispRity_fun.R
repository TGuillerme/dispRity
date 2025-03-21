#####################
##
## BEFORE the lapply_loop
##
#####################

## Check covar metric
check.covar <- function(metric, data) {
    ## Check whether the metric is a covar one
    is_covar <- eval.covar(metric, null.return = FALSE)
    if(is_covar) {
        ## Check if data has a covar component
        if(is.null(data$covar)) {
            stop.call(msg = "Impossible to use a metric with as.covar() if the data has no $covar component.\nCheck MCMCglmm.subsets() function.", call = "")
        } else {
            dim_out <- rep(length(data$call$dimensions), 2)
        }
    } else {
        dim_out <- dim(data$matrix[[1]])
        ## Check if there is a smaller dataset available
        # if(!is.null(data$subsets)) {
        #     dim_out[1] <- max(size.subsets(data))
        # }
        if(!is.null(data$call$dimensions)) {
            dim_out[2] <- length(data$call$dimensions)
        }
    }

    return(list(is_covar = is_covar, data.dim = dim_out))
}

## Checks the levels and extras for one metric
check.one.metric <- function(metric, data, tree, ...) {
    
    dots <- list(...)
    ## Check the class
    check.class(metric, c("function", "standardGeneric"), report = 1)

    ## Run the checks
    checks <- check.covar(metric, data)
    get_help <- check.get.help(metric)
    if(!is.null(names(dots)) && ("dist.helper" %in% names(dots))) {
        get_help <- TRUE
    }
    data_dim <- if(get_help) {data} else {checks$data.dim}

    return(make.metric(metric, silent = TRUE, check.between.groups = TRUE, data.dim = data_dim, tree = tree, covar = checks$is_covar, get.help = get_help, ...))
    # warning("DEBUG: dispRity_fun.R::check.one.metric"); test <- make.metric(metric, silent = TRUE, check.between.groups = TRUE, data.dim = data_dim, tree = tree, covar = checks$is_covar, get.help = get_help, dist.helper = dist.helper)
}

## Handle the disparity metrics
get.dispRity.metric.handle <- function(metric, match_call, data = list(matrix = list(matrix(NA, 5, 4))), tree = NULL, ...) {

    dist_help <- level3.fun <- level2.fun <- level1.fun <- reduce.dist.lvl3 <- reduce.dist.lvl2 <- reduce.dist.lvl1 <- NULL
    tree.metrics <- between.groups <- rep(FALSE, 3)
    length_metric <- length(metric)

    ## Check the metrics
    if(!is(metric, "list")) {
        metric <- list(metric)
    }
    dots <- list(...)

    ## Check if there are two metric + RAM helper
    if("dist.helper" %in% names(dots) && length(metric) > 1) {
        stop(paste0("dist.help can only be used for one metric. You can try combine the ", length(metric), " metrics together into one or calculate disparity step by step. For example:\ndispRity(dispRity(data, metric = level2.metric), metric = level1.metric)"), call. = FALSE)
    }

    ## Check all metrics
    metric_checks <- lapply(metric, check.one.metric, data, tree, ...)    
    # warning("DEBUG: dispRity_fun.R::get.dispRity.metric.handle") ; metric_checks <- lapply(metric, check.one.metric, data, tree, dist.helper = dist.helper)

    ## Sort out the tests
    levels       <- unlist(lapply(metric_checks, `[[` , "type"))
    btw_groups   <- unlist(lapply(metric_checks, `[[` , "between.groups"))
    tree_metrics <- unlist(lapply(metric_checks, `[[` , "tree"))
    dist_help    <- unlist(lapply(metric_checks, `[[` , "dist.help"), recursive = FALSE)
    reduce_dist  <- unlist(lapply(metric_checks, `[[` , "reduce.dist"))
    remove(metric_checks)

    ## can only unique levels
    if(length(levels) != length(unique(levels))) stop("Some functions in metric are of the same dimension-level.\nTry combining them in a single function.\nFor more information, see:\n?make.metric()", call. = FALSE)
    ## At least one level 1 or level 2 metric is required
    if(length(levels) == 1 && levels[[1]] == "level3") {
        stop("At least one metric must be dimension-level 1 or dimension-level 2\n.For more information, see:\n?make.metric()", call. = FALSE)
    }

    ## Sort the levels
    if(!is.na(match("level1", levels))) {
        level1.fun        <- metric[[match("level1", levels)]]
        between.groups[1] <- btw_groups[match("level1", levels)]
        tree.metrics[1]   <- tree_metrics[match("level1", levels)]
        reduce.dist.lvl1  <- reduce_dist[match("level1", levels)]
        if(!is.null(reduce.dist.lvl1)) {
            reduce.dist.lvl1 <- match_call$dist.helper
        }
    }
    if(!is.na(match("level2", levels))) {
        level2.fun        <- metric[[match("level2", levels)]]
        between.groups[2] <- btw_groups[match("level2", levels)]
        tree.metrics[2]   <- tree_metrics[match("level2", levels)]
        reduce.dist.lvl2  <- reduce_dist[match("level2", levels)]
        if(!is.null(reduce.dist.lvl2)) {
            reduce.dist.lvl2 <- match_call$dist.helper
        }
    }
    if(!is.na(match("level3", levels))) {
        level3.fun        <- metric[[match("level3", levels)]]
        between.groups[3] <- btw_groups[match("level3", levels)]
        tree.metrics[3]   <- tree_metrics[match("level3", levels)]
        reduce.dist.lvl3  <- reduce_dist[match("level3", levels)]
        if(!is.null(reduce.dist.lvl3)) {
            reduce.dist.lvl3 <- match_call$dist.helper
        }
    }

    ## Evaluate the covarness
    covar_check <- unlist(lapply(list(level1.fun, level2.fun, level3.fun), eval.covar))
    if(any(covar_check)) {
        if(sum(covar_check) > 1) {
            ## Stop if there are more than one covar meetirc
            stop.call(msg = "Only one metric can be set as as.covar().", call = "")
        } else {
            if(!covar_check[length(covar_check)]) {
                ## Stop if the last dimension-level metric is not the covar one
                stop.call(msg = "Only the highest dimension-level metric can be set as as.covar().", call = "")
            }
        }
    }

    return(list(levels = list("level3.fun" = reduce.checks(level3.fun, reduce.dist = reduce.dist.lvl3), "level2.fun" = reduce.checks(level2.fun, reduce.dist = reduce.dist.lvl2), "level1.fun" = reduce.checks(level1.fun, reduce.dist = reduce.dist.lvl1)), between.groups = rev(between.groups), tree.metrics = rev(tree.metrics), dist.help = dist_help))
}

## Function to reduce the checks (distance matrix input is already handled)
reduce.checks <- function(fun, reduce.dist = NULL) {

    ## Do nothing
    if(is.null(fun)) {
        return(NULL)
    }

    ## Reduce distance checks
    if(!is.null(reduce.dist)) {

        ## If reduce.dist is logical (TRUE) change the function to "check.dist.matrix"
        if(is.logical(reduce.dist)) {
            to_reduce <- "check.dist.matrix"
        } else {
            ## reduce.dist is a function
            if(grepl("check.dist.matrix", paste(as.character(body(fun)), collapse = ""))) {
                ## The function contains check.dist.matrix and is internal to dispRity
                to_reduce <- "check.dist.matrix"
            } else {
                ## The function is external
                to_reduce <- as.character(reduce.dist)
                if(length(to_reduce) > 1) {
                    ## has the package name description
                    to_reduce <- paste0(to_reduce[2], to_reduce[1], to_reduce[3], collapse = "")
                }
                to_reduce <- paste0(to_reduce, "\\(")
            }
        }

        ## Reduce the function
        if(length(check_line <- grep(to_reduce, body(fun))) > 0) {
            ## Remove them!
            for(one_check in check_line) {
                if(is(body(fun)[[one_check]], "<-") || is(body(fun)[[one_check]], "call")) {
                    ## Substitute the line (keeping the variable name)
                    body(fun)[[one_check]] <- substitute(var_name <- matrix.to.dist(matrix), list(var_name = as.character(body(fun)[[one_check]])[2]))
                } else {
                    ## recursively dig in the loop
                    inner_line <- grep(to_reduce, as.character(body(fun)[[one_check]]))
                    if(is(body(fun)[[one_check]][[inner_line]], " <-") || is(body(fun)[[one_check]], "call")) {
                        # body(fun)[[one_check]][[inner_line]] <- substitute(distances <- matrix.to.dist(matrix))
                        body(fun)[[one_check]][[inner_line]] <- substitute(var_name <- matrix.to.dist(matrix), list(var_name = as.character(body(fun)[[one_check]][[inner_line]])[2]))
                    } else {
                        inner_line2 <- grep(to_reduce, as.character(body(fun)[[one_check]][[inner_line]]))
                        # body(fun)[[one_check]][[inner_line]][[inner_line2]] <- substitute(distances <- matrix.to.dist(matrix))
                        body(fun)[[one_check]][[inner_line]][[inner_line2]] <- substitute(var_name <- matrix.to.dist(matrix), list(var_name = as.character(body(fun)[[one_check]][[inner_line]][[inner_line2]])[2]))
                    }
                }
            }
        }
    }

    ## Reduce method check
    if(length(check_line <- grep("check.method", body(fun))) > 0) {
        ## Remove them!
         for(one_check in check_line) {
            if(is(body(fun)[[one_check]], "<-") || is(body(fun)[[one_check]], "call")) {
                ## Substitute the line
                body(fun)[[one_check]] <- substitute(no_check <- NULL)
            } else {
                ## recursively dig in the loop
                inner_line <- grep("check.method", as.character(body(fun)[[one_check]]))
                if(is(body(fun)[[one_check]][[inner_line]], " <-") || is(body(fun)[[one_check]][[inner_line]], "call")) {
                    body(fun)[[one_check]][[inner_line]] <- substitute(no_check <- NULL)
                } else {
                    inner_line2 <- grep("check.method", as.character(body(fun)[[one_check]][[inner_line]]))
                    body(fun)[[one_check]][[inner_line]][[inner_line2]] <- substitute(no_check <- NULL)
                }
            }
        }
    }

    ## Reduce class check
    if(length(check_line <- grep("check.class", body(fun))) > 0) {
        ## Remove them!
         for(one_check in check_line) {
            if(is(body(fun)[[one_check]], "<-") || is(body(fun)[[one_check]], "call")) {
                ## Substitute the line
                body(fun)[[one_check]] <- substitute(no_check <- NULL)
            } else {
                ## recursively dig in the loop
                inner_line <- grep("check.class", as.character(body(fun)[[one_check]]))
                if(is(body(fun)[[one_check]][[inner_line]], " <-") || is(body(fun)[[one_check]][[inner_line]], "call")) {
                    body(fun)[[one_check]][[inner_line]] <- substitute(no_check <- NULL)
                } else {
                    inner_line2 <- grep("check.class", as.character(body(fun)[[one_check]][[inner_line]]))
                    body(fun)[[one_check]][[inner_line]][[inner_line2]] <- substitute(no_check <- NULL)
                }
            }
        }
    }

    ## Reduce length check
    if(length(check_line <- grep("check.length", body(fun))) > 0) {
        ## Remove them!
         for(one_check in check_line) {
            if(is(body(fun)[[one_check]], "<-") || is(body(fun)[[one_check]], "call")) {
                ## Substitute the line
                body(fun)[[one_check]] <- substitute(no_check <- NULL)
            } else {
                ## recursively dig in the loop
                inner_line <- grep("check.length", as.character(body(fun)[[one_check]]))
                if(is(body(fun)[[one_check]][[inner_line]], " <-") || is(body(fun)[[one_check]][[inner_line]], "call")) {
                    body(fun)[[one_check]][[inner_line]] <- substitute(no_check <- NULL)
                } else {
                    inner_line2 <- grep("check.length", as.character(body(fun)[[one_check]][[inner_line]]))
                    body(fun)[[one_check]][[inner_line]][[inner_line2]] <- substitute(no_check <- NULL)
                }
            }
        }
    }
    return(fun)
}


#####################
##
## INSIDE the lapply_loop
##
#####################


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


## Decompositions
single.decompose <- function(matrix, bs_rows, bs_cols, fun, ...) {
    return(fun(matrix[bs_rows, bs_cols, drop = FALSE], ...))
}
double.decompose <- function(matrix, bs_rows, bs_cols, fun, nrow, is.dist = FALSE, ...) {

    ## Get the columns to return
    select_cols1 <- select_cols2 <- bs_cols
    if(is.dist) {
        select_cols1 <- bs_cols[1:nrow]
        select_cols2 <- bs_cols[-c(1:nrow)]
    } 

    ## Return the fun
    return(
        fun(matrix  = matrix[bs_rows[1:nrow],     select_cols1, drop = FALSE],
            matrix2 = matrix[bs_rows[-c(1:nrow)], select_cols2, drop = FALSE],
            ...)
        )
}

## Applying the function to one matrix (or two if nrow is not null)
# one_matrix <- data$matrix[[1]] ; warning("DEBUG: dispRity_fun")
# bootstrap <- na.omit(one_subsets_bootstrap) ; warning("DEBUG: dispRity_fun")
# fun <- first_metric ; warning("DEBUG: dispRity_fun")
# dimensions <- data$call$dimensions ; warning("DEBUG: dispRity_fun")
decompose.base <- function(one_matrix, bootstrap, dimensions, fun, nrow, ...) {

    ## Select the variables
    bs_rows <- bootstrap
    bs_cols <- dimensions
    matrix  <- one_matrix

    if(is.null(nrow)) {
        ## Normal decompose
        return(single.decompose(matrix, bs_rows, bs_cols, fun, ...))
    } else {
        ## Serial decompose
        return(double.decompose(matrix, bs_rows, bs_cols, fun, nrow, is.dist = FALSE, ...))
    }
}

## Same as decompose but including the tree argument
# one_matrix <- matrices[[1]] ; warning("DEBUG: dispRity_fun")
# one_tree <- trees[[1]] ; warning("DEBUG: dispRity_fun")
# bootstrap <- na.omit(one_subsets_bootstrap) ; warning("DEBUG: dispRity_fun")
# fun <- first_metric ; warning("DEBUG: dispRity_fun")
# dimensions <- data$call$dimensions ; warning("DEBUG: dispRity_fun")
decompose.tree <- function(one_matrix, one_tree, bootstrap, dimensions, fun, nrow, dist_help = NULL, dist.data = FALSE, by.col = NULL, ...) {

    ## Select the variables
    bs_rows <- bootstrap
    bs_cols <- dimensions
    matrix  <- as.matrix(one_matrix)

    ## Check if fun has a "reference.data" argument
    if(!("reference.data" %in% formalArgs(fun))) {
        ##Does not use reference.data
        if(is.null(nrow)) {
            ## Normal decompose
            return(single.decompose(matrix, bs_rows, bs_cols, fun, tree = one_tree, ...))
        } else {
            ## Serial decompose
            return(double.decompose(matrix, bs_rows, bs_cols, fun, nrow, is.dist = FALSE, tree = one_tree, ...))
        }
    } else {
        ## Uses reference.data
        if(is.null(nrow)) {
            ## Normal decompose
            return(single.decompose(matrix, bs_rows, bs_cols, fun, tree = one_tree, reference.data = one_matrix, ...))
        } else {
            ## Serial decompose
            return(double.decompose(matrix, bs_rows, bs_cols, fun, nrow, is.dist = FALSE, tree = one_tree, reference.data = one_matrix, ...))
        }
    }
}

## Calculates disparity from a bootstrap table
# fun <- first_metric ; warning("DEBUG: dispRity_fun")
decompose.matrix <- function(one_subsets_bootstrap, fun, data, nrow, use_tree, dist_help = NULL, dist.data = FALSE, by.col = NULL, ...) {

    ## Return NA if no data
    if(length(na.omit(one_subsets_bootstrap)) < 2) {
        return(NA)
    }

    ## Some compactify/decompactify thingy can happen here for a future version of the package where lapply(data$matrix, ...) can be lapply(decompact(data$matrix), ...)

    ## Select the data
    if(!is.null(dist_help)) {
        ## RAM help setup (assuming distance matrices)
        data_list  <- dist_help
        ## Toggle dist.data
        dist.data <- TRUE
    } else {
        data_list  <- data$matrix
    
    }
    ## Select the dimensions
    if(dist.data) {
        bootstrap  <- dimensions <- na.omit(one_subsets_bootstrap)
    } else {
        if(!is.null(by.col)) {
            ## Dimensions is bootstrap if not elements.
            dimensions <- na.omit(one_subsets_bootstrap)
            bootstrap <- na.omit(by.col) 
        } else {
            ## Base bootstrap use
            dimensions <- data$call$dimensions
            bootstrap  <- na.omit(one_subsets_bootstrap)
        }
    }

    if(!use_tree) {
        ## Apply the fun, bootstrap and dimension on each matrix
        return(unlist(lapply(data_list, decompose.base,
                            bootstrap  = bootstrap,
                            dimensions = dimensions,
                            fun        = fun,
                            nrow       = nrow,
                            ...),
                      recursive = FALSE))

        #TG: when multiple matrices and dimensions level2 this should return an array?
        # decompose.base(data_list[[1]], bootstrap = bootstrap, dimensions = dimensions, fun = fun, nrow = nrow)

    } else {
        ## Check whether the number of trees and matrices match
        ## Applying the decomposition to all trees and all matrices
        return(do.call(cbind,
            mapply(decompose.tree, data_list, data$tree,
                    MoreArgs = list(bootstrap  = bootstrap,
                                    dimensions = dimensions,
                                    fun        = fun,
                                    nrow       = nrow,
                                    ...),
                    SIMPLIFY = FALSE)))
    }
}

## Calculates disparity from a VCV matrix
decompose.VCV <- function(one_subsets_bootstrap, fun, data, use_array, use_tree = FALSE, dist_help = NULL, dist.data = FALSE, by.col = NULL, ...) {

    # ## Return NA if no data
    # if(length(na.omit(one_subsets_bootstrap)) < 2) {
    #     return(NA)
    # }
    # ## Find which subset of the VCVs to use
    # find.subset <- function(sub, cur) {
    #     if(length(c(sub)) == length(c(cur))) {
    #         return(all(c(sub) == c(cur)))
    #     } else {
    #         return(FALSE)
    #     }
    # }
    verbose_place_holder <- NULL

    ## Apply the fun
    if(!use_tree) {
        if(length(one_subsets_bootstrap) == 1) {

            return(do.call(cbind, lapply(data$covar[[one_subsets_bootstrap]], fun, ...)))
        
        } else {
        
            return(do.call(cbind, mapply(fun, data$covar[[one_subsets_bootstrap[1]]], data$covar[[one_subsets_bootstrap[2]]], MoreArgs = list(...), SIMPLIFY = FALSE)))
            #do.call(cbind, mapply(fun, data$covar[[one_subsets_bootstrap[1]]], data$covar[[one_subsets_bootstrap[2]]], SIMPLIFY = FALSE))
            #fun(data$covar[[one_subsets_bootstrap[1]]][[1]], data$covar[[one_subsets_bootstrap[2]]][[2]])
        
        }
    } else {
        stop("Impossible to use tree metric in dispRity with covar (yet!).", call. = FALSE)
    }
}

## Apply decompose matrix
# fun = first_metric ; warning("DEBUG: dispRity_fun")
decompose.matrix.wrapper <- function(one_subsets_bootstrap, fun, data, use_array, use_tree = FALSE, dist_help = NULL, dist.data = FALSE, by.col = NULL, ...) {
   
    if(is(one_subsets_bootstrap)[[1]] == "list") {
        ## Isolating the matrix into it's two components if the "matrix" is actually a list
        nrow <- one_subsets_bootstrap$nrow[1]
        one_subsets_bootstrap <- one_subsets_bootstrap$data
    } else {
        nrow <- NULL
    }

    ## Decomposing the matrix
    if(use_array) {
        return(array(apply(one_subsets_bootstrap, 2, decompose.matrix, fun = fun, data = data, nrow = nrow, use_tree = use_tree, dist_help = dist_help, dist.data = dist.data, by.col = by.col, ...), dim = c(length(data$call$dimensions), length(data$call$dimensions), ncol(one_subsets_bootstrap))))
 
    } else {

        ## one_subsets_bootstrap is a list (in example) on a single matrix
        results_out <- apply(one_subsets_bootstrap, 2, decompose.matrix, fun = fun, data = data, nrow = nrow, use_tree = use_tree, dist_help = dist_help, dist.data = dist.data, by.col = by.col, ...)

        # one_subsets_bootstrap <- cbind(one_subsets_bootstrap, one_subsets_bootstrap)
        # decompose.matrix(one_subsets_bootstrap[,1], fun = fun, data = data, nrow = nrow, use_tree = use_tree, dist_help = dist_help, dist.data = dist.data, by.col = by.col)


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
# one_subsets_bootstrap <- lapply_loop[[1]][[1]] ; warning("DEBUG: dispRity_fun")
# subsets <- lapply_loop[[1]] ; warning("DEBUG: dispRity_fun")
# one_subsets_bootstrap <- subsets[[1]] ; warning("DEBUG: dispRity_fun")
disparity.bootstraps <- function(one_subsets_bootstrap, metrics_list, data, matrix_decomposition, metric_has_tree = rep(FALSE, length(metrics_list)), dist_help = NULL, dist.data = FALSE, by.col = NULL, ...){
    
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

        if(!eval.covar(first_metric, null.return = FALSE)) {
            ## Decompose the metric using the first metric
            disparity_out <- decompose.matrix.wrapper(one_subsets_bootstrap, fun = first_metric, data = data, use_array = use_array, use_tree = use_tree, dist_help = dist_help, dist.data = dist.data, by.col = by.col, ...)
        } else {
            disparity_out <- decompose.VCV(one_subsets_bootstrap, fun = first_metric, data = data, use_array = use_array, use_tree = use_tree, dist_help = dist_help, by.col = by.col,...)
        }
    } else {
        disparity_out <- one_subsets_bootstrap
    }
    rm(one_subsets_bootstrap)

    #TODO: handle tree metrics after the matrix decomposition

    ## 2 - Applying the metrics to the decomposed matrix
    if(!is.null(metrics_list$level3.fun)) {
        if(!metric_has_tree[1]) {
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
        if(!metric_has_tree[3]) {
            disparity_out <- apply(disparity_out, MARGIN = length(dim(disparity_out)), metrics_list$level1.fun, ...)
        } else {
            disparity_out <- apply(disparity_out, MARGIN = length(dim(disparity_out)), metrics_list$level1.fun, tree = data$tree, ...)
        }
        disparity_out <- t(as.matrix(disparity_out))
    }

    ## Clean the dimnames
    dimnames(disparity_out) <- NULL    

    return(disparity_out)
}

## Lapply wrapper for disparity.bootstraps function
# subsets <- lapply_loop[[1]] ; warning("DEBUG: dispRity_fun")
lapply.wrapper <- function(subsets, metrics_list, data, matrix_decomposition, verbose, metric_has_tree = rep(FALSE, length(metrics_list)), dist_help = NULL, dist.data = FALSE, do_by.col = FALSE, ...) {
    if(verbose) {
        ## Making the verbose version of disparity.bootstraps
        body(disparity.bootstraps)[[2]] <- substitute(message(".", appendLF = FALSE))
    }

    ## Toggle bootstrap by columns
    ## Inherit a toggle from dispRity entering the lapply loop whether to do by columns or not
    if(do_by.col) {
        ## Get the elements and pass them on
        by.col <- subsets$elements
        ## Replace the first subset (elements) by the data dimensions
        subsets$elements <- matrix(data$call$dimensions, ncol = 1)
    } else {
        ## Don't pass anything
        by.col <- NULL
    }

    return(lapply(subsets, disparity.bootstraps, metrics_list, data, matrix_decomposition, metric_has_tree, dist_help, dist.data, by.col, ...))
}
mapply.wrapper <- function(lapply_loop, data, metrics_list, matrix_decomposition, verbose, metric_has_tree, dist_help = NULL, dist.data = FALSE, do_by.col = FALSE, ...) {
    return(lapply(lapply_loop, lapply.wrapper,
        metrics_list = metrics_list,
        data = data,
        matrix_decomposition = matrix_decomposition,
        verbose = verbose,
        metric_has_tree = metric_has_tree,
        dist_help = dist_help,
        dist.data = dist.data,
        do_by.col = do_by.col, ...))
}



#####################
##
## AFTER the lapply_loop
##
#####################



## Split the lapply_loop for bound tree/matrices
lapply_loop.split <- function(lapply_loop, n_trees) {

    split.matrix <- function(matrix, n_trees) {
        ncol_out  <- ncol(matrix)/n_trees
        return(lapply(split(as.vector(matrix), rep(1:n_trees, each = ncol_out * nrow(matrix)) ), matrix, ncol = ncol_out))
    }

    ## Combine them in lapply loops
    return(lapply(as.list(1:n_trees), function(tree, splits) lapply(splits, lapply, `[[`, tree),
            splits = lapply(lapply_loop, lapply, split.matrix, n_trees)))
}

## Split the data for bound tree/matrices
bound.data.split <- function(data) {

    ## Extract the necessary variables
    matrices   <- data$matrix
    trees      <- data$tree

    if((n_matrices <- length(matrices)) != (n_trees <- length(trees))) {
        ## Match the trees and the matrices by multiplying them
        matrices <- unlist(replicate(n_trees, matrices, simplify = FALSE), recursive = FALSE)
        trees <- unlist(replicate(n_matrices, trees, simplify = FALSE), recursive = FALSE)
    }
    ## Splitting the different matrices and trees
    return(mapply(function(matrix, tree, data) {return(list("matrix" = list(matrix), "tree" = c(tree), "call" = list("dimensions" = data$call$dimensions)))},
        matrix = matrices,
        tree   = trees,
        MoreArgs = list(data = data), SIMPLIFY = FALSE))
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

## Transform BAT results into dispRity format
.format.results.subsets <- function(one_subset_lapply, disparities, one_subset) {
    ## Get the results
    results <- disparities[grepl(one_subset, rownames(disparities)), , drop = FALSE]
    rownames(results) <- unlist(lapply(strsplit(rownames(results), split = paste0(one_subset, ".")), `[[`, 2))

    ## Get the elements
    one_subset_lapply$elements <- matrix(nrow = 1, results["elements", ])
    results <- results[-1,, drop = FALSE]
    ## Get the following elements
    length_per_bootstraps <- lapply(one_subset_lapply, ncol)[-1]

    ## Split the rest of the results
    counter <- 0
    while(length(length_per_bootstraps) > 0) {
        counter <- counter + 1
        one_subset_lapply[[1+counter]] <- matrix(results[1:length_per_bootstraps[[1]]], nrow = 1)
        results <- results[-c(1:length_per_bootstraps[[1]]),, drop = FALSE]
        length_per_bootstraps[[1]] <- NULL
    }
    return(one_subset_lapply)
}