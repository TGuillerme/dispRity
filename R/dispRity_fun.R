#FUNCTIONS FOR DISPRITY
get.dispRity.metric.handle <- function(metric, match_call) {
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
        level <- make.metric(metric, silent = TRUE)
        if(level == "level3") {
            stop(paste(as.expression(match_call$metric), " must contain at least a level 1 or a level 2 metric.\nFor more information, see ?make.metric.", sep = ""))
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
        if(length(levels) != length(unique(levels))) stop("Some functions in metric are the same of the same level.\nTry combining them in a single function.\nFor more information, see:\n?make.metric()")

        ## At least one level 1 or level 2 metric is required
        if(length(levels) == 1 && levels[[1]] == "level3") {
            stop("At least one metric must be level 1 or level 2\n.For more information, see:\n?make.metric()")
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



## Calculating the disparity for a bootstrap matrix
disparity.bootstraps <- function(one_bs_matrix, metrics_list, data, matrix_decomposition, ...){ # matrix_decomposition argument is for recalculating disparity

    ## Calculates disparity from a bootstrap table
    decompose.matrix <- function(one_bootstrap, fun, data, ...) {
        return(fun( data$matrix[one_bootstrap, 1:data$call$dimensions], ...))
    }

    if(matrix_decomposition) {
        ## Decompose the matrix using the bootstraps
        decompose_matrix <- TRUE
    } else {
        ## Matrix already decomposed, used the decomposition
        decompose_matrix <- FALSE
        matrix_decomposition <- one_bs_matrix
    }

    ## Level 3 metric decomposition
    ## do the decomposition in the matrix or return FALSE
    ## level 3 decomposition
    if(!is.null(metrics_list$level3.fun)) {
        if(decompose_matrix) {
            #matrix_decomposition <- apply(one_bs_matrix, 2, decompose.matrix, fun = metrics_list$level3.fun, data = data, ...)
            matrix_decomposition <- array(apply(one_bs_matrix, 2, decompose.matrix, fun = metrics_list$level3.fun, data = data, ...), dim = c(data$call$dimensions, data$call$dimensions, ncol(one_bs_matrix)))
            decompose_matrix <- FALSE
        } else {
            matrix_decomposition <- apply(matrix_decomposition, 2, metrics_list$level3.fun, ...)
        }
    }# ; warning("DEBUG dispRity_fun.R")
    ## This should output a list of matrices for each bootstraps


    ## level 2 decomposition
    if(!is.null(metrics_list$level2.fun)) {
        if(decompose_matrix) {
            matrix_decomposition <- apply(one_bs_matrix, 2, decompose.matrix, fun = metrics_list$level2.fun, data = data, ...)
            decompose_matrix <- FALSE
        } else {
            matrix_decomposition <- apply(matrix_decomposition, 3, metrics_list$level2.fun, ...)
        }
    }# ; warning("DEBUG dispRity_fun.R")
    ## This should output a matrix with n-bootstraps columns and n-dimensions rows


    ## level 1 metric decomposition
    if(!is.null(metrics_list$level1.fun)) {
        if(decompose_matrix) {
            matrix_decomposition <- apply(one_bs_matrix, 2, decompose.matrix, fun = metrics_list$level1.fun, data = data, ...)
            decompose_matrix <- FALSE
        } else {
            if(class(matrix_decomposition) != "array") {
                matrix_decomposition <- apply(matrix_decomposition, 2, metrics_list$level1.fun, ...)
            } else {
                matrix_decomposition <- apply(matrix_decomposition, 3, metrics_list$level1.fun, ...)
            }

            ## Transform the vector back into a matrix
            matrix_decomposition <- t(as.matrix(matrix_decomposition))
        }
    }# ; warning("DEBUG dispRity_fun.R")
    ## This should output a matrix with n-bootstraps columns and 1 row

    return(matrix_decomposition)
}

## Lapply wrapper for disparity.bootstraps function
lapply.wrapper <- function(series, metrics_list, data, matrix_decomposition, verbose, ...) {
    if(verbose) message(".", appendLF = FALSE)
    return(lapply(series, disparity.bootstraps, metrics_list, data, matrix_decomposition, ...))
}

one_series <- data$series[[2]]



one_bs_matrix <- as.matrix(one_series$elements)




## Lapply wrapper for getting the observed data
lapply.wrapper.observed <- function(series, metrics_list, data, matrix_decomposition, verbose, ...) {
    if(verbose) message(".", appendLF = FALSE)
    return(lapply(series, disparity.bootstraps, metrics_list, data, matrix_decomposition, ...))
}



## Combining the disparity results with the elements
combine.disparity <- function(one_disparity_series, one_bootstrap_series) {
    return(c(one_bootstrap_series[1], one_disparity_series))
}