#' @title phylolm dispRity (from \code{phylolm::phylolm})
#'
#' @description Passing \code{dispRity} objects to the \code{\link[phylolm]{phylolm}} function from the \code{phylolm} package. Typically to run some PGLS.
#'
#' @param data A \code{dispRity} object with a metric of dimension level 2 at least
#' @param tree If \code{data} does not contain a tree component, a \code{"phylo"} or \code{"multiPhylo"} object to be used as the tree. If \code{data} already contains a tree component and the \code{tree} argument is not missing, the provided \code{tree} will replace any contained in \code{data}.
#' @param formula The PGLS formula. If left empty, runs either \code{disparity ~ 1} or \code{disparity ~ subsets} if \code{data} contains subsets.
#' @param model The covariance model (default is \code{"BM"}). For more details (including the models available) see the manual for \code{\link[phylolm]{phylolm}}.
#' @param ... Any optional arguments to be passed to \code{\link[phylolm]{phylolm}}
#' @param optim An optional named list of arguments to be passed to the function \code{optim}
#' 
#' @details
#' The \code{formula} needs to be expressed by always naming the response variable \code{disparity} to use the calculated disparity data from \code{data}.
#' 
#' Optional arguments \code{...} correspond to all the non-ambiguous named arguments from the \code{\link[phylolm]{phylolm}}. Optional arguments for the internal \code{optim} function can be passed as a named list to the \code{optim} argument.
#' 
#' @seealso
#' \code{\link[phylolm]{phylolm}}, \code{\link{test.dispRity}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}.
#' 
#' @examples
#' ## Simple example
#' data(BeckLee_mat50)
#' data(BeckLee_tree)
#' disparity <- dispRity(BeckLee_mat50, metric = centroids, tree = BeckLee_tree)
#'
#' ## Running a simple PGLS
#' model <- pgls.dispRity(disparity)
#' summary(model)
#'
#' ## More complex example running a PGLS
#' ## on multiple trees and using groups as a predictor 
#' 

#' @author Thomas Guillerme
pgls.dispRity <- function(data, tree, formula, model = "BM", ..., optim = list()) {

    match_call <- match.call()

    ## Check data
    check.class(data, "dispRity")
    ## Check data level
    disparity_list <- get.disparity(data)
    checks <- unlist(lapply(disparity_list, check.dimension))
    if(any(!checks)) {
        stop.call(msg.pre =  "Impossible to run a univariate pgls on ", match_call$data, msg = " because doesn't contain a dimension level-2 metric. See ?dispRity.metric for more info.")
    }

    ## Check tree in data
    if(!missing(tree)) {
        data <- add.tree(data, tree = tree, replace = TRUE)
    } else {
        if(is.null(get.tree(data))) {
            stop("No tree was found in the provided data and none was provided through the tree argument.")
        }
    }

    ## Check the formula
    if(missing(formula)) {
        ## Select the formula
        formula <- get.formula(data)
    }

    ## Check if response is disparity
    if(as.character(formula[[2]]) != "disparity") {
        stop("The response term of the formula must be 'disparity'.", call. = FALSE)
    }

    ## Get the pgls data
    data_list <- get.pgls.data(data)

    ## Check model
    check.method(model, all_arguments = eval(formals(phylolm::phylolm)$model), msg = "model")

    ## Set the phylolm optional args
    phylolm_args <- as.list(c(..., optim))
    # warning("DEBUG"); phylolm_args <- as.list(c(optim))
    ## Add the main arguments
    phylolm_args$formula <- formula
    phylolm_args$model   <- model

    ## Run all the models
    models_out <- lapply(data_list, one.phylolm, phylolm_args)

    ## Handle the output
    if(length(models_out) == 1) {
        return(models_out[[1]])
    } else {
        class(models_out) <- c("dispRity", "pgls.dispRity")
        return(models_out)
    }
}

## Internals
## Check the dimension of the data
check.dimension <- function(one_disparity) {
    ## Must have names, be numeric (or integer), no dimensions and length > 1
    return(!is.null(names(one_disparity)) &&
           (is.numeric(one_disparity) || is.integer(one_disparity)) &&
           is.null(dim(one_disparity)) &&
           length(one_disparity) > 1)
}

## Outputs the formula depending on what's in the object
get.formula <- function(disparity) {
    if(is.null(disparity$call$subsets)) {
        return(disparity ~ 1)
    } else {
        ## Grouped data
        ## Get the groups
        group <- lapply(disparity$subsets, function(x) return(c(x$elements)))
        ## Check overlap
        if(any(table(unlist(group)) != 1)) {
            stop("Some groups have overlapping elements.")
        }
        ## Return the correct formula
        if(disparity$call$subsets[[1]] == "customised") {
            return(disparity ~ group)
        } else {
            ## Warning for time auto-correlation
            stop("It is currently not possible to apply an phylogenetic linear model on dispRity data with time series.")
            # warning("Data contains time series: the default formula used is disparity ~ time but it does not take time autocorrelation into account.", call. = FALSE)
            # colnames(group_table) <- "time"
            # return(list(formula = disparity ~ time, group = NULL, time = group_table))
        }
    }
}

## Formats the data and trees for phylolm
get.pgls.data <- function(data) {
    ## Extract the trees (as a list)
    trees <- get.tree(data)
    if(is(trees, "phylo")) {
        trees <- list(trees)
    }

    ## Extract the disparity results (as a list)
    disparity <- get.disparity(data, observed = TRUE, concatenate = FALSE)

    ## Split the data per matrix and per group
    split_data <- lapply(disparity, function(X) apply(X, 2, function(x) return(x), simplify = FALSE))

    ## Get the rownames for all the data
    #TG: expecting all the matrices to have the same rownames
    row_names <- rownames(data$matrix[[1]])[unlist(lapply(data$subsets, function(x) return(x[["elements"]][,1])))]

    ## Split between matrix
    data_list <- list()
    while(length(split_data[[1]]) > 0) {
        ## Get the disparity and group for one matrix
        dispa <- unlist(lapply(split_data, `[[`, 1))
        group <- unlist(mapply(rep_len, as.list(names(split_data)), lapply(lapply(split_data, `[[`, 1), length)))
        ## Populate the data list
        if(!is.null(group)) {
            data_list[[length(data_list)+1]] <- data.frame("disparity" = dispa, "group" = group, row.names = row_names)
        } else {
            data_list[[length(data_list)+1]] <- data.frame("disparity" = dispa, row.names = row_names)
        }
        ## Remove from the list
        split_data <- lapply(split_data, function(x) {x[[1]] <- NULL; return(x)})
    }

    ## Combine both trees and matrices
    if(length(trees) != length(data_list)) {
        ## Check if feasible
        multiple_trees <- (length(trees) > 1)
        multiple_datas <- (length(data_list) > 1)
        if(multiple_datas && multiple_trees) {
            stop(paste0("Data must either same number of matrices (", length(data_list), ") and trees (", length(trees) , ") or just one tree or matrix combined with respectively with multiple matrices or trees."))
        }
        ## Combine the data
        if(multiple_datas) {
            data_out <- lapply(data_list, function(data, tree) return(list(data = data, phy = trees[[1]])), trees)
        } else {
            data_out <- lapply(trees, function(tree, data) return(list(data = data[[1]], phy = tree)), data_list)
        }
    } else {
        data_out <- mapply(function(data, tree) return(list(data = data, phy = tree)), data_list, trees, SIMPLIFY = FALSE)
    }
    return(data_out)
}

## Run one phylolm
one.phylolm <- function(one_datas, args) {
    ## Adding the tree and the data
    args$phy  <- one_datas$phy
    args$data <- one_datas$data

    ## Run the phylolm
    run_out <- do.call(phylolm, args)
    ## Edit the call
    run_out$call <- paste0(c("dispRity interface of phylolm using: formula = ", args$formula, " and model = ", args$model), collapse = "")
    return(run_out)
}


## Pooling output data together for plot and summary
pool.pgls.param <- function(x, param, fun = c(median = median, sd = sd)) {
    ## Extract the parameters
    param_values <- lapply(x, `[[`, param)
    ## Make them into a table
    param_values <- do.call(rbind, param_values)
    ## Get param names
    if(is.null(colnames(param_values))) {
        param_names <- param
    } else {
        param_names <- colnames(param_values)
    }
    ## Output
    return(matrix(c(apply(param_values, 2, fun[[1]]), apply(param_values, 2, fun[[2]])), ncol = length(fun), dimnames = list(c(param_names), names(fun))))
}

# pool.pgls.dispRity <- function(results) {
#     ## Pool everything!
#     pooled_list <- list()
#     to_pool <- names(results[[1]])
#     while(length(to_pool) > 0) {
#         ## Raw pool
#         pooled <- lapply(results, `[[`, to_pool[1])
#         ## Smarter pool
#         if(length(unique(unlist(pooled))) == 1) {
#             ## Is the same (probs parameter)
#             pooled_list[[to_pool[1]]] <- unique(unlist(pooled))
#         } else {
#             if(is.null(unlist(pooled))) {
#                 ## Is null (only pool one)
#                 pooled_list[to_pool[1]] <- list(NULL)
#             } else {
#                 if(all(unlist(lapply(pooled, function(x) !is.null(names(x)))))) {
#                     ## Named vector
#                     pooled_list[[to_pool[1]]] <- unlist(lapply(results, `[[`, to_pool[1]))
#                 } else {
#                     pooled_list[[to_pool[1]]] <- lapply(results, `[[`, to_pool[1])
#                 }
#             }
#         } 
#         to_pool <- to_pool[-1]
#     }

#     ## Sort sigma2, logLik, aic and mean.tip.height
#     pooled_list[["sigma2"]] <- median(unlist(pooled_list[["sigma2"]]))
#     pooled_list[["logLik"]] <- median(unlist(pooled_list[["logLik"]]))
#     pooled_list[["aic"]] <- median(unlist(pooled_list[["aic"]]))
#     pooled_list[["mean.tip.height"]] <- mean(unlist(pooled_list[["mean.tip.height"]]))

#     ## Sort coeffients and vcov
#     # pooled_list[["coefficients"]]
#     # pooled_list[["vcov"]]

#     ## Adjust the call
#     pooled_list$call <- c(pooled_list$call,
#                           "AIC, logLik and sigma2 values are pooled medians.",
#                           "AOV is based on the median coefficients estimates.")

#     ## Restate class
#     class(pooled_list) <- "phylolm"
#     return(pooled_list)
# }


