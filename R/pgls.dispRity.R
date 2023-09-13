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
        stop.call("No tree was found in the provided data and none was provided through the tree argument.")
    }

    ## Check the formula
    if(missing(formula)) {
        ## Select the formula
        formula <- get.formula(data)
    }

    ## Get the pgls data
    datas <- get.pgls.data(data, formula)


    # ## Get the list of disparities
    # datas <- get.data(data, trees)



    ## Check if response is disparity
    if(as.character(formula[[2]]) != "disparity") {
        stop("The response term of the formula must be 'disparity'.", call. = FALSE)
    }

    ## Check model
    check.method(model, all_arguments = eval(formals(phylolm::phylolm)$model), msg = "model")

    ## Set the phylolm optional args
    phylolm_args <- as.list(c(..., optim))
    # warning("DEBUG"); phylolm_args <- as.list(c(optim))
    ## Add the main arguments
    phylolm_args$formula <- formula
    phylolm_args$model   <- model

    ## Run all the models
    models_out <- mapply(one.phylolm, trees, datas, MoreArgs = list(args = phylolm_args), SIMPLIFY = FALSE)

    ## Handle the output
    if(length(models_out) == 1) {
        return(models_out[[1]])
    } else {
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

get.pgls.data <- function(data, tree) {
    ## Extract the trees (as a list)
    trees <- get.tree(data)
    if(is(trees, "phylo")) {
        trees <- list(trees)
    }

    ## Extract the disparity results (as a list)
    # disparity <- get.disparity(data, observed = TRUE, concatenate = FALSE)

    # data$disparity


    ## Make the disparity data.frames

    ## Match the number of disparity data.frames and the number of trees 
}


## Grabs the data to be passed to phylolm
get.data <- function(data, trees) {
    ## Extract the disparity
    disparity <- get.disparity(data, observed = TRUE, concatenate = FALSE)

    if(dim(disparity[[1]])[2] > 1) {
        ## Has multiple matrices
        ## Get all matrices
        get.one.group <- function(one.group, names) {
            return(apply(one.group, 2, function(x) data.frame(disparity = unlist(x, use.names = FALSE), row.names = names)))
        }

        list_of_matrices <- do.call(cbind, lapply(disparity, get.one.group, names = ))



    } else {
        data_out <- data.frame(disparity = unlist(disparity, use.names = FALSE))
        rownames(data_out) <- unlist(lapply(disparity, names), use.names = FALSE)
        data_out <- list(data_out)
    }




    ## Add predictors (if needed)
    if(!is.null(data$call$subsets)) {
        if(data$call$subsets[[1]] == "customised") {
            ## Fill the empty group
            data_out$group <- NA
            ## Get the subsets
            subsets <- lapply(data$subsets, `[[`, "elements")
            ## Fill the subsets
            while(length(subsets) != 0) {
                data_out$group[c(subsets[[1]])] <- names(subsets)[1]
                subsets[1] <- NULL
            }
        }
    }
    ## Return the datasets (one per tree)
    return(replicate(length(trees), data_out, simplify = FALSE))
}

## Run one phylolm
one.phylolm <- function(one_tree, one_data, args) {
    ## Adding the tree and the data
    args$phy  <- one_tree
    args$data <- one_data

    ## Run the phylolm
    run_out <- do.call(phylolm, args)
    ## Edit the call
    run_out$call <- paste0(c("dispRity interface of phylolm using: formula = ", args$formula, " and model = ", args$model), collapse = "")
    return(run_out)
}