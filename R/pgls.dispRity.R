#' @title phylolm dispRity (from \code{phylolm::phylolm})
#'
#' @description Passing \code{dispRity} objects to the \code{\link[phylolm]{phylolm}} function from the \code{phylolm} package. Typically to run some PGLS.
#'
#' @param data A \code{dispRity} object with a metric of dimension level 2 at least
#' @param tree If \code{data} does not contain a \code{tree} component, a \code{"phylo"} object to be used as the tree.
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
#' ## PGLS 
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
    if(is.null(unlist(data$tree))) {
        ## Check the tree format
        data <- add.tree(data, tree = tree)
    }
    ## Get the trees
    trees <- get.tree(data)


    ## Check the formula
    if(missing(formula)) {
        ## Handle the formula depending on what is in data
        formula <- disparity ~ 1

            ## If data has subsets
                ## Set model to disparity ~ groups
                ## Else, set to disparity ~ 1


    }
    ## Check if response is disparity
    if(as.character(formula[[2]]) != "disparity") {
        stop("The response term of the formula must be 'disparity'.", call. = FALSE)
    }

    ## Check model

    # ## Set the phylolm args
    # phylolm_args <- list(...)
    # phylolm_args$formula <- 
    # phylolm_args$model <-

    # ## Handle the multiple matrices
    # phylolm_args$data <-
    # ## Handle the multiple trees
    # phylolm_args$tree <-

    # ## Run all the PGLS
    # do.call(phylolm, phylolm_args)

    ## Run PGLS on all the trees

    ## Handle outputs

    return(NULL)
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
