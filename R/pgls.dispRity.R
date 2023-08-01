#' @title phylolm dispRity (from \code{phylolm::phylolm})
#'
#' @description Passing \code{dispRity} objects to the \code{\link[phylolm]{phylolm}} function from the \code{phylolm} package. Typically to run some PGLS.
#'
#' @param data A \code{dispRity} object with a metric of dimension level 2 at least
#' @param tree If \code{data} does not contain a \code{tree} component, a \code{"phylo"} object to be used as the tree.
#' @param formula Optional, the PGLS formula. If left empty, runs either \code{disparity ~ 1} or \code{disparity ~ subsets} if \code{data} contains subsets.
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

pgls.dispRity <- function(data, tree, formula, ..., optim = list()) {

    ## Check data (level 2)

    ## Check tree in data
        ## If tree is given, replace the tree in data

    ## Check the formula
        ## If missing
            ## the first term is disparity ~ 
            ## If data has subsets
                ## Set model to disparity ~ groups
                ## Else, set to disparity ~ 1
        ## Regardless check if response is "disparity"

    ## Run PGLS on all the trees

    ##
}