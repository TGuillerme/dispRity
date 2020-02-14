#' @title Cleaning phylogenetic data
#'
#' @description Cleans a table/tree to match with a given table/tree
#'
#' @param data A \code{data.frame}, \code{matrix} or a \code{list} of data frames or matrices with the elements names as row names.
#' @param tree A \code{phylo} or \code{multiPhylo} object.
#' @param pairwise \code{logical}, the same number of datasets and trees are provided, whether to match them only two by two (\code{TRUE}) or not (\code{FALSE} - default).
#'
#' @return
#' A \code{list} containing the cleaned data(s) and tree(s) and information on the eventual dropped tips and rows.
#'
#' @examples
#' ##Creating a set of different trees
#' trees_list <- list(rtree(5, tip.label = LETTERS[1:5]), rtree(4,
#'      tip.label = LETTERS[1:4]), rtree(6, tip.label = LETTERS[1:6]))
#' class(trees_list) <- "multiPhylo"
#' 
#' ##Creating a matrix
#' dummy_data <- matrix(c(rnorm(5), runif(5)), 5, 2,
#'     dimnames = list(LETTERS[1:5], c("var1", "var2")))
#'
#' ##Cleaning the trees and the data
#' cleaned <- clean.data(data = dummy_data, tree = trees_list)
#' ##The taxa that where dropped (tips and rows):
#' c(cleaned$dropped_tips, cleaned$dropped_rows)
#' ##The cleaned trees:
#' cleaned$tree
#' ##The cleaned data set:
#' cleaned$data
#'
#' @seealso \code{\link{tree.age}}.
#' 
#' @author Thomas Guillerme
# @export
clean.data <- function(data, tree, pairwise = FALSE) {

    ## Get call
    match_call <- match.call()

    ## SANITIZING
    ## tree
    tree_class <- check.class(tree, c("phylo", "multiPhylo"), " must be a phylo or multiPhylo object.")
    if(tree_class == "phylo") {
        ## Make into a list
        tree <- list(tree)
    }

    ## data
    data_class <- check.class(data, c("matrix", "data.frame", "list"), " must be a data.frame, a matrix or a list of data frames and matrices.")

    ## check classes
    if(data_class == "list") {
        ## Check internal classes
        data_classes <- unlist(lapply(data, class))
        if(any(!(data_classes %in% c("matrix", "data.frame")))) {
            stop(paste0(match_call$data, " must be a data.frame, a matrix or a list of data frames and matrices."), call. = FALSE)
        }

        ## Check for rownames
        data_rownames <- unlist(lapply(data, rownames))
        if(any(is.null(data_rownames))) {
            stop.call(match_call$data, " must have row names for each matrix or data.frame in the list.")
        }
    } else {
        ## Make into a list
        data <- list(data)
    }

    if(!pairwise) { 
        ## Run normal comparisons (all to all)
        return(clean.data.internal(data, tree, tree_class, data_class))
    } else {
        ## Check whether both have the same length
        if(length(data) != length(tree)) {
            stop.call(msg.pre = "Both ", call = match_call$data, msg = paste0(" and ", as.expression(match_call$tree), " must have the same length."))
        }

        ## Double list everything
        lapply(data, function(x) list(x))

        ## Apply a pairwise comparison only
        return(mapply(clean.data.internal,
                     lapply(data, function(x) list(x)),
                     lapply(tree, function(x) list(x)),
                     MoreArgs = list(tree_class = tree_class, data_class = data_class), SIMPLIFY = FALSE))
    }
}