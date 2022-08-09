#' @title Cleaning phylogenetic data
#'
#' @description Cleans a table/tree to match with a given table/tree
#'
#' @param data A \code{data.frame} or \code{matrix} with the elements names as row names.
#' @param tree A \code{phylo} or \code{multiPhylo} object.
#' @param inc.nodes Logical, whether to check if the nodes in \code{tree} are also present in \code{data} (\code{TRUE}) or not (\code{FALSE}; default).
#'
#' @return
#' A \code{list} containing the cleaned data and tree(s) and information on the eventual dropped tips and rows.
#'
#' @details
#' Note if \code{inc.nodes} is set to \code{TRUE}, the function outputs an error message if there is no matching.
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

clean.data <- function(data, tree, inc.nodes = FALSE) {

    ## Get call
    match_call <- match.call()

    ## SANITIZING
    ## data
    silent <- check.class(data, c("matrix", "data.frame"), " must be a data.frame or matrix object.")
    ##  must have row names
    if(is.null(rownames(data))) {
        stop.call(match_call$data, " must have row names.")
    }

    ## tree
    tree_class <- check.class(tree, c("phylo", "multiPhylo"), " must be a phylo or multiPhylo object.")

    ## inc.nodes
    check.class(inc.nodes, "logical")
    if(inc.nodes) {
        if(tree_class == "phylo") {
            if(is.null(tree$node.label)) {
                stop("Impossible to use check the nodes in the tree because the tree has no nodes. Set the option inc.nodes = FALSE, or add node labels to the tree (e.g. using ape::makeNodeLabels(...)).", call. = FALSE)
            }
        } else {
            if(any(no_lab <- unlist(lapply(tree, function(x) is.null(x$node.label))))) {
                stop(paste0("Impossible to use check the nodes in the tree", ifelse(sum(no_lab) > 1, "s ", " "), paste(which(no_lab), collapse = ", "), " because the tree has no nodes. Set the option inc.nodes = FALSE, or add node labels to the tree (e.g. using ape::makeNodeLabels(...))."), call. = FALSE)
            }
        }
    }

    ## CLEANING THE DATA/TREES
    ## for a single tree
    if(tree_class == "phylo") {
        ## Cleaning
        cleaned_data <- clean.tree.table(tree, data, inc.nodes = inc.nodes)

    } else {
        ## for multiple trees
        ## lapply function
        cleaned_list <- lapply(tree, clean.tree.table, data = data, inc.nodes = inc.nodes)

        ## Check if all matched (no missing nodes)
        all_matched <- unlist(lapply(cleaned_list, function(x) return(methods::is(x, "list"))))
        if(any(!all_matched)) {
            
            ## Errors
            wrongs_nodes <- cleaned_list[!all_matched]
            wrongs_trees <- which(!all_matched)
            error_msg <- NULL
            for(i in 1:length(wrongs_trees)) {
                error_msg <- c(error_msg, paste0("Node", ifelse(length(wrongs_nodes[[i]]) > 1, "s ", " "), paste(wrongs_nodes[[i]], collapse = ", "), " from tree ", wrongs_trees[[i]]," not found in the data."))
            }
            stop(paste(error_msg, "(nodes cannot be trimmed automatically).") , call. = FALSE)
        }

        ## Selecting the tips to drop
        tips_to_drop <- unique(unlist(lapply(cleaned_list, function(x) x[[3]])))
        ## removing NAs
        if(any(is.na(tips_to_drop))) {
            tips_to_drop <- tips_to_drop[-which(is.na(tips_to_drop))]    
        }

        ## Selecting the rows to drop
        rows_to_drop <- unique(unlist(lapply(cleaned_list, function(x) x[[4]])))
        ## removing NAs
        if(any(is.na(rows_to_drop))) {
            rows_to_drop <- rows_to_drop[-which(is.na(rows_to_drop))]
        }
        
        ## Combining both
        taxa_to_drop <- c(tips_to_drop, rows_to_drop)

        ## Dropping the tips across all trees
        if(length(taxa_to_drop) != 0) {
            tree_new <- lapply(tree, drop.tip, taxa_to_drop)
            class(tree_new) <- 'multiPhylo'
        } else {
            ## removing taxa from the trees
            ## keep the same trees
            tree_new <- tree
        }

        ## Dropping the rows
        if(length(rows_to_drop) != 0) {
            ## removing taxa from the data
            data_new <- data[-match(rows_to_drop, rownames(data)),]
        } else {
            ## keep the same data
            data_new <- data
        }

        ## Replacing empty vectors by NAs
        if(length(tips_to_drop) == 0) tips_to_drop <- NA
        if(length(rows_to_drop) == 0) rows_to_drop <- NA

        ## output list
        cleaned_data <- list("tree" = tree_new, "data" = data_new, "dropped_tips" = tips_to_drop,  "dropped_rows" = rows_to_drop)
    }

    if(!is(cleaned_data, "list")) {
        stop(paste0("Node", ifelse(length(cleaned_data) > 1, "s ", " "), paste(cleaned_data, collapse = ", "), " not found in the data (nodes cannot be trimmed automatically)."), call. = FALSE)
    } else {
        return(cleaned_data)
    }
}
