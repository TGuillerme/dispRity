#' @title Cleaning phylogenetic data
#'
#' @description Cleans a table/tree to match with a given table/tree
#'
#' @param data A \code{data.frame} or \code{matrix} with the elements names as row names.
#' @param tree A \code{phylo} or \code{multiPhylo} object.
#'
#' @return
#' A \code{list} containing the cleaned data and tree(s) and information on the eventual dropped tips and rows.
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
#' @export

clean.data <- function(data, tree) {

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


    ## CLEANING THE DATA/TREES
    ## for a single tree
    if(tree_class == "phylo") {
        
        cleaned_data <- clean.tree.table(tree, data)

    } else {
        ## for multiple trees
        ## lapply function
        cleaned_list <- lapply(tree, clean.tree.table, data = data)

        ## Selecting the tips to drop
        tips_to_drop <- unique(unlist(lapply(cleaned_list, function(x) x[[3]])))
        ## removing NAs
        tips_to_drop <- tips_to_drop[-which(is.na(tips_to_drop))]

        ## Selecting the rows to drop
        rows_to_drop <- unique(unlist(lapply(cleaned_list, function(x) x[[4]])))
        ## removing NAs
        rows_to_drop <- rows_to_drop[-which(is.na(rows_to_drop))]

        ## Combining both
        taxa_to_drop <- c(tips_to_drop, rows_to_drop)

        ## Dropping the tips across all trees
        if(length(taxa_to_drop) != 0) {
            tree_new <- lapply(tree, drop.tip, taxa_to_drop) ; class(tree_new) <- 'multiPhylo'
        } else {
            ## removing taxa from the trees
            ## keep the same trees
            tree_new <- tree
            if(length(tips_to_drop) == 0) tips_to_drop <- NA
        }

        ## Dropping the rows
        if(length(rows_to_drop) != 0) {
            ## removing taxa from the data
            data_new <- data[-match(rows_to_drop, rownames(data)),]
        } else {
            ## keep the same data
            data_new <- data
            if(length(rows_to_drop) == 0) rows_to_drop <- NA
        }

        ## output list
        cleaned_data <- list("tree" = tree_new, "data" = data_new, "dropped_tips" = tips_to_drop,  "dropped_rows" = rows_to_drop)
    }

    return(cleaned_data)

## End
}
