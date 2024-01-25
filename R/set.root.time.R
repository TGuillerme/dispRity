#' @name set.root.time
#' 
#' @title Adds root time to a tree
#' 
#' @description Adds or replace root time to a tree by calculating it's root's depth
#'
#' @param tree A \code{phylo}, \code{mutiPhylo} or \code{dispRity} object that contains trees.
#' @param present The age of the most recent tip. By default this is set to \code{0}. 
#' 
#' @examples
#' ## A random tree with no root.time
#' my_tree <- rtree(10)
#' my_tree$root.time # is NULL
#'
#' 
set.root.time <- function(tree, present = 0) {
    ## Check input
    input_class <- check.class(tree, c("phylo", "multiPhylo", "dispRity"))

    if(input_class == "phylo") {
        return(add.root.time(tree, present))
    }
    if(input_class == "multiPhylo") {
        tree <- lapply(tree, add.root.time, present)
        class(tree) <- "multiPhylo"
        return(tree)
    }
    if(input_class == "dispRity") {
        if(is.null(tree$tree)) {
            stop("input dispRity object doesn't contain any tree(s).")
        } else {
            disparitree <- lapply(tree$tree, add.root.time, present)
            class(disparitree) <- "multiPhylo"
            tree$tree <- disparitree
            return(tree)
        }
    }
}
## Internal
add.root.time <- function(tree, present) {
    tree$root.time <- max(tree.age(tree, digits = .Machine$double.digits)$ages) + present
    return(tree)
}