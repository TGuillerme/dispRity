#' @title Separates stem and crown species
#'
#' @description Selects the crown
#'
#' @param tree a code{"phylo"} object
#' @param inc.nodes whether to include the nodes (\code{TRUE}; default) or not (\code{FALSE}) in the output.
#' @param output.names whether to output the taxa names (\code{TRUE}; default) or two phylogenetic trees (\code{FALSE}).
#' 
#' @examples
#' ## A tree with fossil taxa
#' data(BeckLee_tree)
#' 
#' ## Getting both crown and stem taxa lists
#' crown.stem(BeckLee_tree)
#' 
#' ## Splitting the tree into two subtrees
#' crown_stem_trees <- crown.stem(BeckLee_tree, output.names = FALSE)
#' ## Graphical parameters
#' op <- par(mfrow = c(1,3))
#' ## Plotting the trees
#' plot(BeckLee_tree, main = "Full tree")
#' plot(crown_stem_trees$crown, main = "Crown group")
#' plot(crown_stem_trees$stem, main = "Stem group")
#'
#' @seealso \code{\link{custom.subsets}}, \code{\link{tree.age}}
#' 
#' @author Thomas Guillerme
#' @export

## DEBUG
#source("sanitizing.R")

crown.stem <- function(tree, inc.nodes = TRUE, output.names = TRUE) {

    ## Sanitizing
    check.class(tree, "phylo")
    check.class(inc.nodes, "logical")
    check.class(output.names, "logical")
    match_call <- match.call()

    ## Get the tree ages
    tree_ages <- tree.age(tree)

    ## Selecting the living taxa
    living_taxa <- as.character(tree_ages[which(tree_ages[,1] == 0), 2])

    if(length(living_taxa) == 0) {
        stop(paste0("No taxa with age 0 found in ", as.expression(match_call$tree), "."), call. = FALSE)
    }

    ## Finding the MRCA for these species
    MRCA <- getMRCA(tree, tip = living_taxa)

    if(is.null(MRCA)) {
        stop(paste0("Only one taxon of age 0 found (", living_taxa ,")."), call. = FALSE)
    }

    ## Extract the crown group
    crown_tree <- extract.clade(tree, node = MRCA)

    ## Extract the stem group
    stem_tree <- drop.tip(tree, tip = crown_tree$tip.label)

    ## Transforming the output in names
    if(output.names) {
        ## Extract the names
        if(inc.nodes) {
            crown_names <- c(crown_tree$tip.label, crown_tree$node.label)
            stem_names <- c(stem_tree$tip.label, stem_tree$node.label)
        } else {
            crown_names <- c(crown_tree$tip.label)
            stem_names <- c(stem_tree$tip.label)
        }
        return(list("crown" = crown_names, "stem" = stem_names))
    } else {
        ## Make a multiphylo object
        output <- list("crown" = crown_tree, "stem" = stem_tree)
        class(output) <- "multiPhylo"
        return(output)
    }
}