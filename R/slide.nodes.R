#' @title Stretching a tree
#'
#' @description Stretches a phylogenetic tree at a particular node
#'
#' @param nodes A list of the ID nodes to slide (\code{"integer"}) or names (\code{"character"}). The first node is \code{ape::Ntip(tree) + 1}, etc.
#' @param tree a \code{"phylo"} object.
#' @param slide the sliding value.
#' @param allow.negative.root logical, whether to allow negative branch lengths and moving the root node (\code{TRUE}) or not (\code{FALSE}; default).
#' 
#' @details
#' The sliding works by subtracting the slide value to the branch leading to the node and adding it to the descendant branches.
#' Note that the slide value can be negative to slide nodes the other way (up); the only requirement is that the slide does not lead to negative branch length values.
#' 
#' @return
#' A \code{"phylo"} object.
#' 
#' @seealso
#' \code{\link{remove.zero.brlen}}
#' 
#' @examples
#' set.seed(42)
#' ## Generating a coalescent tree
#' tree <- rcoal(5)
#'
#' ## Stretching node 8 up and down
#' tree_slide_up <- slide.nodes(8, tree, slide = 0.075)
#' tree_slide_down <- slide.nodes(8, tree, slide = -0.075)
#' 
#' ## Display the results
#' par(mfrow = c(3,1))
#' plot(tree) ; axisPhylo() ; nodelabels()
#' plot(tree_slide_up) ; axisPhylo() ; nodelabels()
#' plot(tree_slide_down) ; axisPhylo() ; nodelabels()
#' 
#' ## Stretching many nodes
#' set.seed(42)
#' tree <- rtree(50)
#' move_nodes <- c(99, 93, 53, 86, 58, 63, 60, 84)
#' tree_slided <- slide.nodes(move_nodes, tree, slide = 0.07)
#' 
#' ## Display the results
#' par(mfrow = c(2, 1))
#' node_colors <- c("lightblue", "orange")[((1:Nnode(tree))+Ntip(tree)) %in% move_nodes + 1]
#' plot(tree, show.tip.label = FALSE) ; axisPhylo()
#' nodelabels(bg = node_colors, cex = 0.5)
#' plot(tree_slided, show.tip.label = FALSE) ; axisPhylo()
#' nodelabels(bg = node_colors, cex = 0.5)
#' 
#' @author Thomas Guillerme
#' @export

slide.nodes <- function(nodes, tree, slide, allow.negative.root = FALSE) {
    ## Sanitizing
    check.class(allow.negative.root, "logical")
    node_class <- check.class(nodes, c("integer", "numeric", "character"))

    ## Getting the node IDs (if character)
    if(node_class == "character") {
        if(is.null(tree$node.label)) {
            stop("The tree has no node labels, provide the nodes as integers.", call. = FALSE)
        } 
        nodes <- which(tree$node.label %in% nodes) + Ntip(tree)
    }
    nodes <- as.integer(nodes)
    check.class(tree, "phylo")

    ## Check whether nodes exist in the tree
    if(any(nodes > (Nnode(tree)+Ntip(tree)))) stop("node(s) not found in tree.", call. = FALSE)
    if(any(nodes < Nnode(tree))) stop("node(s) not found in tree.", call. = FALSE)
    if(!allow.negative.root) {
        if(any(nodes == (Ntip(tree)+1))) warning(paste0("The parent of the root node (", (Ntip(tree) + 1), ") cannot be slid."))
    }

    ## Check whether the tree has edge lengths
    if(is.null(tree$edge.length)) stop("The tree has no edge lengths.", call. = FALSE)

    ## Slide
    check.class(slide, c("numeric", "integer"))

    ## Slide the nodes
    tree <- slide.nodes.internal(tree, nodes, slide, allow.negative.root)

    ## Catch eventual errors
    if(is.null(tree)) {
        stop(paste0("The slide value (", slide, ") produced negative branch length(s)."), call. = FALSE)
    }
    return(tree)
}