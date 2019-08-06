#' @title Stretching a tree
#'
#' @description Stretches a phylogenetic tree at a particular node
#'
#' @param nodes A list of the ID nodes to slide (\code{"integer"}). The first node is \code{ape::Ntip(tree) + 1}, etc.
#' @param tree a \code{"phylo"} object.
#' @param slide the sliding value.
#' 
#' @details
#' The sliding works by subtracting the slide value to the branch leading to the node and adding it to the descendant branches.
#' Note that the slide value can be negative to slide nodes the other way (up); the only requirement is that the slide does not lead to negative branch length values.
#' 
#' @return
#' A \code{"phylo"} object.
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

slide.nodes <- function(nodes, tree, slide) {
    ## Sanitizing
    check.class(nodes, c("integer", "numeric"))
    nodes <- as.integer(nodes)
    check.class(tree, "phylo")

    ## Check whether nodes exist in the tree
    if(any(nodes > (Nnode(tree)+Ntip(tree)))) stop("node(s) not found in tree.")
    if(any(nodes < Nnode(tree))) stop("node(s) not found in tree.")
    if(any(nodes == (Ntip(tree)+1))) warning(paste0("The parent of the root node (", (Ntip(tree) + 1), ") cannot be slideed."))

    ## Check whether the tree has edge lengths
    if(is.null(tree$edge.length)) stop("The tree has no edge lengths.")

    ## Slide
    check.class(slide, c("numeric", "integer"))

    ## Find the parent and descendants
    parent_edge <- which(tree$edge[,2] %in% nodes)
    descendant_edge <- which(tree$edge[,1] %in% nodes)

    ## Stretch the nodes
    if(length(parent_edge) != 0) {
        tree$edge.length[parent_edge] <- tree$edge.length[parent_edge] + slide
        ## Check for negatives
        if(any(tree$edge.length[parent_edge] < 0)) {
            stop(paste0("The slide value (", slide, ") produced negative branch length(s)."))
        }
    }
    tree$edge.length[descendant_edge] <- tree$edge.length[descendant_edge] - slide
    ## Check for negatives
    if(any(tree$edge.length[descendant_edge] < 0)) {
        stop(paste0("The slide value (", slide, ") produced negative branch length(s)."))
    }
    return(tree)
}