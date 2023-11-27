## Internal for slide.nodes
slide.nodes.internal <- function(tree, nodes, slide, allow.negative.root) {
    ## Find the parent and descendants
    parent_edge <- which(tree$edge[,2] %in% nodes)
    descendant_edge <- which(tree$edge[,1] %in% nodes)

    ## Stretch the nodes
    if(length(parent_edge) != 0) {
        tree$edge.length[parent_edge] <- tree$edge.length[parent_edge] + slide
        ## Check for negatives
        if(any(tree$edge.length[parent_edge] < 0)) {
            return(NULL)
        }
    }
    tree$edge.length[descendant_edge] <- tree$edge.length[descendant_edge] - slide
    ## Check for negatives
    if(!allow.negative.root && any(tree$edge.length[descendant_edge] < 0)) {
        return(NULL)
    }
    return(tree)
}
