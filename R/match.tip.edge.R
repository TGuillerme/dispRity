#' @title Match tips or nodes edge vector
#'
#' @description Match a vector of tips or tips and nodes with the an edge list from a \code{"phylo"} or \code{"multiPhylo"}.
#'
#' @param vector a vector of variables (equal to the number of tips or to the number of tips and nodes).
#' @param phylo a phylo or multiPhylo object.
#' @param replace.na optional, what to replace NAs with.
#' @param use.parsimony logical, whether to also colour internal edges parsimoniously (\code{TRUE} - default; i.e. if two nodes have the same unique ancestor node and the same variable, the ancestor node is assume to be the of the same value as its descendants) or not (\code{FALSE}).
#'  
#' @returns
#' A vector of variables equal to the number of edges in the tree (or a list of vectors if the \code{phylo} input is of class \code{"multiPhylo"}).
#' 
#' @examples
#' ## A random tree
#' tree <- rtree(20)
#' 
#' ## A random vector of two variables for each tips
#' tip_values <- sample(c("blue", "red"), 20, replace = TRUE)
#' 
#' ## Matching the colors (blue and red) to the tips descendants
#' edge_colors <- match.tip.edge(tip_values, tree, replace.na = "grey")
#'
#' ## Plotting the results
#' plot(tree, show.tip.label = FALSE, edge.color = edge_colors)
#' tiplabels(1:20, bg = tip_values)
#' 
#' ## Same but without assuming parsimony for the internal nodes
#' plot(tree, show.tip.label = FALSE,
#'      edge.color = match.tip.edge(tip_values, tree,
#'                                  use.parsimony = FALSE,
#'                                  replace.na = "grey"))
#' 
#' ## Matching the tips and nodes colors with the edges
#' node_values <- sample(c("blue", "red"), 19, replace = TRUE)
#' edge_colors <- match.tip.edge(c(tip_values, node_values), tree)
#' plot(tree, show.tip.label = FALSE, edge.color = edge_colors) 
#' tiplabels(1:20, bg = tip_values)
#' nodelabels(1:19, bg = node_values)
#' @author Thomas Guillerme
#' @export

## Matching edges and colours
match.tip.edge <- function(vector, phylo, replace.na, use.parsimony = TRUE) {

    match_call <- match.call()

    ## Sanitizing
    phylo_class <- check.class(phylo, c("phylo", "multiPhylo"))
    if(is(phylo, "multiPhylo")) {
        if(length(unique(Ntip(phylo))) != 1) {
            stop.call(msg.pre = "The trees from ", call = match_call$phylo, msg = " must have the same number of tips.")
        }
        ## Run the function on the list of trees
        return(lapply(phylo, function(tree, vector, replace.na, use.parsimony) match.tip.edge(vector, tree, replace.na, use.parsimony), vector, replace.na, use.parsimony))
    }
    check.class(vector, c("factor", "character", "numeric", "integer"))

    ## TODO: check number of nodes as well
    if(length(vector) != Ntip(phylo)[1]) {
        if(length(vector) != Ntip(phylo)[1]+Nnode(phylo)[1]) {
            stop(paste0("The input vector must of the same length as the number of tips (", Ntip(phylo)[1], ") or tips and nodes (", Ntip(phylo)[1]+Nnode(phylo)[1] ,") in phylo."))
        }
    }
    if(length(vector) == Ntip(phylo)[1]+Nnode(phylo)[1]) {
        ## Don't use parsimony if node info is available
        use.parsimony <- FALSE
    }

    ## Fill in the edges
    if(missing(replace.na)) {
        replace.na <- NA
    }
    edge_vector <- rep(replace.na, Nedge(phylo))

    ## Find the number of levels (groups/clades)
    groups <- unique(vector)

    ## Ignore the ones that are NAs
    if(is.na(replace.na)) {
        which_na <- is.na(groups)
    } else {
        which_na <- groups == replace.na
    }
    if(any(which_na)) {
        groups <- groups[!which_na]
    }

    ## Get the edge table
    edge_table <- phylo$edge

    ## Find the edges for each group
    for(group in 1:length(groups)) {
        ## Get the tips for the group
        tips <- which(vector == groups[group])
        ## Get the tip edges
        selected_edges <- which(edge_table[, 2] %in% tips)
        
        # # DEBUG
        # warning("DEBUG")
        # counter <- 0

        ## Recursively find any cherries
        if(use.parsimony) {
            focal_edges <- which(edge_table[, 2] %in% tips)
            while(any(duplicated(edge_table[focal_edges, 1]))) {
                
                # # DEBUG
                # warning("DEBUG")
                # counter <- counter + 1
                # print(counter)

                ## Find and cherries of the same group
                nodes <- edge_table[focal_edges, 1][which(duplicated(edge_table[focal_edges, 1]))]
                
                ## Update the group edges
                selected_edges <- c(selected_edges, which(edge_table[, 2] %in% nodes))

                ## Update the tips to check
                tips <- c(tips[!(tips %in% edge_table[which(edge_table[, 1] %in% nodes), 2])], nodes)

                ## Update the selected edges
                focal_edges <- which(edge_table[, 2] %in% tips)
            }
        }

        ## Replace the selected edges by the group value
        edge_vector[selected_edges] <- groups[group]
    }

    ## Done
    return(edge_vector)
}