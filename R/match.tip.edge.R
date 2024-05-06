#' @title Match tips or nodes edge vector
#'
#' @description Match a vector of tips or tips and nodes with the an edge list from a \code{"phylo"} or \code{"multiPhylo"}.
#'
#' @param vector a vector of variables (equal to the number of tips or to the number of tips and nodes) or a vector of tips and nodes names or IDs.
#' @param phylo a phylo or multiPhylo object.
#' @param replace.na optional, what to replace NAs with.
#' @param use.parsimony logical, whether to also colour internal edges parsimoniously (\code{TRUE} - default; i.e. if two nodes have the same unique ancestor node and the same variable, the ancestor node is assume to be the of the same value as its descendants) or not (\code{FALSE}).
#' @param to.root logical, if \code{vector} is a list of tips and nodes, whether to colour internal edges all the way to the root (\code{TRUE}) or not (\code{FALSE} - default).
#'  
#' @returns
#' If the input \code{vector} is a vector of variables, the function returns a vector of variables equal to the number of edges in the tree (or a list of vectors if the \code{phylo} input is of class \code{"multiPhylo"}). Else it returns an \code{integer} vector for the selected edges.
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
#'
#' ## Matching the tips and nodes colours to the root
#' data(bird.orders)
#'
#' ## Getting the bird orders starting with a "C"
#' some_orders <- sort(bird.orders$tip.label)[4:9]
#'
#' ## Get the edges linking these orders
#' edges_of_interest <- match.tip.edge(vector = some_orders,
#'                                     phylo  = bird.orders)
#'
#' ## Create a colour vector for all edges
#' all_edges <- rep("grey", Nedge(bird.orders))
#' ## Replacing the edges of interest by another colour
#' all_edges[edges_of_interest] <- "black"
#'
#' ## Plot the results
#' plot(bird.orders, edge.color = all_edges)
#'
#' @author Thomas Guillerme
#' @export

## Matching edges and colours
match.tip.edge <- function(vector, phylo, replace.na, use.parsimony = TRUE, to.root = FALSE) {

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

    ## Check the vector
    error_message <- paste0("The input vector must of the same length as the number of tips (", Ntip(phylo)[1], ") or tips and nodes (", Ntip(phylo)[1]+Nnode(phylo)[1] ,") in phylo. Or it must be a vector of node or tips IDs or names.")
    vector_is_id <- FALSE
    if(length(vector) != Ntip(phylo)[1]) {
        if(length(vector) != Ntip(phylo)[1]+Nnode(phylo)[1]) {
            ## Check if the vector is a vector of tips or nodes numbers.
            vector_class <- check.class(vector, c("character", "numeric", "integer"))
            if(vector_class %in% c("numeric", "integer")) {
                ## Check if can be tips and nodes
                if(any(vector > Ntip(phylo)+Nnode(phylo))) {
                    stop(error_message, call. = FALSE)
                } else {
                    vector_is_id <- TRUE
                }
            } else {
                if(vector_class %in% c("character")) {
                    if(any(!(vector %in% c(phylo$tip.label, phylo$node.label)))) {
                        stop(error_message, call. = FALSE)
                    } else {
                        vector_is_id <- TRUE
                        ## Convert into numerics
                        vector <- which(c(phylo$tip.label, phylo$node.label) %in% vector)
                    }
                } else {
                    stop(error_message, call. = FALSE)
                }
            }
        }
    }
    if(length(vector) == Ntip(phylo)[1]+Nnode(phylo)[1]) {
        ## Don't use parsimony if node info is available
        use.parsimony <- FALSE
    }
    check.class(to.root, "logical")
    check.class(use.parsimony, "logical")

    ## Get the edge table
    edge_table <- phylo$edge

    ## Colour the edges
    if(!vector_is_id) {
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
    }
    if(vector_is_id) {
        ## Get the vector of edges
        edge_vector <- rep(FALSE, Nedge(phylo))

        ## Get the mrca
        if(!to.root) {
            target_mrca <- getMRCA(phylo, vector)
        } else {
            ## mrca is the root of the tree
            target_mrca <- Ntip(phylo) + 1
        }

        ## Connect each tip to the mrca
        connect.tip.to.mrca <- function(tip, edge_table, target_mrca) {
            ## Store the edges values
            edges <- integer()
            ## Find the edge connecting to the tip
            tip_edge <- which(edge_table[,2] == tip)
            ## Save the edge
            edges <- c(edges, tip_edge)
            ## Loop through the edges until reaching the mrca_edges
            while(!(target_mrca %in% edge_table[tip_edge, ])) {
                ## Go down the tree
                tip <- edge_table[tip_edge, 1]
                tip_edge <- which(edge_table[,2] == tip)
                edges <- c(edges, tip_edge)
            }
            return(edges)
        }
        edge_vector <- unique(unlist(sapply(vector, connect.tip.to.mrca, edge_table, target_mrca)))
    }

    ## Done
    return(edge_vector)
}