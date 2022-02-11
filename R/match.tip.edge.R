#' @title Match tips edge vector
#'
#' @description Match a vector of tips with the an edge list
#'
#' @param vector a vector of variables (equal to the number of tips).
#' @param phylo a phylo object.
#' @param replace.na optional, what to replace NAs with.
#'  
#' @returns
#' A vector of variables equal to the number of edges in the tree
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
#' 
#' 
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

## Matching edges and colours
match.tip.edge <- function(vector, phylo, replace.na) {

    ## Sanitizing
    check.class(phylo, "phylo")
    check.class(vector, c("factor", "character", "numeric", "integer"))
    if(length(vector) != Ntip(phylo)) {
        stop(paste0("The input vector must of the same length as the number of tips in phylo (", Ntip(phylo), ")."))
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

        ## Replace the selected edges by the group value
        edge_vector[selected_edges] <- groups[group]
    }

    ## Done
    return(edge_vector)
}