## Extracting a specific rarefaction level
extract.disparity.values <- function(subsets, data, rarefaction, concatenate) {
    ## Get the rarefaction level
    if(rarefaction != FALSE) {
        rarefaction = as.numeric(which(lapply(data$subsets[[subsets]][-1], nrow) == rarefaction) + 1)
        if(length(rarefaction) == 0) {
            ## No rarefaction level for this subset
            return(NULL)
        }
    } else {
        rarefaction = 2
    }
    if(concatenate) {
        return(list(as.numeric(data$disparity[[subsets]][[rarefaction]])))
    } else {
        return(lapply(seq_len(ncol(data$disparity[[subsets]][[rarefaction]])), function(col) data$disparity[[subsets]][[rarefaction]][,col]))
    }
}

## Remove nulls from a list
clean.list <- function(list) {
    nulls <- unlist(lapply(list, is.null))
    return(list[!nulls])
}

## Recursive sorting
recursive.sort <- function(data, sort) {
    return(data[sort])
}

## Merging two subsets
merge.two.subsets <- function(subs1, subs2, data) {
    ## Get the list of new subsets
    new_subset <- list("elements" = matrix(unique(c(data$subsets[[subs1]]$elements, data$subsets[[subs2]]$elements, ncol = 1))))
    ## Replace the second subset with the new one
    data$subsets[[subs2]] <- new_subset
    ## Rename it
    names(data$subsets)[subs2] <- paste(names(data$subsets)[subs1], names(data$subsets)[subs2], sep = "-") 
    ## Remove the former
    data$subsets[[subs1]] <- NULL
    return(data)
}

## Check subset availability
check.subsets <- function(subsets, data) {

    if(!is.null(data$call$disparity) && data$call$disparity$metrics$between.groups) {

        ## Numeric subsets
        if(is(subsets, "numeric") || is(subsets, "integer")) {
            if(any(na_subsets <- is.na(match(subsets, 1:length(data$disparity))))) {
                ## Subsets not found
                stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."))
            }
        } else {
            if(is(subsets, "character")) {
                ## Get the subset names (searched and available)
                subset_search <- unique(unlist(strsplit(subsets, split = ":")))
                subset_available <- unique(unlist(strsplit(names(data$disparity), split = ":")))

                ## Check if the searched ones exist
                if(any(na_subsets <- is.na(match(subset_search, subset_available)))) {
                    ## Subsets not found
                    stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."))
                }
            } else {
                stop("subsets argument must be of class \"numeric\" or \"character\".")
            }
        }

    } else {
        
        if(is(subsets, "list")) {
            ## Flatten the list for checking for subsets
            subsets <- unique(unlist(subsets))
        }

        if(length(subsets) > length(data$subsets)) {
            stop("Not enough subsets in the original data.")
        } else {
            if(is(subsets, "numeric") || is(subsets, "integer")) {
                if(any(na_subsets <- is.na(match(subsets, 1:length(data$subsets))))) {
                    ## Subsets not found
                    stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."))
                }
            } else {
                if(is(subsets, "character")) {
                    if(any(is.na(match(subsets, names(data$subsets))))) {

                        subsets <- subsets[which(is.na(match(subsets, names(data$subsets))))]
                        orthograph <- ifelse(length(subsets) == 1, "Subset ", "Subsets ")
                        stop(paste0(orthograph, paste0(subsets, collapse = ", "), " not found."))

                    }
                } else {
                    stop("subsets argument must be of class \"numeric\" or \"character\".")
                }
            }
        }
    }
}

## Detecting the bin age lower or greater than a value
detect.bin.age <- function(data, value, greater = FALSE) {
    ## Detect the bin before the extinction time
    bin_times <- unlist(sapply(names(data$subsets), strsplit, split = " - ", simplify = FALSE), recursive = FALSE)

    ## Detecting the bin age
    detect.bin.ages.lapply <- function(one_bin, value, greater) {
        if(greater) {
            return(ifelse(as.numeric(one_bin)[2] >= value, TRUE, FALSE))
        } else {
            return(ifelse(as.numeric(one_bin)[1] <= value, TRUE, FALSE))
        }
    }

    return(unlist(lapply(bin_times, detect.bin.ages.lapply, value, greater)))
}

## Adding dimnames (if necessary)
add.dimnames <- function(one_output, one_subset, data) {
    input <- data$matrix[[1]][data$subsets[[one_subset]]$elements, data$call$dimensions]
    return(
        switch(as.character(sum(which(dim(input) %in% length(one_output)))),
                ## No matching dim
                "0" = one_output,
                ## Matching rows
                "1" = {names(one_output) <- rownames(input); one_output},
                ## Matching cols
                "2" = {names(one_output) <- colnames(input); one_output},
                ## Matching both (use rows as default)
                "3" = {names(one_output) <- rownames(input); one_output}
            )
        )
}

## Detect the edges containing the requested elements
detect.edges <- function(tree, elements, to.root) {
    ## Detect which edges to keep
    selected_edges <- match(elements, tree$edge[, 2])
    ## Has an NA (grabbed the root)
    if(any(to_drop <- is.na(selected_edges))) {
        ## Drop the root
        selected_edges <- selected_edges[!to_drop]
        ## Manually set the root
        root <- Ntip(tree) + 1
    } else {
        if(to.root) {
            ## Manually set the root
            root <- Ntip(tree) + 1
        } else {
            ## Get the local root
            root <- getMRCA(tree, elements)
        }
    }

    ## Get the nodes to test if all edges connect
    test_nodes <- tree$edge[selected_edges, 1]
    ## Remove the root
    if(root %in% test_nodes) {
        test_nodes <- test_nodes[-c(which(test_nodes == root))] 
    }
    ## Check each node
    while(length(test_nodes) != 0) {
        if(test_nodes[1] %in% tree$edge[selected_edges, 2]) {
            ## Node is OK
            test_nodes <- test_nodes[-1]
        } else {
            ## Node is not connected
            selected_edges <- c(selected_edges, which(tree$edge[,2] == test_nodes[1]))
            ## Get the nodes to test if all edges connect
            test_nodes <- tree$edge[selected_edges, 1]
            ## Remove the root
            if(root %in% test_nodes) {
                test_nodes <- test_nodes[-c(which(test_nodes == root))] 
            }
        }
    }
    return(selected_edges)
}

## Get the tree containing requested elements
get.new.tree <- function(elements, tree, to.root) {

    ## Detect which edges to keep
    new_edges <- unique(detect.edges(tree, elements, to.root))
    if(length(new_edges) < 2) {
        return(NULL)
    }

    ## Tracking the new tree
    tree_track <- as.data.frame(tree$edge)
    tree_track <- cbind(tree_track, is.tip = tree_track[,2] <= Ntip(tree))
    tree_track <- cbind(tree_track, label = c(tree$tip.label,tree$node.label)[tree_track[,2]])
    tree_track <- cbind(tree_track, selected = 1:nrow(tree_track) %in% new_edges)
    
    ## Get the root node name
    root_lab <- tree$node.label[min(tree_track[tree_track$selected, 1])-Ntip(tree)]

    ## Build the new tree
    new_tree <- list(edge        = matrix(unlist(tree_track[tree_track$selected, c(1,2)]), ncol = 2),
                     edge.length = tree$edge.length[new_edges])

    ## Update tips (some nodes become tips)
    new_tips <- c(tree$tip.label,tree$node.label)[tree$edge[new_edges, 2][!(tree$edge[new_edges, 2] %in% tree$edge[new_edges ,1])]]
    tree_track$is.tip[tree_track$label %in% new_tips] <- TRUE

    ## Convert the edge numbers
    new_edge_table <- tree_track[tree_track$selected, ]

    ## Sort the node values
    all_nodes <- c(new_edge_table[, 1], new_edge_table[!new_edge_table$is.tip, 2])
    all_nodes <- match(all_nodes, unique(all_nodes)) + sum(new_edge_table$is.tip)

    ## Update the table nodes
    new_edge_table[, 1] <- all_nodes[1:nrow(new_edge_table)]
    new_edge_table[!new_edge_table$is.tip, 2] <- all_nodes[-c(1:nrow(new_edge_table))]

    ## Update the table tips
    new_edge_table[new_edge_table$is.tip, 2] <- 1:sum(new_edge_table$is.tip)

    ## Update the edge table
    new_tree$edge <- matrix(unlist(new_edge_table[, c(1,2)]), ncol = 2)
    ## Update the Nnodes
    new_tree$Nnode <- sum(!new_edge_table$is.tip) + 1 #+1 is for the root
    ## Update the tip labels
    new_tree$tip.label <- new_edge_table$label[new_edge_table$is.tip]
    ## Update the node labels
    new_tree$node.label <- c(root_lab, new_edge_table$label[!new_edge_table$is.tip])

    ## Update root.time
    if(!is.null(tree$root.time)) {
        if(to.root) {
            new_tree$root.time <- tree$root.time
        } else {
            ages <- tree.age(tree)
            ## Not necessary the first node label?
            new_tree$root.time <- ages$ages[which(ages$elements == new_tree$node.label[1])]
        }
    }

    class(new_tree) <- "phylo"
    return(new_tree)
}

## Return a subseted tree
get.tree.subset <- function(one_subset, data, to.root) {

    ## One input tree
    if(length(data$tree) == 1) {
        output <- apply(one_subset, 2, function(x, tree, to.root) get.new.tree(elements = x, tree = tree, to.root = to.root), tree = data$tree[[1]], to.root = to.root)
        if(is(output, "list")) {
            class(output) <- "multiPhylo"
            return(output)
        } else {
            return(output)
        }
    } else {
        ## Multiple trees
        stop("TODO")
    }
}


