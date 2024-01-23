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
                stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."), call. = FALSE)
            }
        } else {
            if(is(subsets, "character")) {
                ## Get the subset names (searched and available)
                subset_search <- unique(unlist(strsplit(subsets, split = ":")))
                subset_available <- unique(unlist(strsplit(names(data$disparity), split = ":")))

                ## Check if the searched ones exist
                if(any(na_subsets <- is.na(match(subset_search, subset_available)))) {
                    ## Subsets not found
                    stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."), call. = FALSE)
                }
            } else {
                stop("subsets argument must be of class \"numeric\" or \"character\".", call. = FALSE)
            }
        }

    } else {
        
        if(is(subsets, "list")) {
            ## Flatten the list for checking for subsets
            subsets <- unique(unlist(subsets))
        }

        if(length(subsets) > length(data$subsets)) {
            stop("Not enough subsets in the original data.", call. = FALSE)
        } else {
            if(is(subsets, "numeric") || is(subsets, "integer")) {
                if(any(na_subsets <- is.na(match(subsets, 1:length(data$subsets))))) {
                    ## Subsets not found
                    stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."), call. = FALSE)
                }
            } else {
                if(is(subsets, "character")) {
                    if(any(is.na(match(subsets, names(data$subsets))))) {

                        subsets <- subsets[which(is.na(match(subsets, names(data$subsets))))]
                        orthograph <- ifelse(length(subsets) == 1, "Subset ", "Subsets ")
                        stop(paste0(orthograph, paste0(subsets, collapse = ", "), " not found."), call. = FALSE)

                    }
                } else {
                    stop("subsets argument must be of class \"numeric\" or \"character\".", call. = FALSE)
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

## Toggle an output to phylo or multiphylo
toggle.multiphylo.list <- function(x) {
    if(is(x, "list")) {
        ## Check if the elements are phylo
        elements_class <- unlist(lapply(x, class))
        if(length(elements_class) == 1 && elements_class == "phylo") {
            return(x[[1]])
        } else {
            if(length(all_elems <- unique(elements_class)) == 1) {
                if(all_elems == "phylo") {
                    x <- "multiPhylo"
                    return(x)
                }
            }
        }
    }
    return(x)
}

## Return a subseted tree
get.one.tree.subset <- function(one_subset, one_tree, to.root) {
    ## Normal behaviour
    output <- lapply(one_subset, function(x, tree, to.root) apply(x, 2, function(x, tree, to.root) get.new.tree(elements = x, tree = tree, to.root = to.root), tree = tree, to.root = to.root), tree = one_tree, to.root = to.root)
    ## Change the output objects
    output <- lapply(output, toggle.multiphylo.list)

    return(output)
}


## Slide nodes from the root
#@param bin_age the ages of the bin limits
#@param tree the original tree
#@return a tree with all old nodes slided
slide.node.root <- function(bin_age, tree) {
    ## Get the age to slide the nodes to
    time <- bin_age[1]
    ## Get all ages
    tree_ages <- tree.age(tree)
    ## Get the yongest age for late
    younger <- min(tree_ages$ages)
    ## Remove the older tips
    to_drop <- tree_ages$elements[which(tree_ages[tree_ages$elements %in% tree$tip.label, ]$ages > time)]
    tree <- drop.tip(tree, tip = to_drop)
    ## Update the root time
    new_ages <- tree.age(tree)$ages
    tree$root.time <- max(new_ages) + (younger - min(new_ages))

    # warning("DEBUG slide.node.root")
    # plot(tree, main = "dropped old tips") ; nodelabels(); axisPhylo()
    # abline(v = 3-c(2, 1), col = "red", lty = 1)

    ## Recalculate the ages for the nodes
    node_ages <- tree.age(tree)
    younger <- min(node_ages$ages)
    node_ages <- node_ages[!(node_ages$elements %in% tree$tip.label), ]
    ## Get the nodes that are older than the time
    nodes_to_slide <- which(node_ages$ages > time)

    ## reorder the nodes to slide by age
    nodes_to_slide <- nodes_to_slide[match(sort(node_ages$ages[nodes_to_slide]), node_ages$ages[nodes_to_slide])]

    ## Recursively slide all nodes
    while(length(nodes_to_slide) > 0) {
        ## Get the node name and sliding value
        sliding_value <- node_ages$ages[nodes_to_slide[1]] - time
        node_name <- node_ages$elements[nodes_to_slide[1]]
        ## slide it!
        tree <- slide.nodes(node_name, tree, slide = sliding_value, allow.negative.root = TRUE)
        ## Update the root time
        new_ages <- tree.age(tree)$ages
        tree$root.time <- max(new_ages) + (younger - min(new_ages))

        ## Update the list of nodes to slide
        nodes_to_slide <- nodes_to_slide[-1]

        # warning("DEBUG slide.node.root")
        # plot(tree, main = "slid one node") ; nodelabels(); axisPhylo()
        # abline(v = 3-c(2, 1), col = "red", lty = 1)
    }
    return(tree)
}

## Get the subset of trees for one tree in an interval
get.interval.subtrees <- function(one_tree, bin_ages, to.root) {
    ## Slice the right sides of the trees
    slice.one.tree <- function(age, tree) {
        slice.tree(tree, age[2], model = "acctran", keep.all.ancestors = TRUE)
    }
    subset_subtrees <- lapply(bin_ages, slice.one.tree, one_tree) # TODO need fix for multiphylo

    if(!to.root) {
        ## Compressing the root of the tree up until the slice lower boundary
        subset_subtrees <- mapply(slide.node.root, bin_ages, subset_subtrees, SIMPLIFY = FALSE)
    }

    ## Name the subsets
    names(subset_subtrees) <- names(bin_ages)
    return(subset_subtrees)
}

## Get the trees from slices
get.slice.subsets <- function(one_subset, data, to.root) {
    ## ONLY FOR ELEMENTS FOR NOW
    subset <- one_subset$elements

    ## Handeling split slices
    if(!is.null(data$call$subsets[[2]]) && length(grep("split", data$call$subsets[[2]])) > 0) {
        ## Sample the probabilities and collapse the matrix
        sample.x <- function(x) {sample(x[1:2], 1, prob = c(x[3], 1-x[3]))}
        sampled_subset <- matrix(apply(subset[, 1:3, drop = FALSE], 1, sample.x), ncol = 1)
        ## remove the sampled
        subset <- subset[, -c(1:3), drop = FALSE]
        ## Sample for multiple trees
        while(ncol(subset) > 0) {
            sampled_subset <- cbind(sampled_subset, apply(subset[, 1:3, drop = FALSE], 1, sample.x))
            subset <- subset[, -c(1:3), drop = FALSE]
        }
        subset <- sampled_subset
    }

    trees_list <- data$tree
    trees_out <- list()
    while(ncol(subset) > 0) {
        ## Extract the elements for the tree recursively
        new_tree <- get.new.tree(subset[,1], trees_list[[1]], to.root)
        if(is.null(new_tree)) {
            new_tree <- list(NULL)
        }
        trees_out[[length(trees_out) + 1]] <- new_tree
        ## Remove the tree and the subset row
        subset <- subset[, -1, drop = FALSE]
        trees_list[1] <- NULL
    }
    ## Handle the tree output
    if(length(trees_out) > 1) {
        class(trees_out) <- "multiPhylo"
        return(trees_out)
    } else {
        return(trees_out[[1]])
    }
}