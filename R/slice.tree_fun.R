#FUNCTIONS FOR slice.tree

## This function is modified from Dave Bapst paleotree::timeSliceTree (2019/06/19)
## (returns null when failure)
slice.tree.sharp <- function(tree, slice)  {

    ## Get slice time
    slice_time <- tree$root.time - slice
    ## Get node ages
    node_age <- node.depth.edgelength(tree)
    ## Which ancestor nodes/edges cross the slice
    cross_edge <- which((node_age[ tree$edge[, 1] ] < slice_time) & (node_age[tree$edge[, 2] ] >= slice_time))
    ## If no edge is crossed, return null
    if(length(cross_edge) == 0) {
        return(NULL)
    }

    ## Crossing edges function
    get.crossings <- function(one_edge, bipartitions, tree) {
        ## Declaring variables
        n_tips <- Ntip(tree)
        ## Getting descendent
        descendent <- tree$edge[one_edge,2]
        if(descendent > n_tips){
            ## if an internal edge that goes past the tslice
            descendent_tip <- bipartitions[[descendent-n_tips]]
            ## Drop all but one tip
            return(descendent_tip[-1])
        } else {
            return(NA)
        }
    }
    ## Get the bipartitions
    bipartitions <- prop.part(tree)
    ## Get the crossings on each edges
    tips_to_drop <- unlist(sapply(cross_edge, get.crossings, bipartitions, tree, simplify = FALSE))
    tips_to_drop <- na.omit(tips_to_drop)

    ## Drop tips from tree
    tree_sliced <- drop.tip(tree, tips_to_drop)

    ## Recalculate the tree depth
    node_age_sliced <- node.depth.edgelength(tree_sliced)
    ## Find edges crossing the slice
    edges_crossing <- (node_age_sliced[tree_sliced$edge[, 2]] >= slice_time)
    node_sliced_depth <- node_age_sliced[tree_sliced$edge[edges_crossing, 1]]
    tree_sliced$edge.length[edges_crossing] <- slice_time - node_sliced_depth
    tree_sliced$root.time <- tree$root.time
    ## Get the node tips depth
    n_tips_sliced <- Ntip(tree_sliced)
    tips_depth <- dist.nodes(tree_sliced)[n_tips_sliced + 1, 1:n_tips_sliced]
    ## Find tips that do not have the slice age
    #slice_age <- max(tips_depth)
    tips_at_slice <- (tips_depth == slice_time)
    if(length(which(tips_at_slice)) < 2) {
        ## Return NULL if less than two tips are present
        return(NULL)
    } else {
        ## Return the ultrametric tree at the slice
        return(drop.tip(tree_sliced, tip = which(!tips_at_slice)))
    }
}

## Get the node IDs
get.node.ID <- function(node, tree) {
    ## Check if it's a node
    is_node <- which(tree$node.label == node)
    ## Return the node or the tip
    return(ifelse(length(is_node) > 0, is_node + Ntip(tree), which(tree$tip.label == node)))
}

## Get the node ID out of a tree (i.e. from a parent tree)
get.node.ID.expand <- function(node, tree, full_tree) {    
    ## Get the expanded tree
    tree_expand <- extract.clade(full_tree, node = node)
    ## Expanded labels
    labels_expand <- c(tree_expand$tip.label, tree_expand$node.label)
    ## Get all the node IDs matching with the expanded labels
    labels_expand_match <- sapply(labels_expand, get.node.ID, tree = tree)
    ## Get the first match
    match <- as.numeric(labels_expand_match[!is.na(labels_expand_match)][1])
    return(match)
}

## Get the branch length
get.branch.length <- function(node1, node2, tree, full_tree) {

    ## Get the node IDs
    node1_ID <- get.node.ID(node1, tree)
    node2_ID <- get.node.ID(node2, tree)

    ## Check the descendant node IDs (if NAs)
    node2_ID <- ifelse(is.na(node2_ID), get.node.ID.expand(node2, tree, full_tree), node2_ID)

    if(!is.na(node1_ID)) {
        ## Return branch length
        return(tree$edge.length[which(tree$edge[,1] == node1_ID & tree$edge[,2] == node2_ID)])   
    } else {
        ## Calculated branch length from not included nodes

        ## Get it's ID in the full tree
        no1_fulID <- get.node.ID(node1, full_tree)

        ## Get it's ancestor
        ancestor <- slice.tree_parent.node(full_tree, node1)
        ## Get it's ancestor ID in the subtree
        anc_subID <- get.node.ID(ancestor[1], tree)

        ## If ancestor still not found in sub_tree, continue looping and record the ancestors
        while(is.na(anc_subID)) {
            ancestor <- c(slice.tree_parent.node(full_tree, ancestor[1]), ancestor)
            anc_subID <- get.node.ID(ancestor[1], tree)
        }

        ## Get the distance between the slice and the closest ancestor (in the subtree)
        distance1 <- tree$edge.length[which(tree$edge[,1] == anc_subID & tree$edge[,2] == node2_ID)]

        ## Get it's ID in the full tree
        anc_fulID <- sapply(ancestor, get.node.ID, full_tree)
        ## Adding the real ancestor
        anc_fulID <- as.numeric(c(anc_fulID, no1_fulID))

        ## Loop through the nodes to calculate distance2 (the distance between the closest ancestor and the real ancestor in the full tree)
        distance2 <- 0
        for(node in 1:(length(anc_fulID)-1)) {
            distance2 <- distance2 + full_tree$edge.length[which(full_tree$edge[,1] == anc_fulID[node] & full_tree$edge[,2] == anc_fulID[node +1])]
        }

        ## Calculate the real distance from node to slice
        return(distance1 - distance2)
    }
}

#Select the parent node of a tip
slice.tree_parent.node <- function(tree, tip) {
    #Selecting parent edge in the full tree
    parent_edge <- tree$edge[which(tree$edge[,2] == which(tip == c(tree$tip.label, tree$node.label))), 1]
    #Selecting parent node in the full tree
    parent_node <- tree$node.label[parent_edge-Ntip(tree)]
    #error if not working
    if (length(parent_node) != 1) {
        stop('No parent node found!')
    }
    return(parent_node)
}


#Select the offspring node/tip of a node towards a tip
slice.tree_offspring.node <- function(tree, parent_node, tip) {
    #Ntip_tree variable declaration
    Ntip_tree <- Ntip(tree)

    #Stop if parent node is the same as tip
    if(parent_node == tip) {
        stop('Parent node is a tip!')
        # return(NULL)
    }
    #Extracting the subtrees connected to the parent node
    offsprings <- tree$edge[which(tree$edge[,1] == (which(parent_node == tree$node.label)+Ntip_tree)), 2]
    #Testing which subtree contains tip
    for (node in 1:length(offsprings)) {
        #Check if the "node" is a node or a tip
        if(offsprings[node] > Ntip_tree) {
            subtree <- extract.clade(tree, offsprings[node])
            if(tip %in% subtree$tip.label) {
            # if(length(grep(tip, subtree$tip.label)) == 1) {
                offspring.edge <- offsprings[node]
            }
        } else {
            subtree <- tree$tip.label[offsprings[node]]
            if(tip %in% subtree) {
            # if(length(grep(tip, subtree)) == 1) {
                offspring.edge <- offsprings[node]
            }
        }
    }

    #Return null if fails
    # if(is.na(offspring.edge)) {
    #     return(NULL)
    # }


    #Returning the name of the offspring node
    if(offspring.edge > Ntip_tree) {
        offspring_node <- tree$node.label[offspring.edge-Ntip_tree]
    } else {
        offspring_node <- tree$tip.label[offspring.edge]
    }
    return(offspring_node)
}

## Getting number of decimals for precision
num.decimals <- function(x) {
    x <- sub("0+$","",x)
    x <- sub("^.+[.]","",x)
    return(nchar(x))
}


#Modify the tree slicing by replacing the tips that are not at the cut by the parent node
slice.tree_DELTRAN <- function(tree, tip, tree_slice) {
    parent_node <- slice.tree_parent.node(tree, tip)
    while(match(parent_node, tree_slice$node.label, nomatch=FALSE) == 0) {
        #Repeat if slice.tree_parent.node is not present in the sliced tree
        parent_node <- slice.tree_parent.node(tree, parent_node)
    }

    #Test if there is another node between the MRCA (parent_node) and tip
    try(offspring_node <- slice.tree_offspring.node(tree, parent_node, tip), silent = TRUE)
    while(exists("offspring_node")) {

        if(offspring_node == tip) {
            ## The offspring is just the tip
            return(parent_node)
        }

        #Compute the node ages
        age_tree <- tree.age(tree)
        age_slic <- tree.age(tree_slice, fossil = FALSE)
        #select the oldest node in tree_slice
        root <- age_slic$elements[which(age_slic$age == max(age_slic$age))][1]
        #calculate the slice age using the oldest node in tree_slice
        age <- age_tree[which(as.character(age_tree$elements) == as.character(root)),1] - age_slic[which(as.character(age_slic$elements) == as.character(root)),1]
        #extract the age of the offspring node
        off_nod_age <- age_tree[which(as.character(age_tree$elements) == as.character(offspring_node)),1]
        
        precision <- num.decimals(c(off_nod_age, age))
        ## If precision are different and not equal to at least 1
        #if(length(precision) > 1 && sort(precision)[1] != 1) {
        ## Round the node ages
        num_digits <- ifelse(precision[1] < precision[2], precision[1], precision[2])
        off_nod_age <- round(off_nod_age, digits = num_digits)
        age <- round(age, digits = num_digits)
        #}
        
        if(off_nod_age > age) {
            parent_node <- offspring_node
            remove(offspring_node)
            try(offspring_node <- slice.tree_offspring.node(tree, parent_node, tip), silent = TRUE)
        } else {
            remove(offspring_node)
        }
    }
    return(parent_node)
}

#Modify the tree slicing by replacing the tips that are not at the cut by the offspring node towards the tip
slice.tree_ACCTRAN <- function(tree, tip, tree_slice) {
    parent_node <- slice.tree_DELTRAN(tree, tip, tree_slice)
    offspring_node <- slice.tree_offspring.node(tree, parent_node, tip)
    return(offspring_node)
}

#Modify the tree slicing by replacing the tips that are not at the cut by the offspring node towards the tip
slice.tree_PROXIMITY <- function(tree, tip, tree_slice, probability = FALSE) {
    #Calculating both the DELTRAN and the ACCTRAN node
    DEL_node <- slice.tree_DELTRAN(tree, tip, tree_slice)
    ACC_node <- slice.tree_ACCTRAN(tree, tip, tree_slice)

    #If both nodes are the same (i.e slicing through the actual species), just return one
    if(DEL_node == ACC_node) {
        if(probability) {
            return(c(DEL_node, ACC_node, "1"))
        } else {
            return(DEL_node)
        }
    } else {
        ## Get branch lengths
        full_edge <- get.branch.length(DEL_node, ACC_node, tree, full_tree = tree)
        slice_edge <- get.branch.length(DEL_node, ACC_node, tree_slice, full_tree = tree)


        if(probability) {
            prob <- 1-slice_edge/full_edge
            prob <- as.character(round(prob, digits = 10))
            return(c(DEL_node, ACC_node, prob))
        } else {
            return(ifelse(slice_edge < full_edge/2, DEL_node, ACC_node))
        }
    }
}


## Slicing through a single edge
slice.edge <- function(tree, age, model) {
    ## Get the edges length (depth)
    edges_depth <- node.depth.edgelength(tree)

    ## Correct with the root.age
    edges_depth <- max(edges_depth)-edges_depth
    if(!is.null(tree$root.time)) {
        edges_depth <- edges_depth + abs(max(edges_depth) - tree$root.time)
    }
    
    ## Find which edge gets sliced
    edge_slice <- tree$tip.label[which(edges_depth < age)]
    edge_slice <- edge_slice[!is.na(edge_slice)]

    ## Get the upper node (after the slice)
    upper_node <- getMRCA(tree, tip = edge_slice)
    if(is.null(upper_node)) {
        upper_node_name <- edge_slice
        upper_node <- which(tree$tip.label == edge_slice)
    } else {
        upper_node_name <- tree$node.label[upper_node-Ntip(tree)]
    }
    ## Get the lower node name
    lower_node_name <- slice.tree_parent.node(tree, upper_node_name)

    if(any(model %in% c("equal.split", "gradual.split", "proximity"))) {
        
        ## Get the lower node ID
        lower_node <- which(tree$node.label == lower_node_name) + Ntip(tree)

        ## Get the total edge length
        full_edge <- edges_depth[lower_node] - edges_depth[upper_node]

        ## Get the slice edge
        slice_edge <- edges_depth[lower_node] - age

        if(model != "proximity") {
            prob <- 1-slice_edge/full_edge
            prob <- as.character(round(prob, digits = 10))
            if(model == "gradual.split") {
                return(c(lower_node_name, upper_node_name, prob))
            } else {
                return(c(lower_node_name, upper_node_name, "0.5"))
            }
        } else {
            return(ifelse(slice_edge < full_edge/2, lower_node_name, upper_node_name))
        }

    } else {

        switch(model,
            random = {
                return(sample(c(upper_node_name, lower_node_name), 1))
            },
            acctran = {
                return(upper_node_name)
            },
            deltran = {
                return(lower_node_name)
            }
        )
    }
}