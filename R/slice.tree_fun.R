#FUNCTIONS FOR slice.tree
## Get the node IDs
get.node.ID <- function(node, tree) {
    ## Check if it's a node
    is_node <- which(tree$node.label == node)
    ## Return the node or the tip
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
    return(as.numeric(labels_expand_match[!is.na(labels_expand_match)][1]))
}

## Get the branch length
get.branch.length <- function(node1, node2, tree, full_tree) {

    ## Get the node IDs
    node1_ID <- get.node.ID(node1, tree)
    node2_ID <- get.node.ID(node2, tree)

    ## Check node IDs (if NAs)
    node1_ID <- ifelse(is.na(node1_ID), get.node.ID.expand(node1, tree, full_tree), node1_ID)
    node2_ID <- ifelse(is.na(node2_ID), get.node.ID.expand(node2, tree, full_tree), node2_ID)

    ## Return branch length
    return(tree$edge.length[which(tree$edge[,1] == node1_ID & tree$edge[,2] == node2_ID)])
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
    #Returning the name of the offspring node
    if(offspring.edge > Ntip_tree) {
        offspring_node <- tree$node.label[offspring.edge-Ntip_tree]
    } else {
        offspring_node <- tree$tip.label[offspring.edge]
    }
    return(offspring_node)
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
            #Compute the node ages
            age_tree <- tree.age(tree)
            age_slic <- tree.age(tree_slice)
            #select the oldest node in tree_slice
            root <- age_slic$elements[which(age_slic$age == max(age_slic$age))]
            #calculate the slice age using the oldest node in tree_slice
            age <- age_tree[which(as.character(age_tree$elements) == as.character(root)),1] - age_slic[which(as.character(age_slic$elements) == as.character(root)),1]
            #extract the age of the offspring node
            off_nod_age <- age_tree[which(as.character(age_tree$elements) == as.character(offspring_node)),1]
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
            return(ifelse(full_edge < slice_edge/2, DEL_node, ACC_node))
        }
    }
}
