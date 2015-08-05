#FUNCTIONS FOR slice.tree

#Select the parent node of a tip
slice.tree_parent.node<-function(tree, tip) {
    #Selecting parent edge in the full tree
    parent.edge<-tree$edge[which(tree$edge[,2] == grep(tip, c(tree$tip.label, tree$node.label))[1]), 1]
    #Selecting parent node in the full tree
    parent_node<-tree$node.label[parent.edge-Ntip(tree)]
    #error if not working
    if (length(parent_node) != 1) {
        stop('No parent node found!')
    }
    return(parent_node)
}

#Select the offspring node/tip of a node towards a tip
slice.tree_offspring.node<-function(tree, parent_node, tip) {
    #Stop if parent node is the same as tip
    if(parent_node == tip) {
        stop('Parent node is a tip!')
    }
    #Extracting the subtrees connected to the parent node
    offsprings<-tree$edge[which(tree$edge[,1] == (match(parent_node, tree$node.label)+Ntip(tree))), 2]
    #Testing which subtree contains tip
    for (node in 1:length(offsprings)) {
        #Check if the "node" is a node or a tip
        if(offsprings[node] > Ntip(tree)) {
            subtree<-extract.clade(tree, offsprings[node])
            if(length(grep(tip, subtree$tip.label))==1) {
                offspring.edge<-offsprings[node]
            }
        } else {
            subtree<-tree$tip.label[offsprings[node]]
            if(length(grep(tip, subtree))==1) {
                offspring.edge<-offsprings[node]
            }
        }
    }
    #Returning the name of the offspring node
    if(offspring.edge > Ntip(tree)) {
        offspring_node<-tree$node.label[offspring.edge-Ntip(tree)]
    } else {
        offspring_node<-tree$tip.label[offspring.edge]
    }
    return(offspring_node)
}


#Modify the tree slicing by replacing the tips that are not at the cut by the parent node
slice.tree_DELTRAN<-function(tree, tip, tree_slice) {
    parent_node<-slice.tree_parent.node(tree, tip)
    while(match(parent_node, tree_slice$node.label, nomatch=FALSE) == 0) {
        #Repeat if slice.tree_parent.node is not present in the sliced tree
        parent_node<-slice.tree_parent.node(tree, parent_node)
    }

    #Test if there is another node between the MRCA (parent_node) and tip
        try(offspring_node<-slice.tree_offspring.node(tree, parent_node, tip), silent=TRUE)
        while(exists("offspring_node")) {
            #Compute the node ages
            age_tree<-tree.age(tree)
            age_slic<-tree.age(tree_slice)
            #select the oldest node in tree_slice
            root<-age_slic$edges[which(age_slic$age == max(age_slic$age))]
            #calculate the slice age using the oldest node in tree_slice
            age=age_tree[which(as.character(age_tree$edges) == as.character(root)),1] - age_slic[which(as.character(age_slic$edges) == as.character(root)),1]
            #extract the age of the offspring node
            off_nod_age<-age_tree[which(age_tree$edge == offspring_node),1]
            if(off_nod_age > age) {
                parent_node<-offspring_node
                remove(offspring_node)
                try(offspring_node<-slice.tree_offspring.node(tree, parent_node, tip), silent=TRUE)
            } else {
                remove(offspring_node)
            }
        }
    return(parent_node)
}

#Modify the tree slicing by replacing the tips that are not at the cut by the offspring node towards the tip
slice.tree_ACCTRAN<-function(tree, tip, tree_slice) {
    parent_node<-slice.tree_DELTRAN(tree, tip, tree_slice)
    offspring_node<-slice.tree_offspring.node(tree, parent_node, tip)
    return(offspring_node)
}

#Modify the tree slicing by replacing the tips that are not at the cut by the offspring node towards the tip
slice.tree_PROXIMITY<-function(tree, tip, tree_slice) {
    #Calculating both the DELTRAN and the ACCTRAN node
    DEL_node<-slice.tree_DELTRAN(tree, tip, tree_slice)
    ACC_node<-slice.tree_ACCTRAN(tree, tip, tree_slice)

    #If both nodes are the same (i.e slicing through the actual species), just return one
    if(DEL_node == ACC_node) {
        return(DEL_node)
    } else {
        #Extract the distance between DEL_node and ACC_node on tree
        #Creating the two sub clades
        del_tree<-extract.clade(tree, DEL_node)
        if(any(tree$tip.label == ACC_node)) {
            #Subclade irrelevant if ACC_node is a tip
            ACC_tip<-TRUE
            acc_tree<-list() ; acc_tree$tip.label<-ACC_node
        } else {
            ACC_tip<-FALSE
            acc_tree<-extract.clade(tree, ACC_node)
        }
        #Tips to drop (-1)
        if(Ntip(del_tree) > 3) {
            drop<-del_tree$tip.label[which(is.na(match(del_tree$tip.label, acc_tree$tip.label)))]
            #keep one species
            drop<-drop[-1]
            del_tree<-drop.tip(del_tree, drop)
        }

        #Selecting the DEL_edge (root in del_tree) and the ACC_edge
        DEL_edge<-Ntip(del_tree)+1
        if(ACC_tip == TRUE) {
            ACC_edge<-which(del_tree$tip.label == ACC_node)
        } else {
            ACC_edge<-which(del_tree$node.label == ACC_node)+Ntip(del_tree)
        }

        #Extracting the total edge length from DEL to ACC
        total.edge.length<-del_tree$edge.length[which(apply(del_tree$edge, 1, function(x) all(x == c(DEL_edge, ACC_edge))))] #edge connecting DEL_node to ACC_node

        #Calculate the terminal edges branch length and check if the tip is closer to the parent or offspring node.
        terms <- tree_slice$edge[, 2] <= Ntip(tree_slice)
        terminal.edges <- tree_slice$edge.length[terms]
        names(terminal.edges) <- tree_slice$tip.label[tree_slice$edge[terms, 2]]

        #Select the terminal edge for tip
        terminal.edge<-sort(terminal.edges[match(acc_tree$tip.label, names(terminal.edges))])
        names(terminal.edge) <- NULL

        #Choose ACC or DEL node
        if(terminal.edge < total.edge.length/2) {
            #cat(i, "-", tip, "\n", sep=" ")
            #print(DEL_node)
            return(DEL_node)
        } else {
            #cat(i, "-", tip, "\n", sep=" ")
            #print(ACC_node)
            return(ACC_node)
        }

    }
}
