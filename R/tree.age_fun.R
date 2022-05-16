#FUNCTIONS FOR tree.age

#Extract the ages table from a tree
#Calculating the tips and the elements age
tree.age_table<-function(tree){
    tree_length <- Ntip(tree)
    
    ages <- castor::get_all_pairwise_distances(tree)[tree_length + 1,]
    tip.names <- tree$tip.label[1:tree_length]
    if(is.null(tree$node.label)) {
        nod.names <- c((tree_length + 1):length(castor::get_all_pairwise_distances(tree)[, 1]))
    } else {
        nod.names <- tree$node.label[1:Nnode(tree)]
    }
    elements <- c(tip.names, nod.names)
    ages.table <- data.frame(ages = ages, elements = elements)
    return(ages.table)
}


#Scaling the ages from tree.age_table if scale != 1
tree.age_scale <- function(ages.table, scale){
    ages.table$ages <- ages.table$ages/max(ages.table$ages)
    ages.table$ages <- ages.table$ages*scale
    return(ages.table)
}