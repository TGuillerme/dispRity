#FUNCTIONS FOR tree.age


#Extract the ages table from a tree
#Calculating the tips and the elements age
tree.age_table<-function(tree){
    tree_length <- Ntip(tree)
    
    ages <- dist.nodes(tree)[tree_length+1,]
    tip.names <- tree$tip.label
    if(is.null(tree$node.label)) {
        nod.names <- c((tree_length+1):length(dist.nodes(tree)[,1]))
    } else {
        nod.names <- tree$node.label
    }
    elements <- c(tip.names, nod.names)
    ages.table <- data.frame(ages = ages,elements = elements)
    return(ages.table)
}


#Scaling the ages from tree.age_table if scale != 1
tree.age_scale <- function(ages.table, scale){
    ages.table$ages <- ages.table$ages/max(ages.table$ages)
    ages.table$ages <- ages.table$ages*scale
    return(ages.table)
}