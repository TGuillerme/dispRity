## Checking number of elements in each group in the data.frame
check.elements.data.frame <- function(group) {
    any(table(as.factor(group)) < 3)
}

## Splitting element function
split.elements.data.frame <- function(one_group, data) {

    ## lapply fun for selecting the elements
    select.elements <- function(subsets, one_group) {
        return(list("elements" = as.matrix(which(as.character(one_group) == as.character(levels(as.factor(one_group))[[subsets]])))))
    }

    ## Select the elements per subsets
    selected_elements <- lapply(as.list(1:length(levels(as.factor(one_group)))), select.elements, one_group)

    ## Adding the names to the subsets
    names(selected_elements) <- levels(as.factor(one_group))

    ## Output
    return(selected_elements)
}

## Convert row names in row numbers
convert.name.to.numbers <- function(one_group, data) {
    return(match(one_group, rownames(data)))
}


## Internal for set.group.list with tree
get.tree.clades <- function(tree, inc.nodes) {
    ## Get the bipartitions
    clades <- ape::prop.part(tree)

    ## Get the tips names for each clades
    clades <- lapply(clades, function(clade, labels) labels[clade], labels = attr(clades, "labels"))

    ## Add node labels
    if(inc.nodes) {
        get.node.labels <- function(tips, tree) {
            if(length(tips) != Ntip(tree)) {
                return(drop.tip(tree, tip =tree$tip.label[!(tree$tip.label %in% tips)])$node.label)
            } else {
                return(tree$node.label)
            }
        }
        clade_nodes <- lapply(clades, get.node.labels, tree = tree)
        clades <- mapply(c, clades, clade_nodes)
    }
    return(clades)
}

## Handle the group class for custom.subsets or select.axes
set.group.list <- function(group, data) {

    ## Get the overall group class
    group_class <- class(group)[1]
    ## Quick matrix convert
    if(group_class == "matrix") {
        group_class <- "data.frame"
        group <- as.data.frame(group)
    }
    
    ## Switch methods
    return(switch(group_class,
            ## Group is already a list
            "list"       = group,
            ## Group is a data.frame
            "data.frame" = unlist(sapply(1:ncol(group), function(X, group) split(rownames(group), as.factor(group[,X])), group = group, simplify = FALSE), recursive = FALSE),
            ## Group is a phylo
            "phylo"      = get.tree.clades(group, inc.nodes = !is.null(group$node.label))
            )
        )
}

## Check the group list for custom.subsets or select.axes
check.group.list <- function(group, data) {

    ## Input is a list
    ## Output is the list + warnings/stops

}