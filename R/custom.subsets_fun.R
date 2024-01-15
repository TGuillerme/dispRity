## Internal for set.group.list with tree
get.tree.clades <- function(tree, data) {

    ## Get the bipartitions
    clades <- ape::prop.part(tree)

    ## Get the tips names for each clades
    clades <- lapply(clades, function(clade, labels) labels[clade], labels = attr(clades, "labels"))

    ## Select whether to include the nodes or not (default is not)
    inc.nodes <- FALSE
    if(!is.null(tree$node.label)) {
        row_names <- if(is(data, "list")) {
            rownames(data[[1]])
        } else {
            rownames(data)
        }
        ## Tree contains node labels but check if any node is present in the data
        if(any(row_names %in% tree$node.label)) {
            inc.nodes <- TRUE
        }
    }

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
set.group.list <- function(group, data, group_class) {

    ## Quick matrix convert
    if(group_class[1] == "matrix") {
        group_class <- "data.frame"
        group <- as.data.frame(group)
    }

    ## Logical is set to factor
    if(group_class[1] == "logical") {
        group <- as.factor(group)
        group_class[1] <- "factor"
    }

    ## Switch methods
    return(switch(group_class,
            ## Group is already a list
            "list"       = group,
            ## Group is a data.frame
            "data.frame" = {group_list <- sapply(1:ncol(group),
                         function(X, group) split(rownames(group), as.factor(group[,X])), group = group, simplify = FALSE) ;
                           names(group_list) <- colnames(group)
                           unlist(group_list, recursive = FALSE)},
            ## Group is a phylo
            "phylo"      = get.tree.clades(group, data),
            ## Group is factor
            "factor"     = {group_list <- lapply(as.list(levels(group)), function(lvl, group) which(group == lvl), group = group) ; names(group_list) <- levels(group) ; group_list}
        ))
}





## Check the elements in a group
check.elements <- function(one_group, row_names, group_class, match_call) {

    ## Error for weird group formats
    if(!(class(one_group)[1] %in% c("integer", "numeric", "character", "logical"))) {
        stop(paste0("The group argument cannot contain elements of class ", class(one_group)[1], "."), call. = FALSE)
    }

    ## Return NAs (empty groups)
    if(all(is.na(one_group))) {
        return(one_group)
    }

    ## Get the elements classes
    row_class <- class(row_names) # Should always be "character"
    elem_class <- class(one_group)

    ## Match the element names to the row names class
    if(elem_class != row_class) {
        if(elem_class %in% c("integer", "numeric")) {
            converted_group <- row_names[one_group]
        } else {
            ## Convert the elements into characters
            converted_group <- row_names[match(one_group, row_names)]
        }
    } else {
        converted_group <- one_group
    }

    ## Check if all elements are present in rownames
    unmatching <- !(converted_group %in% row_names)
    if(any(unmatching)) {
        stop.call(msg.pre = paste0("The following element", ifelse(sum(unmatching) == 1, " ", "s "), "cannot be found in "),
                  call = match_call,
                  msg = paste0(": ", paste(one_group[unmatching], collapse = ", "), ".", ifelse(group_class == "phylo", "\nSee ?clean.data for matching the tree and the data.", "")))
    }

    ## Convert into integers
    if(!is(one_group, "integer")) {
        if(is(one_group, "numeric")) {
            one_group <- as.integer(one_group)
        } else {
            one_group <- match(one_group, row_names)
        }
    }

    return(one_group)
}

## Check the group list for custom.subsets or select.axes
check.group.list <- function(group, data, group_class, match_call) {

    ## Set the group names
    if(is.null(names(group))) {
        ## Adding some group names
        names(group) <- 1:length(group)
    }

    ## Set the row names
    row_names <- if(is(data, "list")) {
        rownames(data[[1]])  
    } else {
        rownames(data)
    }

    ## Check if each list element exist in the data
    return(lapply(group, check.elements, row_names, group_class, match_call))
}