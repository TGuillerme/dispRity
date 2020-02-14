## Cleaning a tree so that the species match with the ones in a table
recursive.intersect <- function(list) {

    if(length(list) == 1) {
        return(list)
    } else {
        ## Intersect of element n and n+1
        list[[1]] <- intersect(list[[1]], list[[2]])
        ## Remove element n+1 from the list
        list <- list[-2]

        ## Recursive that!
        return(recursive.intersect(list))
    }

}

trim.tree <- function(tree, keep) {
    ## Select the tips to drop
    to_drop <- setdiff(tree$tip.label, keep)

    ## Drop the tips
    if(length(to_drop) > 0) {
        return(list(clean = drop.tip(tree, tip = to_drop),
                    dropped = to_drop))
    } else {
        return(list(clean = tree,
                    dropped = NA))
    }
}

trim.data <- function(data, keep) {
    ## Select the tips to drop
    to_drop <- setdiff(rownames(data), keep)

    ## Drop the tips
    if(length(to_drop) > 0) {
        return(list(clean = data[!(rownames(data) %in% to_drop),],
                    dropped = to_drop))
    } else {
        return(list(clean = data,
                    dropped = NA))
    }
}

clean.data.internal <- function(data, tree, tree_class, data_class) {
    ## Get the list of names for the data and the trees that intersect recursively
    data_names <- recursive.intersect(lapply(data, rownames))
    tree_names <- recursive.intersect(lapply(tree, function(x) x$tip.label))

    ## Make these names intersect!
    keep_names <- intersect(data_names[[1]], tree_names[[1]])

    ## Drop tips
    cleaned_trees <- lapply(tree, trim.tree, keep_names)

    ## Drop rows
    cleaned_datas <- lapply(data, trim.data, keep_names)

    ## Collect dropped info
    get.dropped <- function(x) x$dropped
    dropped_tips <- unique(unlist(lapply(cleaned_trees, get.dropped)))
    dropped_rows <- unique(unlist(lapply(cleaned_datas, get.dropped)))

    ## Remove NAs
    if(length(dropped_tips) > 1) {
        dropped_tips <- na.omit(dropped_tips)
        attributes(dropped_tips) <- NULL
    }
    if(length(dropped_rows) > 1) {
        dropped_rows <- na.omit(dropped_rows)
        attributes(dropped_rows) <- NULL
    }

    ## Collect cleaned data/tree
    get.clean <- function(x) x$clean
    tree_new <- lapply(cleaned_trees, get.clean)
    data_new <- lapply(cleaned_datas, get.clean)

    ## Reformat the data and trees 
    if(tree_class == "phylo") {
        tree_new <- tree_new[[1]]
    } else {
        class(tree_new) <- "multiPhylo"
    }

    if(data_class != "list") {
        data_new <- data_new[[1]]
    }

    ## Output the list
    return(list("tree" = tree_new, "data" = data_new, "dropped_tips" = dropped_tips,  "dropped_rows" = dropped_rows))
}