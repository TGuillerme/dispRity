## Cleaning a tree so that the species match with the ones in a table
clean.tree.table <- function(tree, data, inc.nodes) {

    ## Intersecting names between both data sets
    if(inc.nodes) {
        labels <- c(tree$tip.label, tree$node.label)
    } else {
        labels <- tree$tip.label
    }
    matching_names <- intersect(labels, rownames(data))

    ## Which data is present
    data_match <- rownames(data) %in% matching_names

    ## Which tips are present
    tips_match <- labels %in% matching_names

    ## Matching the data    
    if(all(data_match)) {
        dropped_rows <- NA
    } else {
        rows_numbers <- which(!data_match)
        dropped_rows <- rownames(data)[rows_numbers]
        data <- data[-c(rows_numbers),]
    }

    ## Matching the tree
    if(all(tips_match)) {
        dropped_tips <- NA
    } else {
        dropped_tips <- labels[!tips_match]
        if(any(dropped_tips %in% tree$node.label)) {
            return(dropped_tips[dropped_tips %in% tree$node.label])
        } else {
            tree <- drop.tip(tree, tip = dropped_tips)
        }
    }

    return(list("tree" = tree, "data" = data, "dropped_tips" = dropped_tips, "dropped_rows" = dropped_rows))
}