## Set the percentage for reaching the first sample containing three elements
get.percent.age <- function(tree, percent = 0.01) {
    ## Increment the percentage until at least three nodes/edges are crossed
    tree_slice <- slice.tree.sharp(tree, tree$root.time - (percent * tree$root.time))
    while(is.null(tree_slice) || Ntip(tree_slice) < 3) {
        percent <- percent + 0.01
        tree_slice <- slice.tree.sharp(tree, tree$root.time - (percent * tree$root.time))
    }
    return(percent)
}


## Internal function for adjust.FADLAD
adjust.age <- function(FADLAD, ages_tree) {
    return(ifelse(FADLAD != ages_tree, FADLAD, ages_tree))
}

## Get adjusted FADLAD
## FAD argument is whether to adjust FAD (TRUE) or LAD (FALSE)
adjust.FADLAD <- function(FADLAD, tree, data) {
    ## Get the tree ages
    ages_tree <- tree.age(tree)

    ## Match the ages_tree_FAD/LAD with the FADLAD table
    names_match <- match(rownames(FADLAD), ages_tree[,2])
    ages_tree_tmp <- ages_tree[names_match,]

    ## Adjust the FAD/LAD
    ages_tree_FAD <- ages_tree_LAD <- ages_tree_tmp
    ages_tree_FAD[,1] <- mapply(adjust.age, as.list(FADLAD[,1]), as.list(ages_tree_tmp[,1]))
    ages_tree_LAD[,1] <- mapply(adjust.age, as.list(FADLAD[,2]), as.list(ages_tree_tmp[,1]))

    ## Combine all ages
    ages_tree_FAD <- rbind(ages_tree_FAD, ages_tree[-names_match,])
    ages_tree_LAD <- rbind(ages_tree_LAD, ages_tree[-names_match,])

    ## Match the ages with the data
    row_order <- match(rownames(data), ages_tree_FAD$elements)

    return(list("FAD" = ages_tree_FAD[row_order,], "LAD" = ages_tree_LAD[row_order,]))
}


## Discrete time subsets
chrono.subsets.discrete <- function(data, tree, time, model = NULL, FADLAD, inc.nodes, verbose) {
    
    ## Model option is useless
    model <- NULL

    ## lapply function for getting the interval
    get.interval <- function(interval, time, ages_tree, inc.nodes, verbose) {
        if(verbose) message(".", appendLF = FALSE)
        if(inc.nodes) {
            return( list("elements" = as.matrix(which(ages_tree$FAD$ages >= time[interval+1] & ages_tree$LAD$ages <= time[interval]) )))
        } else {
            one_interval <- which(ages_tree$FAD$ages >= time[interval+1] & ages_tree$LAD$ages <= time[interval])
            matching <- match(tree$tip.label, rownames(data[one_interval,]))
            ## Only remove NAs if present
            if(any(is.na(matching))) {
                elements_out <- list("elements" = as.matrix(one_interval[matching[-which(is.na(matching))]]) )
            } else {
                elements_out <- list("elements" = as.matrix(one_interval[matching]))
            }
            return(elements_out)
        }
    }

    ## Verbose
    if(verbose) {
        message("Creating ", length(time)-1, " time bins through time:", appendLF = FALSE)
    }

    ## ages of tips/nodes + FAD/LAD
    ages_tree <- adjust.FADLAD(FADLAD, tree, data)

    ## Attribute each taxa/node to its interval
    interval_elements <- lapply(as.list(seq(1:(length(time)-1))), get.interval, time, ages_tree, inc.nodes, verbose)

    ## Get the names of the intervals
    names(interval_elements) <- paste(time[-length(time)], time[-1], sep = " - ")

    if(verbose) message("Done.\n", appendLF = FALSE)

    ## If interval is empty, send warning and delete the interval
    for (interval in 1:length(interval_elements)) {
        if(nrow(interval_elements[[interval]]$elements) == 0) {
            warning("The interval ", names(interval_elements)[interval], " is empty.", call. = FALSE)
            interval_elements[[interval]]$elements <- matrix(NA)
        }
    }

    return(interval_elements)
}

## Continuous time subsets
chrono.subsets.continuous <- function(data, tree, time, model, FADLAD, inc.nodes = NULL, verbose) {

    ## verbose
    if(verbose) {
        ## Editing the fast.slice.table function
        message("Creating ", length(time), " time samples through the tree:", appendLF = FALSE)
    }

    ## Get all time slices
    slices_elements <- lapply(as.list(time), get.time.slice, tree, model, verbose)

    ## Adjust the tree/data names
    slices_elements <- lapply(slices_elements, match.tree.data, tree, data)

    ## Adding FADLADs
    if(!is.null(FADLAD)) {
        slices_elements <- mapply(add.FADLAD, slices_elements, as.list(time), MoreArgs = list(FADLAD = FADLAD, data_rownames = rownames(data)), SIMPLIFY = FALSE)
    }

    ## naming the slices
    names(slices_elements) <- time

    ## verbose
    if(verbose) {
        message("Done.\n", appendLF = FALSE)
    }

    return(slices_elements)
}

## Making the origin subsets for a disparity_object
make.origin.subsets <- function(data) {
    origin <- list("elements" = as.matrix(seq(1:nrow(data))))
    origin_subsets <- list("origin" = origin)
    return(origin_subsets)
}

## cbind with missing data
cbind.fill <- function(x, y) {
    ## Check the number of rows
    if(dim(x)[1] == dim(y)[1]) {
        ## Simple cbind
        return(list("elements" = cbind(x, y)))
    } else {
        ## Minimum number of rows
        min_rows <- min(dim(x)[1], dim(y)[1])
        ## Minimal cbind
        output <- cbind(x[1:min_rows, , drop = FALSE], y[1:min_rows, , drop = FALSE])
        ## Add the missing rows
        if(dim(x)[1] == min_rows) {
            ## Add the y last rows 
            NAs <- cbind(matrix(NA, ncol = dim(x)[2], nrow = dim(y)[1]-min_rows),
                            y[-c(1:min_rows), , drop = FALSE])
        } else {
            ## Add the x last rows
            NAs <- cbind(x[-c(1:min_rows), , drop = FALSE],
                        matrix(NA, ncol = dim(y)[2], nrow = dim(x)[1]-min_rows))
        }
        ## Combine both
        return(list("elements" = rbind(output, NAs)))
    }
}

## Recursive combinations of the lists
recursive.combine.list <- function(list) {
    if(length(list) == 2) {
        ## Do cbind on the two elements of the list
        return(mapply(function(x,y) cbind.fill(x$elements, y$elements),
                      list[[1]], list[[length(list)]], SIMPLIFY = FALSE))
    } else {                
        ## Do cbind on the first and last elements of the list
        list[[1]] <- mapply(function(x,y) cbind.fill(x$elements, y$elements),
                            list[[1]], list[[length(list)]], SIMPLIFY = FALSE)
        ## Remove the last element of the list
        list[[length(list)]] <- NULL
        ## Repeat!
        return(recursive.combine.list(list))
    }
}

## Slice tree table
fast.slice.table <- function(slice, tree) {

    ## Get slice time
    slice_time <- round(tree$root.time - slice, 9)

    ## Root slice
    if(slice_time == 0) {
        root_edges <- which(tree$edge[,1] == Ntip(tree)+1)
        return(cbind(tree$edge[root_edges, 1], c(0, 0), tree$edge[root_edges, 2], tree$edge.length[root_edges]))
    }

    ## Get nodes and tips ages
    node_age <- round(castor::get_all_distances_to_root(tree), 9)

    ## Find the edges that are crossed
    crossed_edges <- which((node_age[ tree$edge[, 1] ] < slice_time) & (node_age[tree$edge[, 2] ] >= slice_time))

    ## Beyond tree slice
    if(length(crossed_edges) == 0) {
        return(NULL)
    }

    ## Get the edge lengths length and right
    get.sliced.edge <- function(crossed_edge, tree, node_age, slice_time) {
        # return(tree$root.time - node_age[tree$edge[crossed_edge, ]] - slice_time)
        return(abs(node_age[tree$edge[crossed_edge, ]] - slice_time))
    }
    sliced_edge_lengths <- t(sapply(crossed_edges, get.sliced.edge, tree, node_age, slice_time))

    # warning("DEBUG fast.slice.table colnames")
    # slice_table <- cbind(tree$edge[crossed_edges, 1], sliced_edge_lengths[,1], tree$edge[crossed_edges, 2], sliced_edge_lengths[, 2])
    # colnames(slice_table) <- c("left.point", "left.edge", "right.point", "right.edge")

    return(cbind(tree$edge[crossed_edges, 1], sliced_edge_lengths[,1], tree$edge[crossed_edges, 2], sliced_edge_lengths[, 2]))
}

## select slice table tips
select.table.tips <- function(table, model) {
    if(is.null(table)) {
        return(NA)
    }

    switch(model,
        "acctran"   = return(unique(table[,3])),
        "deltran"   = return(unique(table[,1])),
        "random"    = return(unique(apply(table[,c(1,3)], 1, FUN = function(x) x[sample(c(1,2), 1)]))),
        "proximity" = return(unique(sapply(1:nrow(table), function(x, table, closest) table[,c(1,3)][x, closest[x]], table, apply(table[,c(2,4)], 1, FUN = function(x) which(x == min(x))[1])))),
        ## The split models output a table of two columns (left and right of the split) and the probability for the first column (p(left)). The probability for the second column is simply 1-p(left)
        "equal.split"   = return(cbind(table[, c(1,3)], 0.5)),
        "gradual.split" = return(cbind(table[,c(1,3)], 1-(table[,2]/(table[,2]+table[,4]))))
        )
}

## Wrapper for getting a time slice
get.time.slice <- function(time, tree, model, verbose) {
    if(verbose) message(".", appendLF = FALSE)

    ## Get the precision
    # precision <- ifelse(abs(tree$root.time - round(tree$root.time)) > .Machine$double.eps^0.5, nchar(strsplit(sub('0+$', '', as.character(tree$root.time)), ".", fixed = TRUE)[[1]][[2]]), 0)

    ## Get the slice
    slice <- select.table.tips(fast.slice.table(time, tree), model)
    if(is.na(slice[1])) {
        warning("The slice ", time, " is empty.", call. = FALSE)
    }
    return(list("elements" = matrix(slice, ncol = ifelse(grepl("split", model), 3, 1))))
}

## Adding FADLADs to time slices
add.FADLAD <- function(time_slice, one_time, FADLAD, data_rownames) {
    ## Find if one_time is within any FAD/LAD interval
    intervals <- (one_time >= FADLAD[,2]) & (one_time <= FADLAD[,1])

    if(any(intervals)) {
        ## Try to add the taxa to the interval
        add_tips <- which(data_rownames %in% rownames(FADLAD)[intervals])
        if(dim(time_slice$elements)[2] == 1) {
            ## Add the tips for simple models
            time_slice$elements <- matrix(unique(c(time_slice$elements, add_tips)))
        } else {
            ## Add full probability of being the tip for probabilistic models
            time_slice$elements <- rbind(time_slice$elements, 
                                         cbind(matrix(add_tips),matrix(add_tips), 1))
            ## Remove any non full probability with the same elements for the tips
            remove.duplicates <- function(time_slice, col) {
                ## Find duplicated elements with a probability of 1
                duplicated_elements <- (duplicated(time_slice$elements[,col]) & time_slice$elements[,3] == 1)
                ## Remove the duplicates
                if(any(duplicated_elements)) {
                    ## Find the rows to replace
                    replace <- which(time_slice$elements[,col] %in% time_slice$elements[duplicated_elements, col][1])
                    ## Replace the row
                    time_slice$elements[replace[1], ] <- time_slice$elements[replace[2], ]
                    ## Remove the duplicated row
                    time_slice$elements <- time_slice$elements[-replace[2], ]
                    ## Repeat the operation
                    remove.duplicates(time_slice, col)
                } else {
                    return(time_slice)
                }
            }

            ## Remove the duplicates from the first column
            time_slice <- remove.duplicates(time_slice, col = 2)
            ## Remove the duplicates from the second column
            time_slice <- remove.duplicates(time_slice, col = 1)
        }
    } 
    return(time_slice)
}

## match tree tips/nodes to data rownames
match.tree.data <- function(elements, tree, data) {
    
    if(all(is.na(elements$elements))) {
        return(elements)
    }

    matching <- function(x, tree, data) {
        return(match(c(tree$tip.label, tree$node.label)[x], rownames(data)))
    }
    if(dim(elements$elements)[2] == 1) {
        elements$elements <- matrix(matching(elements$elements, tree, data), ncol = 1)
    } else {
        elements$elements[,c(1,2)] <- apply(elements$elements[,c(1,2)], 2, FUN = matching, tree, data)
    }
    return(elements)
}

