## Set the percentage for reaching the first sample containing three elements
get.percent.age <- function(tree, percent = 0.01) {
    ## Increment the percentage until at least three nodes/edges are crossed
    while(length(which(dist.nodes(tree)[Ntip(tree) + 1, ] - (percent * tree$root.time) < 0)) < 3 ) {
        percent <- percent + 0.01
    }
    ## Increment the slicing to contain 3 elements
    while(Ntip(paleotree::timeSliceTree(tree, tree$root.time - (percent * tree$root.time), drop.extinct = TRUE, plot = FALSE)) < 3) {
        percent <- percent + 0.01
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

    ## inc.nodes option is useless
    inc.nodes <- NULL

    ## lapply function for getting the slices
    get.slice <- function(slice, time, model, tree_ages, FADLADs, data, verbose, tree) {

        ## Verbose
        if(verbose) message(".", appendLF = FALSE)

        ## Get the age case
        if(time[slice] > min(tree_ages[,1])) {
            case <- "older"
        } else {
            case <- ifelse(time[slice] == min(tree_ages[,1]), "equal", "younger")
        }

        ## Slicing the tree
        switch(case,
            older   = {
                sub_tree <- slice.tree(tree, time[slice], model, FAD = FADLADs$FAD, LAD = FADLADs$LAD)
            },
            equal   = {
                sub_tree <- drop.tip(tree, tip = as.character(tree_ages[which(tree_ages[,1] != min(tree_ages[,1])), 2]))
                if(model == "equal.split" || model == "gradual.split") {
                    ## Transforming the subtree into a probability table
                    tips_list <- sub_tree$tip.label
                    nodes_list <- sapply(tips_list, function(tip, tree) slice.tree_parent.node(tree, tip), tree = sub_tree, simplify = FALSE)
                    sub_tree <- cbind(nodes_list, tips_list, rep(0, length(tips_list)))
                    rownames(sub_tree) <- colnames(sub_tree) <-  NULL
                }
            },
            younger = {
                sub_tree <- NA
            }
        )

        ## Empty subset
        if(all(class(sub_tree) != "phylo" & is.na(sub_tree))) {
            warning("The slice ", time[slice], " is empty.", call. = FALSE)
            return(list("elements" = matrix(NA)))
        }

        ## Output are single trees
        if(class(sub_tree) == "phylo") {
            ## Select the tips 
            tips <- sub_tree$tip.label

            ## Add any missed taxa from the FADLAD
            taxa <- rownames(data)[which(FADLADs$FAD$ages > time[slice] & FADLADs$LAD$ages < time[slice])]

            ## Getting the list of elements
            return( list( "elements" = as.matrix(match(unique(c(tips, taxa)), rownames(data))) ) )
        
        } else {
            ## Add any missed taxa from the FADLAD
            taxa <- rownames(data)[which(FADLADs$FAD$ages > time[slice] & FADLADs$LAD$ages < time[slice])]

            if(model == "equal.split" || model == "gradual.split") {
            ## Return a probability table
                if(any(length(taxa) > 0)) {
                    ## Combine the taxa, their ancestor and their probability to the sub_tree table
                    ancestors <- sapply(taxa, function(taxa, tree) return(slice.tree_parent.node(tree, taxa)), tree)
                    sub_tree <- rbind(sub_tree, matrix(c(ancestors, taxa, rep(0, length(taxa))), ncol = 3, byrow = FALSE))
                    rownames(sub_tree) <- NULL
                    ## Convert the tips into row numbers
                    tips <- t(apply(sub_tree[,1:2], 1, function(X) match(unique(X), rownames(data))))

                } else {
                    sub_tree <- matrix(sub_tree, ncol = 3)
                    tips <- matrix(match(sub_tree[,1:2], rownames(data)), ncol = 2)
                }

                ## Returning the tips with the probabilities
                return( list( "elements" = cbind(tips, round(as.numeric(sub_tree[,3]), digits = 4)) ) )
            } else {
            ## Return a matrix

                if(any(length(taxa) > 0)) {
                    sub_tree <- c(sub_tree, taxa)
                }

                return( list( "elements" = as.matrix(match(sub_tree, rownames(data))) ) )
            }

        }
    }

    ## ages of tips/nodes + FAD/LAD
    FADLADs <- adjust.FADLAD(FADLAD, tree, data)

    ## Getting the tree ages
    tree_ages <- tree.age(tree)[1:Ntip(tree), ]

    ## verbose
    if(verbose) {
        message("Creating ", length(time), " time samples through the tree:", appendLF = FALSE)
    }

    #get.slice(slice = 20, time, model, FADLADs, data, verbose, tree)

    ## Get the slices elements
    slices_elements <- lapply(as.list(seq(1:length(time))), get.slice, time, model, tree_ages, FADLADs, data, verbose, tree)

    ## verbose
    if(verbose) {
        message("Done.\n", appendLF = FALSE)
    }

    ## naming the slices
    names(slices_elements) <- time

    return(slices_elements)
}

## Making the origin subsets for a disparity_object
make.origin.subsets <- function(data) {
    origin <- list("elements" = as.matrix(seq(1:nrow(data))))
    origin_subsets <- list("origin" = origin)
    return(origin_subsets)
}