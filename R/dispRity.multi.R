# Internal function for applying a function to multiple data and trees
# If data$call$dispRity.multi = TRUE (sorted by check.dispRity.tree && check.dispRity.data) then apply the disparity function as a lapply




# e.g.

# if(data$call$dispRity.multi) {
#     ## Splitting the dispRity object into a list to be fed to lapply
#     data_split <- dispRity.multi.split(data)
#     ## Apply the function (lapply style) and override split for RAM management
#     data_split <- dispRity.multi.apply(data_split, fun = my_fun, ...) # where my_fun = c(custom.subsets, chrono.subsets, boot.matrix or dispRity)
#     ## Merge the resulting list (and override for RAM management)
#     data_split <- dispRity.multi.merge(data)
#     ## Returns the dispRity object (with call set to dispRity.multi = TRUE)
#     return(data_split)
# }

# ## Final version should be streamlined to
# if(data$call$dispRity.multi) {
#     return(dispRity.multi.merge(dispRity.multi.apply(dispRity.multi.split(data), fun = my_fun, ...)))
# }

## TODO 1: make check.dispRity.tree ping out data$call$dispRity.multi


## Splits the data into pairs of matrix + tree
dispRity.multi.split <- function(data) {
    
    ## Check if tree is needed
    has_tree <- !is.null(data$tree[[1]])

    ## List holder    
    multi.list <- list()

    if(has_tree && length(data$matrix) == length(data$tree)) {
        ## Make pairs
        while(length(data$matrix) != 0) {
            multi.list[[length(multi.list)+1]] <- data
            multi.list[[length(multi.list)]]$matrix <- multi.list[[length(multi.list)]]$matrix[1]
            multi.list[[length(multi.list)]]$tree   <- multi.list[[length(multi.list)]]$tree[1]
            if(!is.null(data$disparity)) {
                multi.list[[length(multi.list)]]$disparity <- lapply(data$disparity, function(x) return(x[1, ]))
                data$disparity <- lapply(data$disparity, function(x) return(x[-1, ]))
            }
            data$matrix <- data$matrix[-1]
            data$tree <- data$tree[-1]     
        }
    } else {
        ## Make multiples
        n_matrices <- length(data$matrix)
        n_trees <- length(data$tree)

        if(has_tree) {
            ## Detect if any of the matrices or trees are unique
            if(length(unique <- which(c(n_matrices, n_trees) == 1)) > 0) {
                ## Find the variable
                not_unique <- which(!(c("matrix", "tree") %in% switch(as.character(unique), "1" = "matrix", "2" = "tree")))
                ## Make the list
                while(length(data[[not_unique]]) != 0) {
                    multi.list[[length(multi.list)+1]] <- data
                    multi.list[[length(multi.list)]][[not_unique]] <- multi.list[[length(multi.list)]][[not_unique]][1]
                    if(!is.null(data$disparity)) {
                        multi.list[[length(multi.list)]]$disparity <- lapply(data$disparity, function(x) return(x[1, ]))
                        data$disparity <- lapply(data$disparity, function(x) return(x[-1, ]))
                    }
                    data[[not_unique]] <- data[[not_unique]][-1] 
                }
            } else {
                ## Multiply the list
                n_out <- expand.grid(1:n_matrices, 1:n_trees)
                ## Make the list
                while(nrow(n_out) > 0) {
                    multi.list[[length(multi.list)+1]] <- data
                    multi.list[[length(multi.list)]]$matrix <- data$matrix[n_out[1,1]]
                    multi.list[[length(multi.list)]]$tree   <- data$tree[n_out[1,2]]
                    if(!is.null(data$disparity)) {
                        multi.list[[length(multi.list)]]$disparity <- lapply(data$disparity, function(x) return(x[1, ]))
                        data$disparity <- lapply(data$disparity, function(x) return(x[-1, ]))
                    }
                    n_out <- n_out[-1, ]  
                }
            }
        } else {
            ## Just change the matrices (data has no tree)
            not_unique <- which(names(data) == "matrix")
            ## Make the list
            while(length(data[[not_unique]]) != 0) {
                multi.list[[length(multi.list)+1]] <- data
                multi.list[[length(multi.list)]][[not_unique]] <- multi.list[[length(multi.list)]][[not_unique]][1]
                if(!is.null(data$disparity)) {
                    multi.list[[length(multi.list)]]$disparity <- lapply(data$disparity, function(x) return(x[1, ]))
                    data$disparity <- lapply(data$disparity, function(x) return(x[-1, ]))
                }
                data[[not_unique]] <- data[[not_unique]][-1] 
            }
        }
    }

    if(has_tree) {
        ## Clean the data (should be checked prior normally)
        return(lapply(multi.list, lapply.clean.data))
    } else {
        return(multi.list)
    }
}
## Clean data for dispRity.multi.split
lapply.clean.data <- function(x) {
    ## Clean the data
    cleaned <- clean.data(x$matrix[[1]], x$tree[[1]], inc.nodes = !is.null(x$tree[[1]]$node.label))
    tree_out <- list(cleaned$tree)
    class(tree_out) <- "multiPhylo"
    return(list(matrix = list(cleaned$data), tree = list(tree_out), multi = x$multi))
}

## Apply the function to any pair of matrix + tree
dispRity.multi.apply <- function(matrices, fun, trees = NULL, ...) {

    ## Handle extra args
    dots <- list(...)
    match_call <- match.call()

    ## Detect the type:
    type <- ifelse(any(c(is.null(trees), (length(trees) == 1))), "lapply", "mapply")

    ## Making argument list for chrono.subsets if FADLAD is provided as a list
    if(!is.null(dots$FADLAD) && is(dots$FADLAD, "list")) {
        ## Use a do.call
        type <- "do.call"

        ## Get the list of arguments
        chrono_args <- mapply(function(x, y) list(data = x, tree = y), matrices, trees, SIMPLIFY = FALSE)

        ## Adding the FADLADs
        chrono_args <- mapply(function(x, y) list(data = x$data, tree = x$tree, "FADLAD" = y), chrono_args, dots$FADLAD, SIMPLIFY = FALSE)

        ## Removing FADLADs
        dots$FADLAD <- NULL
        ## Adding all the other arguments
        chrono_args <- lapply(chrono_args, function(x, args) c(x, args), args = dots)
    }

    ## Toggle to bootstraps (no tree argument)
    if(is.null(trees) && match_call$fun == "boot.matrix.call") {
        type <- "boot"
    }

    ## Applying the fun
    out <- switch(type,
                  "lapply"  = lapply(matrices, fun, trees, ...),
                  "mapply"  = mapply(fun, matrices, trees, MoreArgs = list(...), SIMPLIFY = FALSE),
                  "do.call" = do.call(fun, chrono_args),
                  "boot"    = lapply(matrices, fun, ...))
    ## New class
    class(out) <- c("dispRity", "multi")
    return(out)
}

## Merge the apply results into one classic dispRity object
dispRity.multi.merge <- function(data, output, match_call, ...) {

    ## Combine the data
    data_out <- dispRity.multi.merge.data(data)

    ## Combine the disparity results
    all_disparity <- lapply(output, `[[`, "disparity")
    data_out$disparity <- dispRity.multi.merge.disparity(all_disparity)

    ## Update the call
    data_out$call <- output[[1]]$call$disparity
    ## Update the metric call name
    data_out$call$disparity$metrics$name <- match_call$metric
    ## Make it dispRity multi
    data_out$call$dispRity.multi <- TRUE
    return(data)
}

## Merges data from a split (not output)
dispRity.multi.merge.data <- function(data) {
    data_out <- data[[1]]
    data_out$matrix <- unlist(lapply(data, `[[`, "matrix"), recursive = FALSE)
    if(!is.null(data_out$tree[[1]])) {
        trees <- lapply(data, `[[`, "tree")
        class(trees) <- "multiPhylo"
        data_out$tree <- trees
    }
    ## Make it dispRity multi
    data_out$call$dispRity.multi <- TRUE
    
    ## Merge subset names
    if(!is.null(names(data_out$subsets))) {
        names(data_out$subsets) <- apply(do.call(cbind, lapply(data, name.subsets)), 1, function(row) paste0(unique(row), collapse = "/"))
    }

    return(data_out)
}

## Merging disparity results
dispRity.multi.merge.disparity <- function(all_disparity) {
    merge.subset.pair <- function(subset1, subset2) {
        return(mapply(FUN = function(x,y)return(matrix(c(x, y), nrow = dim(x)[1])), x = subset1, y = subset2, SIMPLIFY = FALSE))
    }
    while(length(all_disparity) != 1) {
        ## Merge all subsets
        all_disparity[[1]] <- mapply(merge.subset.pair, all_disparity[[1]], all_disparity[[2]], SIMPLIFY = FALSE)
        ## Removed merged set
        all_disparity[[2]] <- NULL
    }
    return(unlist(all_disparity, recursive = FALSE))
}
