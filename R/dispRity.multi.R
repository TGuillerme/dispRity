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
    has_tree <- !is.null(data$tree)

    ## List holder    
    multi.list <- list()

    if(has_tree && length(data$matrix) == length(data$tree)) {
        ## Make pairs
        while(length(data$matrix) != 0) {
            multi.list[[length(multi.list)+1]] <- data
            multi.list[[length(multi.list)]]$matrix <- multi.list[[length(multi.list)]]$matrix[1]
            multi.list[[length(multi.list)]]$tree   <- multi.list[[length(multi.list)]]$tree[1]
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
                data[[not_unique]] <- data[[not_unique]][-1] 
            }
        }
    }
    return(multi.list)
}

## Apply the function to any pair of matrix + tree
dispRity.multi.apply <- function(data, fun, ...) {
    return(lapply(data, fun, ...))
}

# ## Merge the apply results into one classic dispRity object
# dispRity.multi.merge <- function(output, data_in, match_call, called_fun, ...) {

#     ## Merge the data
#     data <- data_in

#     ## Merge the disparity

#     ## Update the call

#     return(NULL)
# }



# ## Merging disparity results
# merge.disparity <- function(all_disparity) {
#     merge.subset.pair <- function(subset1, subset2) {
#         return(mapply(FUN = function(x,y)return(matrix(c(x, y), nrow = dim(x)[1])), x = subset1, y = subset2, SIMPLIFY = FALSE))
#     }
#     while(length(all_disparity) != 1) {
#         ## Merge all subsets
#         all_disparity[[1]] <- mapply(merge.subset.pair, all_disparity[[1]], all_disparity[[2]], SIMPLIFY = FALSE)
#         ## Removed merged set
#         all_disparity[[2]] <- NULL
#     }
#     return(unlist(all_disparity, recursive = FALSE))
# }
