# Internal function for applying a function to multiple data and trees
# If data$call$dispRity.multi = TRUE (sorted by check.dispRity.tree) then apply the disparity function as a lapply

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
    if(length(data$matrix) == length(data$tree)) {
        ## Make pairs
    } else {
        ## Make multiples
    }
    return(NULL)
}

## Apply the function to any pair of matrix + tree
dispRity.multi.apply <- function(data, fun, ...) {
    return(NULL)
}

## Merge the apply results into one classic dispRity object
dispRity.multi.split <- function(data) {
    return(NULL)
}