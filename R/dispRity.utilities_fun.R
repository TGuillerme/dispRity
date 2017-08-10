## Extracting a specific rarefaction level
extract.disparity.values <- function(subsamples, data, rarefaction, concatenate) {
    ## Get the rarefaction level
    if(rarefaction != FALSE) {
        rarefaction = as.numeric(which(lapply(data$subsamples[[subsamples]][-1], nrow) == rarefaction) + 1)
        if(length(rarefaction) == 0) {
            ## No rarefaction level for this subsample
            return(NULL)
        }
    } else {
        rarefaction = 2
    }
    if(concatenate) {
        return(list(as.numeric(data$disparity[[subsamples]][[rarefaction]])))
    } else {
        return(lapply(seq_len(ncol(data$disparity[[subsamples]][[rarefaction]])), function(col) data$disparity[[subsamples]][[rarefaction]][,col]))
    }
}

## Remove nulls from a list
clean.list <- function(list) {
    nulls <- unlist(lapply(list, is.null))
    return(list[!nulls])
}

## Recursive sorting
recursive.sort <- function(data, sort) {
    return(data[sort])
}

## Merging two subsamples
merge.two.subsamples <- function(subs1, subs2, data) {
    ## Get the list of new subsamples
    new_subsample <- list("elements" = matrix(unique(c(data$subsamples[[subs1]]$elements, data$subsamples[[subs2]]$elements, ncol = 1))))
    ## Replace the second subsample with the new one
    data$subsamples[[subs2]] <- new_subsample
    ## Rename it
    names(data$subsamples)[subs2] <- paste(names(data$subsamples)[subs1], names(data$subsamples)[subs2], sep = "-") 
    ## Remove the former
    data$subsamples[[subs1]] <- NULL
    return(data)
}