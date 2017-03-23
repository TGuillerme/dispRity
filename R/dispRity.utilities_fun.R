## Extracting a specific rarefaction level
extract.disparity.values <- function(subsamples, data, rarefaction, concatenate) {
    ## Get the rarefaction level
    if(rarefaction != FALSE) {
        rarefaction = as.numeric(which(lapply(data$subsamples[[subsamples]][-1], nrow) == rarefaction) + 1)
        if(length(rarefaction) == 0) {
            ## No rarefaction level for this subsamples
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