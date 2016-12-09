## Extracting a specific rarefaction level
extract.disparity.values <- function(series, data, rarefaction) {
    ## Get the rarefaction level
    if(rarefaction != FALSE) {
        rarefaction = as.numeric(which(lapply(data$series[[series]][-1], nrow) == rarefaction) + 1)
        if(length(rarefaction) == 0) {
            ## No rarefaction level for this series
            return(NULL)
        }
    } else {
        rarefaction = 2
    }
    return(as.numeric(data$disparity[[series]][[rarefaction]]))
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