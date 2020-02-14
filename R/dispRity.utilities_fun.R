## Extracting a specific rarefaction level
extract.disparity.values <- function(subsets, data, rarefaction, concatenate) {
    ## Get the rarefaction level
    if(rarefaction != FALSE) {
        rarefaction = as.numeric(which(lapply(data$subsets[[subsets]][-1], nrow) == rarefaction) + 1)
        if(length(rarefaction) == 0) {
            ## No rarefaction level for this subset
            return(NULL)
        }
    } else {
        rarefaction = 2
    }
    if(concatenate) {
        return(list(as.numeric(data$disparity[[subsets]][[rarefaction]])))
    } else {
        return(lapply(seq_len(ncol(data$disparity[[subsets]][[rarefaction]])), function(col) data$disparity[[subsets]][[rarefaction]][,col]))
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

## Merging two subsets
merge.two.subsets <- function(subs1, subs2, data) {
    ## Get the list of new subsets
    new_subset <- list("elements" = matrix(unique(c(data$subsets[[subs1]]$elements, data$subsets[[subs2]]$elements, ncol = 1))))
    ## Replace the second subset with the new one
    data$subsets[[subs2]] <- new_subset
    ## Rename it
    names(data$subsets)[subs2] <- paste(names(data$subsets)[subs1], names(data$subsets)[subs2], sep = "-") 
    ## Remove the former
    data$subsets[[subs1]] <- NULL
    return(data)
}

## Check subset availability
check.subsets <- function(subsets, data) {
    if(length(subsets) > length(data$subsets)) {
        stop("Not enough subsets in the original data.")
    } else {
        if(is(subsets, "numeric") || is(subsets, "integer")) {
            if(any(is.na(match(subsets, 1:length(data$subsets))))) {

                subsets <- subsets[which(is.na(match(subsets, 1:length(data$subsets))))]
                orthograph <- ifelse(length(subsets) == 1, "Subsample", "Subsamples")
                stop(paste(orthograph, paste(subsets, collapse = ", "), "not found."))

            }
        } else {
            if(is(subsets, "character")) {
                if(any(is.na(match(subsets, names(data$subsets))))) {

                    subsets <- subsets[which(is.na(match(subsets, names(data$subsets))))]
                    orthograph <- ifelse(length(subsets) == 1, "Subsample", "Subsamples")
                    stop(paste(orthograph, paste(subsets, collapse = ", "), "not found."))

                }
            } else {
                stop("subsets argument must be of class \"numeric\" or \"character\".")
            }
        }
    }
}

## Detecting the bin age lower or greater than a value
detect.bin.age <- function(data, value, greater = FALSE) {
    ## Detect the bin before the extinction time
    bin_times <- unlist(sapply(names(data$subsets), strsplit, split = " - ", simplify = FALSE), recursive = FALSE)

    ## Detecting the bin age
    detect.bin.ages.lapply <- function(one_bin, value, greater) {
        if(greater) {
            return(ifelse(as.numeric(one_bin)[2] >= value, TRUE, FALSE))
        } else {
            return(ifelse(as.numeric(one_bin)[1] <= value, TRUE, FALSE))
        }
    }

    return(unlist(lapply(bin_times, detect.bin.ages.lapply, value, greater)))
}