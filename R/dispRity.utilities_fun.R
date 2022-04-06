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

    if(!is.null(data$call$disparity) && data$call$disparity$metrics$between.groups) {

        ## Numeric subsets
        if(is(subsets, "numeric") || is(subsets, "integer")) {
            if(any(na_subsets <- is.na(match(subsets, 1:length(data$disparity))))) {
                ## Subsets not found
                stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."))
            }
        } else {
            if(is(subsets, "character")) {
                ## Get the subset names (searched and available)
                subset_search <- unique(unlist(strsplit(subsets, split = ":")))
                subset_available <- unique(unlist(strsplit(names(data$disparity), split = ":")))

                ## Check if the searched ones exist
                if(any(na_subsets <- is.na(match(subset_search, subset_available)))) {
                    ## Subsets not found
                    stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."))
                }
            } else {
                stop("subsets argument must be of class \"numeric\" or \"character\".")
            }
        }

    } else {
        
        if(is(subsets, "list")) {
            ## Flatten the list for checking for subsets
            subsets <- unique(unlist(subsets))
        }

        if(length(subsets) > length(data$subsets)) {
            stop("Not enough subsets in the original data.")
        } else {
            if(is(subsets, "numeric") || is(subsets, "integer")) {
                if(any(na_subsets <- is.na(match(subsets, 1:length(data$subsets))))) {
                    ## Subsets not found
                    stop(paste0(ifelse(length(which(na_subsets)) > 1, "Subsets ", "Subset "), paste0(subsets[which(na_subsets)], collapse = ", "), " not found."))
                }
            } else {
                if(is(subsets, "character")) {
                    if(any(is.na(match(subsets, names(data$subsets))))) {

                        subsets <- subsets[which(is.na(match(subsets, names(data$subsets))))]
                        orthograph <- ifelse(length(subsets) == 1, "Subset ", "Subsets ")
                        stop(paste0(orthograph, paste0(subsets, collapse = ", "), " not found."))

                    }
                } else {
                    stop("subsets argument must be of class \"numeric\" or \"character\".")
                }
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

## Adding dimnames (if necessary)
add.dimnames <- function(one_output, one_subset, data) {
    input <- data$matrix[[1]][data$subsets[[one_subset]]$elements, data$call$dimensions]
    return(
        switch(as.character(sum(which(dim(input) %in% length(one_output)))),
                ## No matching dim
                "0" = one_output,
                ## Matching rows
                "1" = {names(one_output) <- rownames(input); one_output},
                ## Matching cols
                "2" = {names(one_output) <- colnames(input); one_output},
                ## Matching both (use rows as default)
                "3" = {names(one_output) <- rownames(input); one_output}
            )
        )
}