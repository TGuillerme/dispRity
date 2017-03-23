check.data.to.merge <- function(data) {
    if(class(data) != "dispRity") {
        #data must be a matrix
        check.class(data, 'matrix')
        #nrow_data variable declaration
        nrow_data <- nrow(data)
        #data must be of size k*<=k-1
        if(ncol(data) > (nrow_data - 1)) stop("Input data must have at maximum k-1 columns")
    } else {
        check.length(data, 3, " must be a dispRity subsamples object output from time.subsamples().")
    }
}

warn.merge <- function(data, subsamples, merge) {
    message("The interval ", names(data$data)[subsamples], " was merged with ", names(data$data)[merge], " because it had less than 3 elements.")
}

merge.subsamples <- function(data, subsamples, merge) {
    # Target subsamples (where to merge)
    matrix_merge <- data$data[[merge]]
    name_merge <- data$subsamples[[merge+1]]

    # subsamples to remove
    matrix_remove <- data$data[[subsamples]]
    name_remove <- data$subsamples[[subsamples+1]]
    # Removing redundant rownames (if any!)
    redundants <- which(!is.na(match(rownames(matrix_remove), rownames(matrix_merge))))
    if(length(redundants) != 0) {
        matrix_remove <- matrix_remove[-redundants,]
    }

    # Merging data
    data$data[[merge]] <- rbind(matrix_merge, matrix_remove)
    data$data[[subsamples]] <- NULL

    # Renaming subsamples
    if(subsamples < merge) {
        names(data$data)[[subsamples]] <- paste(name_remove, name_merge, sep = ";")
        data$subsamples[[subsamples+1]] <- paste(name_remove, name_merge, sep = ";")
        data$subsamples <- data$subsamples[-(merge+1)]
    } else {
        names(data$data)[[merge]] <- paste(name_merge, name_remove, sep = ";")
        data$subsamples[[merge+1]] <- paste(name_merge, name_remove, sep = ";")
        data$subsamples <- data$subsamples[-(subsamples+1)]
    }
    return(data)
}

clean.subsamples <- function(data1, after) {
    # Looping through each element (iterative process!)
    interval <- 1
    while(interval <= length(data1$data)) {
        # Check if there are enough elements
        if(nrow(data1$data[[interval]]) < 3) {
            # Merge with the next subsamples
            if(after) {
                # Check if the next subsamples is not last!
                if(interval != length(data1$data)) {
                    warn.merge(data1, interval, interval+1)
                    data1 <- merge.subsamples(data1, interval, interval+1)
                } else {
                    warn.merge(data1, interval, interval-1)
                    data1 <- merge.subsamples(data1, interval, interval-1)
                }
            } else {
                #Check if the subsamples is not first!
                if(interval != 1) {
                    warn.merge(data1, interval, interval-1)
                    data1 <- merge.subsamples(data1, interval, interval-1)
                } else {
                    warn.merge(data1, interval, interval+1)
                    data1 <- merge.subsamples(data1, interval, interval+1)
                }
            }
        }
        #Increment interval
        interval <- interval + 1
    }
    return(data1)
}