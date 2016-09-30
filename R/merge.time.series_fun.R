check.data.to.merge <- function(data) {
    if(class(data) != "dispRity") {
        #data must be a matrix
        check.class(data, 'matrix')
        #nrow_data variable declaration
        nrow_data <- nrow(data)
        #data must be of size k*<=k-1
        if(ncol(data) > (nrow_data - 1)) stop("Input data must have at maximum k-1 columns")
    } else {
        check.length(data, 3, " must be a dispRity series object output from time.series().")
    }
}

warn.merge <- function(data, series, merge) {
    message("The interval ", names(data$data)[series], " was merged with ", names(data$data)[merge], " because it had less than 3 elements.")
}

merge.series <- function(data, series, merge) {
    # Target series (where to merge)
    matrix_merge <- data$data[[merge]]
    name_merge <- data$series[[merge+1]]

    # Series to remove
    matrix_remove <- data$data[[series]]
    name_remove <- data$series[[series+1]]
    # Removing redundant rownames (if any!)
    redundants <- which(!is.na(match(rownames(matrix_remove), rownames(matrix_merge))))
    if(length(redundants) != 0) {
        matrix_remove <- matrix_remove[-redundants,]
    }

    # Merging data
    data$data[[merge]] <- rbind(matrix_merge, matrix_remove)
    data$data[[series]] <- NULL

    # Renaming series
    if(series < merge) {
        names(data$data)[[series]] <- paste(name_remove, name_merge, sep = ";")
        data$series[[series+1]] <- paste(name_remove, name_merge, sep = ";")
        data$series <- data$series[-(merge+1)]
    } else {
        names(data$data)[[merge]] <- paste(name_merge, name_remove, sep = ";")
        data$series[[merge+1]] <- paste(name_merge, name_remove, sep = ";")
        data$series <- data$series[-(series+1)]
    }
    return(data)
}
