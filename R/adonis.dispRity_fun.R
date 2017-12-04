
## Checking if the matrix is a distance matrix
check.dist.matrix <- function(matrix, method) {

    ## Is the matrix square?
    if(dim(matrix)[1] == dim(matrix)[2]) {
        ## Check if the diagonal is equal to 0
        if(all(diag(matrix) == 0)) {
            ## Check if both triangles are equal
            if(all(matrix[upper.tri(matrix)] == matrix[rev(lower.tri(matrix))])) {
                return(list(stats::as.dist(matrix), "was_dist" = TRUE))
            } else {
                return(list(stats::dist(matrix, method = method), "was_dist" = FALSE))
            }
        } else {
            return(list(stats::dist(matrix, method = method), "was_dist" = FALSE))
        }
    } else {
        return(list(stats::dist(matrix, method = method), "was_dist" = FALSE))
    }
}

## Make the factors for the dispRity object
make.factors <- function(data, group_names, group_variables, time_subsets) {
    ## Extracting the factors


    ## Getting one group factor
    get.group.factors <- function(one_group_variable, factors) {
        for(group in 1:length(one_group_variable)) {
            factors[grep(one_group_variable[group], factors)] <- one_group_variable[group]
        } 
        return(factors)
    }

    ## Output one factor
    output.factor <- function(factors, one_group_variable, data) {
        return(data.frame(group = get.group.factors(one_group_variable, factors), row.names = rownames(data$matrix)))
    }


    ## Make time factor
    make.time.factor <- function(data, pool = FALSE) {
        ## Get the time data
        time_data <- lapply(data$subsets, function(X) return(X$elements))

        ## Individual time series
        make.time.series <- function(one_time_subset, data) {
            ## Generate the time series
            time_series <- matrix(FALSE, ncol = 1, nrow = nrow(data$matrix), dimnames = list(rownames(data$matrix)))
            time_series[as.vector(one_time_subset), 1] <- TRUE
            return(time_series)
        }
        ## Get the data for each time series
        groups <- lapply(time_data, make.time.series, data)
        groups <- do.call(cbind, groups)

        ## Naming the groups
        colnames(groups) <- names(time_data)

        if(pool) {
            ## Translating the data
            groups <- t(apply(groups, 1,  function (X) ifelse(X, names(X), NA)))
            groups <- matrix(apply(groups, 1, function(row) return(na.omit(row)[1])), ncol = 1, dimnames = list(rownames(data$matrix), "time"))
        } 
        
        return(groups)
    }


    if(length(group_names) == 1) {
        if(time_subsets) {
            ## Time groups
            return(make.time.factor(data))
        } else {
            ## Normal groups
            ## Extract all subsets as a factor list
            factors <- names(sort(unlist(lapply(data$subsets, function(X) return(X$elements)))))
            ## Single group
            return(output.factor(factors, group_variables, data))
        }
    } else {
        ## Extract all subsets
        subsets <- lapply(data$subsets, function(X) return(X$elements))
        ## Extract the factor list
        lapply.subsets <- function(group, subsets) {
            names(sort(unlist(sapply(group, function(X) return(subsets[[grep(X, names(subsets))]]), simplify = FALSE))))
        }
        factors <- lapply(group_variables, lapply.subsets, subsets)
        ## Multiple groups
        groups_out <- do.call(cbind, mapply(output.factor, factors, group_variables, MoreArgs = list(data = data), SIMPLIFY = FALSE))
        ## Name the groups
        colnames(groups_out) <- group_names
        return(groups_out)
    }
}
