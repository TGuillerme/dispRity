## Make the factors for the dispRity object
make.factors <- function(data, group_names, group_variables, time_subsets, pool_time) {
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

        test <- try( factors_out <- data.frame(group = get.group.factors(one_group_variable, factors), row.names = rownames(data$matrix[[1]])), silent = TRUE)

        if(class(test) == "try-error") {
            ## Deal with NAs down the line
            factors_out <- make.time.factor(data, pool = TRUE, time = FALSE)

        } else {
            return(factors_out)
        }
    }
    

    ## Make time factor
    make.time.factor <- function(data, pool = FALSE, time = TRUE) {

        if(time) {
            colnames <- "time"
        } else {
            colnames <- "group"
        }

        ## Get the time data
        time_data <- lapply(data$subsets, function(X) return(X$elements))
        
        ## Individual time series
        make.time.series <- function(one_time_subset, data) {
            ## Generate the time series
            time_series <- matrix(FALSE, ncol = 1, nrow = nrow(data$matrix[[1]]), dimnames = list(rownames(data$matrix[[1]])))
            time_series[as.vector(one_time_subset), 1] <- TRUE
            return(time_series)
        }
        ## Get the data for each time series
        groups <- lapply(time_data, make.time.series, data)
        groups <- do.call(cbind, groups)

        ## Naming the groups
        if(time) {
            colnames(groups) <- paste0("t", gsub(" - ", "to", names(time_data)))
        } else {
            colnames(groups) <- names(time_data)
        }

        if(pool) {
            ## Translating the data
            groups <- t(apply(groups, 1, function (X) ifelse(X, names(X), NA)))
            groups <- data.frame(matrix(as.factor(apply(groups, 1, function(row) return(na.omit(row)[1]))), ncol = 1, dimnames = list(rownames(data$matrix[[1]]), colnames)))
        } else {
            ## Binarise the data
            if(time) {
                groups <- data.frame(ifelse(groups, 1, 0), stringsAsFactors = TRUE)
            }
            for(col in 1:ncol(groups)) {
                groups[,col] <- factor(groups[,col])
            }
            if(time) {
                colnames(groups) <- paste0("t", gsub(" - ", "to", names(time_data)))
            } else {
                colnames(groups) <- names(time_data)
            }
        }
        
        return(groups)
    }


    if(length(group_names) == 1) {
        if(time_subsets) {
            ## Time groups
            return(make.time.factor(data, pool = pool_time))
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
