#FUNCTIONS FOR DISPRITY


get.dispRity.data.handle<-function(data_fetch) {
    is.bootstrapped <- NULL
    prev_info <- NULL
    disparity.exists <- NULL
    taxa_list <- NULL
    series_list <- NULL
    series_type <- NULL
    data <- NULL
    boot.call <- NULL
    BSresult <- NULL

    if(class(data_fetch) != "dispRity") {
        #Data is not bootstrapped
        is.bootstrapped <- FALSE
        prev_info <- FALSE
        disparity.exists <- FALSE
        data <- data

    } else {
        prev_info <- TRUE

        #length_data_fetch variable initialisation
        length_data_fetch <- length(data_fetch)

        #If length is 3, no bootstrap, just time series
        if(length_data_fetch == 3) {
            #Data is not bootstrapped
            is.bootstrapped <- FALSE
            #Extracting the info
            prev_info <- TRUE
            taxa_list <- data_fetch$elements
            series_list <- data_fetch$series[-1]
            series_type <- data_fetch$series[1]
            data <- data_fetch$data
        }

        #If length is 4, bootstrap (+ time series?)
        if(length_data_fetch == 4) {
            #Data is bootstrapped
            is.bootstrapped <- TRUE
            #Extracting the info
            BSresult <- data_fetch$data$bootstraps
            data <- data_fetch$data$observed
            boot.call <- data_fetch$call
            taxa_list <- data_fetch$elements
            series_list <- data_fetch$series
        }

        #Disparity was already calculated
        if(length(match(names(data_fetch), c("data", "disparity", "elements", "series", "call"))) == 5) {
            disparity.exists <- TRUE
            is.bootstrapped <- ifelse(length(data$disparity$bootstrapped) != 0, TRUE, FALSE)
        } else {
            disparity.exists <- FALSE
        }
    }


    #Checking the matrix list (if bs=F)
    if(is.bootstrapped != TRUE && disparity.exists != TRUE) {

        #If matrix, transform to list
        if(class(data_fetch) == "matrix") {
            data <- list(data_fetch)
        }

        #Must be a list
        check.class(data, "list", " must be a matrix or a list of matrices.")
        #Each matrix must have the same number of columns
        mat_columns <- unique(unlist(lapply(data, ncol)))
        if(length(mat_columns) != 1) stop("Some matrices in data have different number of columns.")
        #Making sure there is at least 3 rows per element
        if(any(unlist(lapply(data, nrow) < 3))) stop("Some matrices in data have less than 3 rows.")

        #Setting the info
        if(prev_info == FALSE) {
            taxa_list <- unlist(lapply(data, rownames))
            names(taxa_list) <- NULL
            series_list <- names(data)
            if(is.null(series_list)) {
                series_list <- length(data)
            }            
        }

        #Make the data bootstrap results format (0 bootstrap)
        BSresult <- list(list(data))
        #BSresult <- boot.matrix(data, bootstraps = 0, rarefaction = FALSE, rm.last.axis = FALSE, verbose = FALSE, boot.type = "full")$data$bootstraps
    }

    return(list("is.bootstrapped" = is.bootstrapped, "prev_info" = prev_info, "disparity.exists" = disparity.exists, "taxa_list" = taxa_list, "series_list" = series_list, "series_type" = series_type, "data" = data, "boot.call" = boot.call, "BSresult" = BSresult))
}


get.dispRity.metric.handle<-function(metric) {
    level3.fun <- NULL
    level2.fun <- NULL
    level1.fun <- NULL

    #length_metric variable initialisation
    length_metric <- length(metric)  

    #must be at least one metric
    if(length_metric < 1) {
        stop("At least one metric must be provided.")
    }
    
    if(length_metric != 1) {

        #Check all the metrics
        for(i in 1:length_metric) {
            if(class(metric[[i]]) != "function") stop(paste("Error in metric argument: ",match_call$metric[[i+1]], " is not a function!", sep = ""))
        }

        #Sorting the metrics by levels
        #getting the metric levels
        levels <- unlist(lapply(metric, make.metric, silent=TRUE))
        #can only unique levels
        if(length(levels) != length(unique(levels))) stop("Some functions in metric are the same of the same level.\nTry combining them in a single function.\nFor more information, see:\n?make.metric()")

        #At least one level 1 or level 2 metric is required
        if(length(levels == 1) && levels == "level3") {
            stop("At least one metric must be level 1 or level 2\n.For more information, see:\n?make.metric()")
        }
        
        #Get the level 1 metric
        if(!is.na(match("level1", levels))) {
            level1.fun <- metric[[match("level1", levels)]]
        } else {
            #is null if doesn't exist
            level1.fun <- NULL
        }
        #Get the level 2 metric
        if(!is.na(match("level2", levels))) {
            level2.fun <- metric[[match("level2", levels)]]
        } else {
            #is null if doesn't exist
            level2.fun <- NULL
        }
        #Get the level 3 metric
        if(!is.na(match("level3", levels))) {
            level3.fun <- metric[[match("level3", levels)]]
        } else {
            #is null if doesn't exist
            level3.fun <- NULL
        }

    } else {

        #Metric was still fed as a list
        if(class(metric) == "list") {
            check.class(metric[[1]], "function")
            metric <- metric[[1]]
        } else {
            #Metric was fed as a single element
            check.class(metric, "function")
        }

        #Getting the metric level
        levels <- make.metric(metric, silent=TRUE)
        #Metric must not be level 3
        if(levels == "level3") {
            stop(paste(match_call$metric, " must contain at least a level 1 or a level 2 metric. For more information, use:\nmake.metric(",match_call$metric,")", sep = ""))
        }
        # Set the metric level
        if(levels == "level2") {
            level3.fun = NULL; level2.fun = metric; level1.fun = NULL
        } else {
            level3.fun = NULL; level2.fun = NULL; level1.fun = metric
        }
    }

    return(list("level3.fun" = level3.fun, "level2.fun" = level2.fun, "level1.fun" = level1.fun))
}

disparity.calc<-function(BSresult, level3.fun, level2.fun, level1.fun, ...) {

    lapply_fun<-function(data, fun, ...) {
        return(lapply(data, fun, ...))
    }

    matrix_decomposition <- ifelse(!is.null(level3.fun), list(lapply(BSresult, lapply_fun, level3.fun, ...)), list(BSresult))
    #matrix_decomposition <- ifelse(!is.null(level3.fun), list(lapply(BSresult, lapply_fun, level3.fun)), list(BSresult)) ; warning("DEBUG")

    matrix_decomposition <- ifelse(!is.null(level2.fun), list(lapply(matrix_decomposition[[1]], lapply_fun, level2.fun, ...)), matrix_decomposition)
    #matrix_decomposition <- ifelse(!is.null(level2.fun), list(lapply(matrix_decomposition[[1]], lapply_fun, level2.fun)), matrix_decomposition) ; warning("DEBUG")

    matrix_decomposition <- ifelse(!is.null(level1.fun), list(lapply(matrix_decomposition[[1]], lapply_fun, level1.fun, ...)), matrix_decomposition)
    #matrix_decomposition <- ifelse(!is.null(level1.fun), list(lapply(matrix_decomposition[[1]], lapply_fun, level1.fun)), matrix_decomposition) ; warning("DEBUG")

    return(matrix_decomposition[[1]])
}
