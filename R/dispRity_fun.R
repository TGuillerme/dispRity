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

#lapply fun
lapply_fun<-function(data, fun, ...) {
    return(lapply(data, fun, ...))
}

# #Calculates disparity metrics
# disparity.calc<-function(BSresult, class.metric, summary.metric) {
#     matrix_descriptor<-lapply(BSresult, lapply_fun, fun=class.metric)
#     matrix_summary<-lapply(matrix_descriptor, lapply_fun, fun=summary.metric)
#     return(matrix_summary)
# }


disparity.calc<-function(BSresult, level3.fun, level2.fun, level1.fun, ...) {
    #Run level 3 fun (matrix transformation - mat.trans)
    if(!is.null(level3.fun)) {
        matrix_decomposition <- lapply(BSresult, lapply_fun, fun = level3.fun, ...)
    } else {
        matrix_decomposition <- BSresult
    }

    #Run level 2 fun (vector aggregate - vec.aggr)
    if(!is.null(level2.fun)) {
        matrix_decomposition <- lapply(matrix_decomposition, lapply_fun, fun = level2.fun, ...)
    } else {
        matrix_decomposition <- matrix_decomposition
    }

    #Run level 1 fun (vector aggregate - vec.aggr)
    if(!is.null(level1.fun)) {
        matrix_decomposition <- lapply(matrix_decomposition, lapply_fun, fun = level1.fun, ...)
    } else {
        matrix_decomposition <- matrix_decomposition
    }

    return(matrix_decomposition)
}
