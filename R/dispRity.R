#' @title Calculates disparity from an ordinated matrix.
#'
#' @description Calculates disparity on an ordinated matrix or series of matrices, where the disparity metric can be user specified.
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)}, a list of matrices (typically output from the functions \code{\link{time.series}} or \code{\link{cust.series}}) or a bootstrapped matrix output from \code{\link{boot.matrix}}.
#' @param metric A vector containing up to three functions and at least a level 1 or 2 function (see details).
#' @param ... Optional arguments to be passed to the metric.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param parallel An optional vector containing the number of parallel threads and the virtual connection process type to run the function in parallel (requires \code{\link[snow]{snow}} package; see \code{\link[snow]{makeCluster}} function).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the observed and bootstrapped matrices.}
#' \item{disparity}{A \code{list} of disparity values.}
#' \item{elements}{A \code{vector} containing all the names of the elements from the original matrix.}
#' \item{series}{A \code{vector} containing the name of the series (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstrapping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @details  
#' \code{metric} should be input as a vector of functions.
#' The functions are sorted and used by "level" from "level 3" to "level 1" (see \code{\link{dispRity.metric}} and
#' \code{\link{make.metric}}). Typically "level 3" functions intake a \code{matrix} and outputs a \code{matrix};
#' level2 functions intake a \code{matrix} and output a \code{vector} and "level 1" functions intake a
#' \code{matrix} or a \code{vector} and output a single value.
#' Some metric functions are inbuilt in the \code{dispRity} package: see \code{\link{dispRity.metric}}. For
#' user specified metrics, please use \code{\link{make.metric}} to ensure if the metric works.
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity as the sum of variances from a single matrix
#' sum_of_variances <- dispRity(BeckLee_mat50, metric = c(sum, variances))
#' summary(sum_of_variances)
#' ## Bootstrapping this value
#' bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps = 100)
#' dispRity(bootstrapped_data, metric = c(sum, variances))
#'
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#' summary(sum_of_variances)
#'
#' \dontrun{
#' ## Calculating disparity using one thread
#' system.time(dispRity(bootstrapped_data, metric = c(sum, variances)))
#' ## Bootstrapping a series of matrices using 4 threads
#' system.time(dispRity(bootstrapped_data, metric = c(sum, variances), parallel = c(4, "SOCK")))
#' # System time is significantly longer! Using parallel is only an improvement for big datasets.
#' }
#' 
#' @seealso \code{\link{boot.matrix}} for bootstrapping the data; \code{\link{dispRity.metric}} for details on the implemented metrics and \code{\link{summary.dispRity}} for summarising \code{dispRity} objects.
#'
#' @author Thomas Guillerme

dispRity<-function(data, metric, ..., verbose=FALSE, parallel) {
    #----------------------
    # SANITIZING
    #----------------------
    
    #Saving the call
    match_call<-match.call()

    #DATA
    #Check if the input is a dispRity object
    data_fetch<-data
    if(class(data_fetch) == "dispRity") {

        #If length is 3, no bootstrap, just time series
        if(length(data_fetch) == 3) {
            #Data is not bootstrapped
            is.bootstraped<-FALSE
            #Extracting the info
            prev_info<-TRUE
            taxa_list<-data_fetch$elements
            series_list<-data_fetch$series[-1]
            series_type<-data_fetch$series[1]
            data<-data_fetch$data
        }

        #If length is 4, bootstrap (+ time series?)
        if(length(data_fetch) == 4) {
            #Data is bootstrapped
            is.bootstraped<-TRUE
            #Extracting the info
            BSresult<-data_fetch$data$bootstraps
            data<-data_fetch$data$observed
            boot.call<-data_fetch$call
            taxa_list<-data_fetch$elements
            series_list<-data_fetch$series
        }

    } else {
        #Data is not bootstrapped
        is.bootstraped<-FALSE
        prev_info<-FALSE
    }

    #Checking the matrix list (if bs=F)
    if(is.bootstraped == FALSE) {

        #If matrix, transform to list
        if(class(data) == "matrix") {
            data<-list(data)
        }

        #Must be a list
        check.class(data, "list", " must be a matrix or a list of matrices.")
        #Each matrix must have the same number of columns
        mat_columns<-unique(unlist(lapply(data, ncol)))
        if(length(mat_columns) != 1) stop("Some matrices in data have different number of columns.")
        #Making sure there is at least 3 rows per element
        if(any(unlist(lapply(data, nrow) < 3))) stop("Some matrices in data have less than 3 rows.")

        #Setting the info
        if(prev_info == FALSE) {
            taxa_list<-unlist(lapply(data, rownames))
            names(taxa_list)<-NULL
            series_list<-names(data)
            if(is.null(series_list)) {
                series_list<-length(data)
            }            
        }

        #Make the data bootstrap results format (0 bootstrap)
        BSresult<-boot.matrix(data, bootstraps=0, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full")$data$bootstraps
    }

    #METRIC
    #must be at least one metric
    if(length(metric) < 1) {
        stop("At least one metric must be provided.")
    }
    #must be maximum 3
    if(length(metric) > 3) {
        stop("Only three functions can be used for metric.\nYou can try transforming one of the functions to do more than one operation.")
    }
    #Making the list of metrics for testing
    if(length(metric) == 1) {
        check.class(metric, "function")
    } else {
        for(i in 1:length(metric)) {
            check.class(metric[[i]], "function")
        }
    }

    #Sorting the metrics by levels
    if(length(metric) == 1) {
        #Getting the metric level
        metric_level <- make.metric(metric, silent=TRUE)
        #Metric must be level1
        if(metric_level != "level1") {
            stop(paste(match_call$metric, " must be a 'level1' metric. For more information, use:\nmake.metric(",match_call$metric,")", sep=""))
        } else {
            level3.fun=NULL; level2.fun=NULL; level1.fun=metric
        }
    } else {
        #getting the metric levels
        levels <- unlist(lapply(metric, make.metric, silent=TRUE))
        #can only unique levels
        if(length(levels) != length(unique(levels))) stop("Some functions in metric are the same 'level'. For more information, see:\n?make.metric()")
        if(is.na(match("level1", levels))) {
            #At least one level1 metric is required
            stop("At least one metric must be 'level1'. For more information, see:\n?make.metric()")
        } else {
            #Get the level 1 metric (mandatory)
            level1.fun <- metric[[match("level1", levels)]]
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
        }
    }

    #VERBOSE
    check.class(verbose, "logical")
    # ~~~
    # Use progress bars?
    # ~~~
        #total <- 20
        ## create progress bar
        #pb <- txtProgressBar(min = 0, max = total, style = 3)
        #for(i in 1:total){
        #Sys.sleep(0.1)
        ## update progress bar
        #setTxtProgressBar(pb, i)
        #}
        #close(pb)

    #Parallel
    if(missing(parallel)) {
        do_parallel <- FALSE
    } else {
        do_parallel <- TRUE
        check.length(parallel, 2, " must be a vector containing the number of threads and the virtual connection process type.")
        check.class(as.numeric(parallel[1]), "numeric", " must be a vector containing the number of threads and the virtual connection process type.")
        check.class(parallel[2], "character", " must be a vector containing the number of threads and the virtual connection process type.")
        cluster <- makeCluster(as.numeric(parallel[1]), parallel[2])
    }

    #----------------------
    #CALCULTING DISPARITY
    #----------------------
    #verbose
    if(verbose==TRUE) message("Calculating disparity...", appendLF=FALSE)
    #Calculate disparity in all the series
    if(do_parallel == FALSE) {
        results<-lapply(BSresult, disparity.calc, level3.fun=level3.fun, level2.fun=level2.fun, level1.fun=level1.fun, ...)
    } else {
        results<-parLapply(cluster, BSresult, disparity.calc, level3.fun=level3.fun, level2.fun=level2.fun, level1.fun=level1.fun, ...)
        stopCluster(cluster)
    }
    
    #if data is bootstrapped, also calculate the observed disparity
    if(is.bootstraped == TRUE) {
        OBSresults<-lapply(data_fetch$data$observed, disparity.calc, level3.fun=level3.fun, level2.fun=level2.fun, level1.fun=level1.fun, ...)
    }
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=FALSE)

    #----------------------
    #OUTPUT
    #----------------------
    #call details
    dispRity.call<-paste("Disparity calculated as: ", as.expression(match_call$metric), " for ", ncol(BSresult[[1]][[1]][[1]]) ," dimensions.", sep="")
    #Add BS (and series) details
    if(is.bootstraped == TRUE) {
        dispRity.call<-paste(dispRity.call, boot.call, sep="\n")
    } else {
        if(prev_info == TRUE) {
            dispRity.call<-paste(dispRity.call, "\nData was split using ", series_type, " method.", sep="")
        }
    }


    #Creating the output object
    if(is.bootstraped == TRUE) {
        output<-list("data"=list("bootstraps"=BSresult, "observed"=data) , "disparity"=list("bootstrapped"=results, "observed"=OBSresults), "elements"=taxa_list, "series"=series_list, "call"=dispRity.call)
    } else {
        output<-list("data"=list("observed"=data), "disparity"=list("observed"=results), "elements"=taxa_list, "series"=series_list, "call"=dispRity.call)
    }
    #Output object is a dispRity object
    class(output)<-"dispRity"

    return(output)
}

