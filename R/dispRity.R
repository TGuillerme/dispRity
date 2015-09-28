#' @title Calculates disparity from an ordinated matrix.
#'
#' @description Calculates disparity on an ordinated matrix or series of matrices, where the disparity metric can be user specified.
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)}, a list of matrices (typically output from the functions \code{\link{time.series}} or \code{\link{cust.series}}) or a boostrapped matrix output from \code{\link{boot.matrix}}.
#' @param metric A vector containing the class metric and the summary metric (see details).
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{bootstraps}{A \code{list} of boostraped matrices.}
#' \item{disparity}{A \code{list} of disparity values.}
#' \item{taxa}{A \code{vector} containing all the names of the taxa from the original matrix.}
#' \item{series}{A \code{vector} containing the name of the series (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstraping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @details  
#' \code{metric} should be input as a vector containing both the class and the summary metric.
#' The class metric is the descriptor of the matrix (e.g. ranges of the dimensions or distance between taxa and centroid).
#' The summary metris is the metric used to summarise the matrix descriptor (e.g. the sum of the ranges of the dimensions or the median distance between taxa and centroid).
#' For example use \code{metric = c(sum, range)} for using the sum of the ranges of the dimensions as a disparity metric.
#' Details on the implemented metric (class and summary) can be found at \code{\link{dispRity.metric}}.
#' Note that metrics can be also user specified.
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity as the sum of ranges from a single matrix
#' sum_of_ranges<-dispRity(BeckLee_mat50, metric = c(sum, range))
#' summary(sum_of_ranges)
#' ## Bootstraping this value
#' bootstrapped_data <- boot.matrix(BeckLee_mat50, bootstraps=100)
#' dispRity(bootstrapped_data, metric=c(sum, range))
#'
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
#' ## Caculating the sum of ranges
#' sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, range))
#' summary(sum_of_ranges)
#'


#' ## Setting the data
#' ## Generate 5 equidistant time slices in the data set assuming gradual evolutionary models
#' sliced_data <- time.series(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = 5, FADLAD = BeckLee_ages)
#' bootstrapped_data <- boot.matrix(sliced_data, bootstraps = 20, rarefaction = TRUE)
#' sum_of_ranges <- dispRity(bootstrapped_data, metric = c(sum, range))



#' @seealso \code{\link{boot.matrix}} for bootstraping the data; \code{\link{dispRity.metric}} for details on the implemented metrics and \code{\link{summary.dispRity}} for summarising \code{dispRity} objects.
#'
#' @author Thomas Guillerme

dispRity<-function(data, metric, verbose=FALSE) {
    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #Check if the input is a dispRity object
    if(class(data) == "dispRity") {

        #If length is 3, no bootstrap, just time series
        if(length(data) == 3) {
            #Data is not bootstrapped
            is.bootstraped<-FALSE
            #Extracting the info
            prev_info<-TRUE
            taxa_list<-data$taxa
            series_list<-data$series[-1]
            series_type<-data$series[1]
            data<-data$data
        }

        #If length is 4, bootstrap (+ time series?)
        if(length(data) == 4) {
            #Data is bootstrapped
            is.bootstraped<-TRUE
            #Extracting the info
            BSresult<-data$bootstraps
            boot.call<-data$call
            taxa_list<-data$taxa
            series_list<-data$series
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
        BSresult<-boot.matrix(data, bootstraps=0, rarefaction=FALSE, rm.last.axis=FALSE, verbose=FALSE, boot.type="full")$bootstrap        
    }

    #METRIC
        # ~~~~
        # Make summary metric is not mandatory
        # ~~~~
    #must be a vector of two elements
    check.length(metric, 2, " must be a vector of two elements, the first being the metric class and the second being the metric summary.")
    #both elements must be functions
    check.class(metric[[1]], "function")
    check.class(metric[[2]], "function")
    #both functions must work
    metric1<-check.metric(metric[[1]])
    metric2<-check.metric(metric[[2]])
    #Both functions must be of different type
    if(metric1 == metric2) stop("One metric must be a class metric and the other a summary metric.")
    #Assigning each metric to it's type
    if(metric1 == "class.metric") {
        class.metric<-metric[[1]] ; summary.metric<-metric[[2]] 
    } else {
        class.metric<-metric[[2]] ; summary.metric<-metric[[1]] 
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

    #Saving the call
    match_call<-match.call()

    #----------------------
    #CALCULTING DISPARITY
    #----------------------
    #verbose
    if(verbose==TRUE) message("Calculating disparity...", appendLF=FALSE)
    #Calculate disparity in all the series
    results<-lapply(BSresult, disparity.calc, class.metric, summary.metric)
    #verbose
    if(verbose==TRUE) message("Done.", appendLF=FALSE)

    #----------------------
    #OUTPUT
    #----------------------
    #call details
    dispRity.call<-paste("Disparity calculated as: ", match_call$metric[[2]]," ", match_call$metric[[3]], " for ", ncol(BSresult[[1]][[1]][[1]]) ," dimensions.", sep="")
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
        output<-list("bootstraps"=BSresult, "disparity"=results, "taxa"=taxa_list, "series"=series_list, "call"=dispRity.call)
    } else {
        output<-list("matrix"=data, "disparity"=results, "taxa"=taxa_list, "series"=series_list, "call"=dispRity.call)
    }
    #Output object is a dispRity object
    class(output)<-"dispRity"

    return(output)
}

