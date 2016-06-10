#' @title Calculates disparity from an ordinated matrix.
#'
#' @description Calculates disparity on an ordinated matrix or series of matrices, where the disparity metric can be user specified.
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)}, or a \code{dispRity} object (see details).
#' @param metric A vector containing one to three functions. At least of must be a "level 1" or a "level 2" function (see details).
#' @param ... Optional arguments to be passed to the metric.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param parallel An optional vector containing the number of parallel threads and the virtual connection process type to run the function in parallel (requires \code{\link[snow]{snow}} package; see \code{\link[snow]{makeCluster}} function).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the observed and bootstrapped matrices.}
#' \item{disparity}{A \code{list} of disparity values (containing the observed disparity and the eventual bootstrapped one).}
#' \item{elements}{A \code{vector} containing all the names of the elements from the original matrix.}
#' \item{series}{A \code{vector} containing the name of the series (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstrapping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @details  
#' The \code{dispRity} object given to the \code{data} argument can be: a list of matrices (typically output from the functions \code{\link{time.series}} or \code{\link{cust.series}}), a bootstrapped matrix output from \code{\link{boot.matrix}} or a list of disparity measurements calculated from this \code{dispRity} function.
#' 
#' \code{metric} should be input as a vector of functions.
#' The functions are sorted and used by "level" from "level 3" to "level 1" (see \code{\link{dispRity.metric}} and \code{\link{make.metric}}).
#' Typically "level 3" functions intake a \code{matrix} and output a \code{matrix}; level2 functions intake a \code{matrix} and output a \code{vector} and "level 1" functions intake a \code{matrix} or a \code{vector} and output a single value.
#' When more than one function is input, they are treated first by level (i.e. level 3, then level 2 and finally level 1).
#' Note that the functions can only take one metric of each level and thus can only take a maximum of three arguments!
#' 
#' Some metric functions are inbuilt in the \code{dispRity} package: see \code{\link{dispRity.metric}}
#' For user specified metrics, please use \code{\link{make.metric}} to ensure that the metric will work.
#' 
#' \emph{HINT:} for using more than three functions you can always create your own function that uses more than one function (e.g. \code{my_function <- function(matrix) cor(var(matrix))} is perfectly valid and allows to use two level 3 functions - the correlation of the variance-covariance matrix in this case).
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
#' ## Calculating disparity with different metrics levels
#' ## Disparity is calculated as the distribution of the variances in each
#' ## dimensions (output are distributions)
#' disparity_level2 <- dispRity(BeckLee_mat50, metric = variances)
#' ## Disparity is calculated as the mean of the variances in each dimensions 
#' ## (output are single values)
#' disparity_level1 <- dispRity(disparity_level2, metric = mean)
#' ## Both disparity have the same means but level 1 has no quantiles
#' summary(disparity_level2)
#' summary(disparity_level1)
#'
#' \dontrun{
#' ## Calculating disparity using one thread
#' system.time(dispRity(bootstrapped_data, metric = c(sum, variances)))
#' ## Bootstrapping a series of matrices using 4 threads
#' system.time(dispRity(bootstrapped_data, metric = c(sum, variances),
#'      parallel = c(4, "SOCK")))
#' ## System time is significantly longer! Using parallel is only an improvement
#' ## for big datasets.
#' }
#' 
#' @seealso \code{\link{cust.series}}, \code{\link{time.series}}, \code{\link{boot.matrix}}, \code{\link{dispRity.metric}}, \code{\link{summary.dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme

#For testing
#source("sanitizing.R")
#source("dispRity_fun.R")

dispRity<-function(data, metric, ..., verbose=FALSE, parallel) {
    #----------------------
    # SANITIZING
    #----------------------
    
    #Saving the call
    match_call <- match.call()
    #return(match_call) ; warning("DEBUG")

    #Get the data handle
    data_handle <- get.dispRity.data.handle(data)

    #Get the metric handle
    metric_handle <- get.dispRity.metric.handle(metric)

    #Stop if data already contains disparity and metric is not level1
    if(!is.null(metric_handle$level3.fun) && data_handle$disparity.exists != FALSE) {
        stop("Impossible to apply a level 3 metric on disparity data.")
    }

    #VERBOSE
    check.class(verbose, "logical")
    # ~~~
    # Use progress bars?
    # ~~~
        # total <- 20
        # # create progress bar
        # pb <- txtProgressBar(min = 0, max = total, style = 3)
        # for(i in 1:total){
        # Sys.sleep(0.1)
        # # update progress bar
        # setTxtProgressBar(pb, i)
        # }
        # close(pb)

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

    #If disparity already exists, export the data
    if(data_handle$disparity.exists != FALSE) {
        #Recalculating the observed disparity
        data$disparity$observed <- lapply(data$disparity$observed, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun, ...)
        #data$disparity$observed <- lapply(data$disparity$observed, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun) ; warning("DEBUG")
        if(data_handle$is.bootstrapped != FALSE) {
            data$disparity$bootstrapped <- lapply(data$disparity$bootstrapped, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun, ...)
            #data$disparity$bootstrapped <- lapply(data$disparity$bootstrapped, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun) ; warning("DEBUG")
        }
        #Updating the call
        #Querying the metrics from former call
        call_split <- strsplit(strsplit(data$call, split = "Disparity calculated as: ")[[1]][2], split = " for ")[[1]]
        
        #Generating the new call
        if(length(length(match_call$metric)) != 1) {
            new_call <- paste("c(", paste(as.character(match_call$metric[-1]), collapse = ", ", sep = ""), ", ", call_split[1], ")", collapse = "", sep = "")
        } else {
            new_call <- paste("c(", match_call$metric, ", ", call_split[1], ")", collapse = "")
        }

        #Saving the new call
        data$call <- paste(c("Disparity calculated as: ", new_call, " for ", call_split[2]), collapse = "")
        #Returning the results
        return(data)
    }

    #verbose
    if(verbose != FALSE) message("Calculating disparity...", appendLF = FALSE)
    #Calculate disparity in all the series
    if(do_parallel != TRUE) {
        results <- lapply(data_handle$BSresult, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun, ...)
        #results <- lapply(data_handle$BSresult, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun); warning("DEBUG")
    } else {
        results <- parLapply(cluster, data_handle$BSresult, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun, ...)
        #results <- parLapply(cluster, data_handle$BSresult, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun); warning("DEBUG")
        stopCluster(cluster)
    }
    
    #if data is bootstrapped, also calculate the observed disparity
    if(data_handle$is.bootstrapped != FALSE) {
        OBSresults <- lapply(data$data$observed, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun, ...)
        #OBSresults <- lapply(data$data$observed, disparity.calc, level3.fun = metric_handle$level3.fun, level2.fun = metric_handle$level2.fun, level1.fun = metric_handle$level1.fun); warning("DEBUG")
    }
    #verbose
    if(verbose != FALSE) message("Done.", appendLF = FALSE)

    #----------------------
    #OUTPUT
    #----------------------
    #call details
    dispRity.call <- paste("Disparity calculated as: ", as.expression(match_call$metric), " for ", ncol(data_handle$BSresult[[1]][[1]][[1]]) ," dimensions.", sep = "")
    #Add BS (and series) details
    if(data_handle$is.bootstrapped != FALSE) {
        dispRity.call <- paste(dispRity.call, data_handle$boot.call, sep = "\n")
    } else {
        if(data_handle$prev_info != FALSE) {
            dispRity.call <- paste(dispRity.call, "\ndata was split using ", data_handle$series_type, " method.", sep = "")
        }
    }


    #Creating the output object
    if(data_handle$is.bootstrapped != FALSE) {
        output <- list("data" = list("bootstraps" = data_handle$BSresult, "observed" = data_handle$data) , "disparity" = list("bootstrapped" = results, "observed" = OBSresults), "elements" = data_handle$taxa_list, "series" = data_handle$series_list, "call" = dispRity.call)
    } else {
        output <- list("data" = list("observed" = data_handle$data), "disparity" = list("observed" = results), "elements" = data_handle$taxa_list, "series" = data_handle$series_list, "call" = dispRity.call)
    }
    #Output object is a dispRity object
    class(output) <- "dispRity"

    return(output)
}

