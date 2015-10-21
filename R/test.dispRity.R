#' @title Disparity statistics 
#'
#' @description Applying statistical tests to dispRity objects
#'
#' @param data A \code{dispRity} object.
#' @param test A statistical \code{function} to apply to the data.
##' @param format The expected input format to the function (either \code{"matrix"} or \code{"vector"} - default).
#' @param comparisons If data contains more than two series, the type of comparisons to apply: either \code{"referential"}, \code{"sequential"}, \code{"pairwise"} or an even \code{vector} of series names/number to compare (see details).
#' @param ... Additional options to pass to the test \code{function}.
#'
#' @details  
#' The \code{comparison} argument can be:
#' \itemize{
#'   \item \code{"referential"}: compares the first series to all the others.
#'   \item \code{"sequential"}: compares each series sequentially (e.g. first against second, second against third, etc.).
#'   \item \code{"pairwise"}: pairwise comparisons of all the series.
#'   \item An even \code{vector} of names or number of series to compare.
#' }
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
#' ## Caculating the sum of ranges
#' sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))
#'
#' ## Summarising the results
#' summary(sum_of_ranges) # default
#' ## Using different options
#' summary(sum_of_ranges, quantile = 75, cent.tend = median,
#'      rounding=  0, recall = TRUE)
#' 
#' @seealso \code{\link{dispRity}}
#'
#' @author Thomas Guillerme


test.dispRity<-function(data, test, comparisons, ...) { #format

    #get call
    match_call<-match.call()

    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #Check if it's a bootstrapped dispRity object
    if(class(data) == "dispRity" & length(data) == 4) stop(paste(data$call), "\nUse the dispRity function to calculate disparity.", sep="")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a dispRity object.")
    OBSresults<-data$disparity$observed
    #is the data bootstrapped? 
    if(!is.na(match("bootstraps", names(data$data)))) {
        #must have more than one bootstrap!
        if(length(data$data$bootstraps[[1]][[1]]) > 1) {
            is.bootstrapped<-TRUE
            BSresults<-data$disparity$bootstrapped
        } else {
            is.bootstrapped<-FALSE
        }
    } else {
        is.bootstrapped<-FALSE
    }
    
    #Test
    #must be a single function
    check.class(test, "function", " must be a single function.")
    check.length(test, 1, " must be a single function.")

    # #Format
    # #must be a single character string
    # check.class(format, "character", " must be either 'matrix' or 'vector'.")
    # check.length(format, 1, " must be either 'matrix' or 'vector'.")
    # #must be either matrix or vector
    # if(format != "matrix") {
    #     if(format != "vector") {
    #         stop(paste(as.expression(match_call$format), " must be either 'matrix' or 'vector'.", sep=""))
    #     }
    # }

    #Comparisons
    test_data_length <- extract.dispRity(data)
    #Stop if only one series
    if(length(test_data_length) == 1) stop(paste(as.expression(match_call$data), " must have at least two series.", sep=""))
    #Ignore if length data = 2
    if(length(test_data_length) == 2) comparisons <- NULL
    #Else, check comparisons
    if(length(test_data_length) > 2) {
        is.pair <- FALSE
        all_comparisons <- c("referential", "sequential", "pairwise")
        if(all(is.na(match(comparisons, all_comparisons)))) {
            #Check if the length of comparisons is even
            if(comparisons%%2 != 0) stop(paste(as.expression(match_call$comparisons), " is not an even vector!", sep=""))
            #Check if the given vector corresponds to the series names
            if(class(comparisons) == "character") {
                if(any(is.na(match(comparisons, data$series)))) stop(paste(as.expression(match_call$comparisons), ": at least one series was not found.", sep=""))
            }
            #Check if the given vector corresponds to the series numbers
            if(class(comparisons) == "numeric") {
                if(any(is.na(match(comparisons, seq(1:length(data$series)))))) stop(paste(as.expression(match_call$comparisons), ": at least one series was not found.", sep=""))
            }
        } else {
            #Make sure only one inbuilt comparison is given
            check.length(comparisons, 1, " must be either 'referential', 'sequential', 'pairwise' or a vector of series names/numbers.")
        }
    } else {
        is.pair <- TRUE
    }

    #----------------------
    # APPLYING THE TEST
    #----------------------
    #~~~~~~~~~~
    # Add a controller for observed data
    #~~~~~~~~~~

    #Extracting the data
    extracted_data <- extract.dispRity(data, observed=FALSE)

    #Apply the simple test if data is a pair
    if(is.pair == TRUE) {
        #running the test
        output <- test(extracted_data[[1]], extracted_data[[2]])
        #fixing the data name (if hclass)
        if(class(output) == "hclass") output$data.name <- paste(data$series, collapse=" and ")
    }

    #output
    return(output)
}
