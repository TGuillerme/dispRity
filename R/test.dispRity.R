#' @title Disparity statistics 
#'
#' @description Applying statistical tests to dispRity objects
#'
#' @param data A \code{dispRity} object.
#' @param test A statistical \code{function} to apply to the data.
##' @param format The expected input format to the function (either \code{"matrix"} or \code{"vector"} - default).
#' @param comparisons If data contains more than two series, the type of comparisons to apply: either \code{"referential"}, \code{"sequential"}, \code{"pairwise"}, \code{"all"} or a list of pairs of series names/number to compare (see details).
#' @param ... Additional options to pass to the test \code{function}.
#'
#' @details  
#' The \code{comparison} argument can be:
#' \itemize{
#'   \item \code{"referential"}: compares the first series to all the others.
#'   \item \code{"sequential"}: compares each series sequentially (e.g. first against second, second against third, etc.).
#'   \item \code{"pairwise"}: pairwise comparisons of all the series.
#'   \item \code{"all"}: compares all the series simultaneously (ANOVA type). The applyied formula will be \code{bootstrapped disparity ~ series names}.
#'   \item A list of pairs of number of series to compare. Each element of the the list must contain two elements
#'      (e.g. \code{list(c("a","b"), ("b", "a"))} to compare "a" to "b" and then "b" to "a").
# Change that to allow any comparisons pattern?
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


test.dispRity<-function(data, test, comparisons, ...) { #format: get additional option for input format?

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

    # #Ignore if length data = 2
    # if(length(test_data_length) == 2) comparisons <- NULL

    #Else, check comparisons
    if(length(test_data_length) >= 2) {
        all_comparisons <- c("referential", "sequential", "pairwise", "all")

        #Check if the comparisons is not one of the inbuilt comparisons
        if(all(is.na(match(comparisons, all_comparisons)))) {
            #must be a list
            check.class(comparisons, "list", " must be a list of one or more pairs of series.")
            #must be pairs
            if(length(unlist(comparisons))%%2 != 0) stop(paste(as.expression(match_call$comparisons), " must be a list of one or more pairs of series.", sep=""))    
            #If character, input must match the series
            if(class(unlist(comparisons)) == "character") {
                if(any(is.na(match(unlist(comparisons), data$series)))) stop(paste(as.expression(match_call$comparisons), ": at least one series was not found.", sep=""))
            }
            #If numeric, input must match de series numbers
            if(class(unlist(comparisons)) == "numeric") {
                if(any(is.na(match(unlist(comparisons), seq(1:length(data$series)))))) stop(paste(as.expression(match_call$comparisons), ": at least one series was not found.", sep=""))
            }
            #Comparison is "custom"
            comp <- "custom"
        } else {
            #Make sure only one inbuilt comparison is given
            check.length(comparisons, 1, " must be either 'referential', 'sequential', 'pairwise', 'all' or a vector of series names/numbers.")
            comp <- comparisons
        }
    }

    #----------------------
    # APPLYING THE TEST
    #----------------------

    #Extracting the data (sends error if data is not bootstrapped)
    extracted_data <- extract.dispRity(data, observed=FALSE)

    # #Apply the test to the two distributions only
    # if(is.pair == TRUE) {
    #     #running the test
    #     output <- test(extracted_data[[1]], extracted_data[[2]])
    #     #fixing the data name (if hclass)
    #     if(class(output) == "hclass") output$data.name <- paste(data$series, collapse=" and ")
    # } 


    #Referential comparisons (first distribution to all the others)
    if(comp == "referential") {
        #Select the reference series
        reference_series <- extracted_data[[1]]
        other_series <- extracted_data[-1]

        #Applying the test to the list of other series
        details_out <- lapply(other_series, flip.ref.lapply, referential=reference_series, test=test, ...)
        #details_out <- lapply(other_series, flip.ref.lapply, referential=reference_series, test=test) ; warning("DEBUG")
    }

    #Pairwise comparisons (all to all)
    if(comp == "pairwise") {
        #Create the list of pairs
        pair_series <- combn(1:length(extracted_data), 2)

        #convert pair series table in a list of pairs
        pair_series <- unlist(apply(pair_series, 2, list), recursive=FALSE)

        #Applying the test to the list of pairwise comparisons
        details_out <- lapply(pair_series, test.list.lapply, extracted_data, test, ...)
        #details_out <- lapply(pair_series, test.list.lapply, extracted_data, test) ; warning("DEBUG")
    }

    #Sequential comparisons (one to each other)
    if(comp == "sequential") {
        #Set the list of sequences
        seq_series <- set.sequence(length(extracted_data))

        #convert seq series in a list of sequences
        seq_series <- unlist(apply(seq_series, 2, list), recursive=FALSE)

        #Applying the test to the list of pairwise comparisons
        details_out <- lapply(seq_series, test.list.lapply, extracted_data, test, ...)
        #details_out <- lapply(seq_series, test.list.lapply, extracted_data, test) ; warning("DEBUG")

    }

    #User defined
    if(comp == "custom") {
        #Set the list of comparisons
        cust_series <- comparisons

        #If the series are characters, convert them into numeric
        if(class(unlist(cust_series)) == "character") {
            cust_series <- convert.to.numeric(cust_series, extracted_data)
        }

        #Applying the test to the custom list
        details_out <- lapply(cust_series, test.list.lapply, extracted_data, test, ...)
        #details_out <- lapply(cust_series, test.list.lapply, extracted_data, test) ; warning("DEBUG")        
    }
    
    #ANOVA type
    if(comp == "all") {
        #Transform the extracted data into a table
        series_table <- list.to.table(extracted_data)
        
        #running the test
        details_out <- test(series_table$data~series_table$factor, ...)
        #details_out <- test(series_table$data~series_table$factor) ; warning("DEBUG")
    }

    #Dealing with the output!




    #output
    return(details_out)
}
