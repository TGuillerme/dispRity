#' @title dispRity object summary
#'
#' @description Creates a summary of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param CI The confidence intervals values (default is \code{CI = c(50,95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[base]{mean}}).
#' @param recall \code{logical}, whether to recall the \code{dispRity} parameters input.
#' @param rounding Optional, a value for rounding the central tendency and the confidence intervals in the output table.
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps=100)
#' ## Caculating the sum of ranges
#' sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))
#'
#' ## Summarising the results
#' summary(sum_of_ranges) # default
#' ## Using different options
#' summary(sum_of_ranges, CI=75, cent.tend=median, rounding=0, recall=TRUE)
#' 
#' @seealso \code{\link{dispRity}}
#'
#' @author Thomas Guillerme


summary.dispRity<-function(data, CI=c(50,95), cent.tend=mean, recall=FALSE, rounding) {
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
    results<-data$disparity
    #is the data bootstrapped?   
    if(!is.na(match("bootstraps", names(data)))) {
        #must have more than one bootstrap!
        if(length(data$bootstrap[[1]][[1]]) > 1) {
            is.bootstrapped<-TRUE
        } else {
            is.bootstrapped<-FALSE
        }
    } else {
        is.bootstrapped<-FALSE
    }
    
    #CI
    #Only check if the data is bootstrapped
    if(is.bootstrapped == TRUE) {
        check.class(CI, "numeric", " must be any value between 1 and 100.")
        #remove warnings
        options(warn=-1)
        if(any(CI) < 1) {
            stop("CI must be any value between 1 and 100.")
        }
        if(any(CI) > 100) {
            stop("CI must be any value between 1 and 100.")
        }
        options(warn=0)
    }

    #cent.tend
    #Must be a function
    check.class(cent.tend, "function")
    #The function must work
    silent<-check.metric(cent.tend)

    #recall
    check.class(recall, "logical")

    #rounding
    if(missing(rounding)) {
        #Set to default (see below)
        rounding<-"default"
    } else {
        check.class(rounding, "numeric")
    }

    #Get call
    match_call<-match.call()

    #----------------------
    # TRANSFORMING THE DATA INTO A TABLE
    #----------------------

    #Unlisting the bootstrap values
    BSresults<-unlist(recursive.unlist(results), recursive=FALSE)

    #Calculating the central tendency
    results_cent<-lapply(BSresults, cent.tend)

    #Create the results table
    results_table<-as.data.frame(cbind(rep(data$series, unlist(lapply(results, length))), diversity.count(data[[1]]), results_cent))

    #Add columns names
    if(is.null(match_call$cent.tend)) {
        colnames(results_table)<-c("series", "n", "mean")
    } else {
        colnames(results_table)<-c("series", "n", match_call$cent.tend)
    }

    #Calculating CIs (if bootstrapped results)
    if(is.bootstrapped == TRUE) {
        #Calculate the CIs
        results_CI<-lapply(BSresults, quantile, probs=CI.converter(CI))

        #Add to the result table
        results_table<-cbind(results_table, matrix(data=unlist(results_CI), ncol=length(CI)*2, byrow=TRUE))

        #Add the CI names
        colnames(results_table)[c(4:(length(CI)*2+3))]<-names(results_CI[[1]])
    }   

    #Round the results (number of decimals = maximum number of digits in the entire)
    if(rounding == "default") {
        for(column in 3:ncol(results_table)) {
            results_table[,column]<-round(as.numeric(results_table[,column]), digit=get.digit(as.numeric(results_table[,column])))
        }
    } else {
        for(column in 3:ncol(results_table)) {
            results_table[,column]<-round(as.numeric(results_table[,column]), digit=rounding)
        }
    }


    #----------------------
    # OUTPUT
    #----------------------
    if(recall==TRUE) {
        cat(data$call, sep="\n")
    }
    return(results_table)

}
