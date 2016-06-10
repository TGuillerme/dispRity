#' @title dispRity object summary
#'
#' @description Creates a summary of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param quantiles The quantiless to display (default is \code{quantiles = c(50,95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[base]{mean}}).
#' @param recall \code{logical}, whether to recall the \code{dispRity} parameters input (default = \code{FALSE}).
#' @param rounding Optional, a value for rounding the values in the output table (default = 2).
#' @param results Optional, in the case of summarising a \code{\link{sequential.test}} which results to display (default = "coefficients")
#'
#' @return
#' A \code{data.frame} with:
#' \item{series}{the series names.}
#' \item{n}{the number of elements per series.}
#' \item{observed}{the observed disparity or the the observed central tendency (<cent_tend>) of disparity (\code{obs.<cent_tend>}).}
#' \item{bootstraps...}{if \code{data} is bootstrapped, the bootstrapped disparity's central tendency (\code{bs.<cent_tend>}) and the quantiless of the bootstrapped disparity's (or, if \code{data} is not bootstrapped but disparity is calculated as a distribution - see \code{\link[dispRity]{dispRity}}) - the quantiless of the observed disparity is displayed).}
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
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_sum_of_variances <- dispRity(bootstrapped_data,
#'      metric = c(sum, sum_of_variances))
#'
#' ## Summarising the results
#' summary(sum_of_sum_of_variances) # default
#' ## Using different options
#' summary(sum_of_sum_of_variances, quantiles = 75, cent.tend = median,
#'      rounding = 0, recall = TRUE)
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme

#testing
# source("sanitizing.R")
# source("summary.dispRity_fun.R")
# data(BeckLee_mat50)
# factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12), rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_series <- cust.series(BeckLee_mat50, factors)
# bootstrapped_data <- boot.matrix(customised_series, bootstraps = 3)
# sum_of_variances <- dispRity(bootstrapped_data, metric =  variances)
# series <- extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = TRUE)
# data <- sequential.test(series, family = gaussian, correction = "hommel")



summary.dispRity<-function(data, quantiles=c(50,95), cent.tend=mean, recall=FALSE, rounding, results="coefficients") {

    #----------------------
    # SANITIZING
    #----------------------

    #Get call
    match_call <- match.call()
    #return(match_call)

    #cent.tend
    #Must be a function
    check.class(cent.tend, "function")
    #The function must work
    silent <- check.metric(cent.tend)

    #recall
    check.class(recall, "logical")

    #rounding
    if(missing(rounding)) {
        #Set to default (see below)
        rounding <- "default"
    } else {
        check.class(rounding, "numeric")
    }

    #Summary sequential.test shortcut
    if(length(class(data)) == 2) {
        if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "seq.test") {
            #Results sanitizing
            check.class(results, "character") 
            #At least one must be "coefficients"
            if(is.na(match("coefficients", results))) {
                stop("At least one of the returned results must be 'coefficients'.")
            }
            #Results must be at least coefficients
            if(is.na(match("coefficients", results))) {
                results <- c(results, "coefficients")
            }

            #Creating the table results
            results_out <- summary.seq.test(data, quantiles, cent.tend, recall, rounding, results, match_call)

            #Checking if distribution
            is.distribution <- ifelse(length(data$models[[1]]) == 1, FALSE, TRUE)

            #Rounding the results
            if(is.distribution == FALSE) {
                results_out <- lapply(results_out, rounding.fun, rounding, seq.test = TRUE)
            } else {
                results_out <- lapply(results_out, lapply, rounding.fun, rounding, seq.test = TRUE)
            }

            return(results_out)
        }
        if(class(data)[1] == "dispRity" & class(data)[2] == "randtest") {
            #No summary
            return(data)
        }
    }

    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #Check if it's a bootstrapped dispRity object
    if(class(data) == "dispRity" & length(data) == 4) stop("No disparity calculated yet.\nUse the dispRity() function to calculate disparity.\n", "So far:\n", paste(data$call), sep="")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a dispRity object.")
    OBSresults <- data$disparity$observed
    #is the data bootstrapped? 
    if(!is.na(match("bootstraps", names(data$data)))) {
        #must have more than one bootstrap!
        if(length(data$data$bootstraps[[1]][[1]]) > 1) {
            is.bootstrapped <- TRUE
            BSresults <- data$disparity$bootstrapped
        } else {
            is.bootstrapped<-FALSE
        }
    } else {
        is.bootstrapped<-FALSE
    }
    
    #quantiles
    #Only check if the data is bootstrapped
    if(is.bootstrapped == TRUE) {
        check.class(quantiles, "numeric", " must be any value between 1 and 100.")
        #remove warnings
        if(any(quantiles < 1)) {
            stop("quantiles(s) must be any value between 1 and 100.")
        }
        if(any(quantiles > 100)) {
            stop("quantiles(s) must be any value between 1 and 100.")
        }
    }

    #check if is.distribution
    is.distribution <- ifelse(length(data$disparity$observed[[1]][[1]][[1]]) == 1, FALSE, TRUE)

    #----------------------
    # TRANSFORMING THE DATA INTO A TABLE
    #----------------------

    if(is.bootstrapped == FALSE) {
        
        if(is.distribution == FALSE) {
            #Extracting the observed disparity
            OBSresults_unl <- unlist(unlist(recursive.unlist(OBSresults), recursive = FALSE))

            #Create the result table
            results_table <- data.frame(cbind(rep(data$series, unlist(lapply(OBSresults_unl, length))), diversity.count(data$data$observed), as.numeric(OBSresults_unl)), stringsAsFactors=FALSE)

            #Add columns names
            colnames(results_table)<-c("series", "n", "observed")
        } else {
            #Extracting the observed disparity
            OBSresults_unl <- unlist(recursive.unlist(OBSresults, is.distribution = TRUE), recursive = FALSE)
            #Calculate their central tendencies
            results_cent <- unlist(lapply(unlist(OBSresults_unl, recursive = FALSE), cent.tend))
            #Calculate their quantiless
            results_quantiles <- lapply(unlist(OBSresults_unl, recursive = FALSE), quantile, probs = CI.converter(quantiles))

            #Create the result table
            results_table <- data.frame(cbind(rep(data$series, unlist(lapply(OBSresults_unl, length))), diversity.count(data$data$observed), results_cent), stringsAsFactors = FALSE)
            #Add to the quantiless table
            results_table <- cbind(results_table, matrix(data = unlist(results_quantiles), ncol = length(quantiles)*2, byrow = TRUE))

            #Add columns names
            if(is.null(match_call$cent.tend)) {
                colnames(results_table)<-c("series", "n", "obs.mean")
            } else {
                colnames(results_table)<-c("series", "n", paste("obs", match_call$cent.tend, sep = "."))
            }
            #Add the quantiles names
            colnames(results_table)[c(4:(length(quantiles)*2+3))] <- names(results_quantiles[[1]])
        }
    
    } else {
        if(is.distribution == FALSE) {
            #Extracting the observed disparity
            OBSresults_unl <- unlist(unlist(recursive.unlist(OBSresults), recursive = FALSE))
        } else {
            #Extracting the observed disparity
            OBSresults_unl <- unlist(recursive.unlist(OBSresults, is.distribution = TRUE), recursive = FALSE)
            #Calculate their central tendencies
            OBSresults_cent <- unlist(lapply(unlist(OBSresults_unl, recursive = FALSE), cent.tend))
        } 

        #Extrating the BS results 
        BSresults_unl <- unlist(recursive.unlist(BSresults), recursive = FALSE)
    
        #Calculating the central tendency
        results_cent <- unlist(lapply(BSresults_unl, cent.tend))

        #Multiplier (for rep)
        multiplier <- unlist(lapply(BSresults, length))

        if(is.distribution == FALSE) {
            #Create the result table
            results_table <- data.frame(cbind(rep(data$series, multiplier), diversity.count(data$data$bootstraps), rep(OBSresults_unl, multiplier), results_cent), stringsAsFactors = FALSE)

            #Add columns names
            if(is.null(match_call$cent.tend)) {
                colnames(results_table)<-c("series", "n", "obs.mean", "bs.mean")
            } else {
                colnames(results_table)<-c("series", "n", paste("obs", match_call$cent.tend, sep = "."), paste("bs", match_call$cent.tend, sep = "."))
            }
        } else {
            #Create the result table
            results_table <- data.frame(cbind(rep(data$series, multiplier), diversity.count(data$data$bootstraps), rep(OBSresults_cent, multiplier), results_cent), stringsAsFactors = FALSE)

            #Add columns names
            if(is.null(match_call$cent.tend)) {
                colnames(results_table)<-c("series", "n", "obs.mean", "bs.mean")
            } else {
                colnames(results_table)<-c("series", "n", paste("obs", match_call$cent.tend, sep = "."), paste("bs", match_call$cent.tend, sep = "."))
            }
        }

        #Checking if the observed values match the n_obs (otherwise replace by NA)
        n_obs <- diversity.count(data$data$observed)
        for(ser in 1:length(data$series)) {
            to_remove<-which(as.numeric(results_table[which(results_table$series == data$series[ser]), 2]) != n_obs[ser])
            results_table[which(results_table$series == data$series[ser]), 3][to_remove] <- NA          
        }

        #Calculate the quantiless
        results_quantiles <- lapply(BSresults_unl, quantile, probs = CI.converter(quantiles))

        #Add to the result table
        results_table <- cbind(results_table, matrix(data = unlist(results_quantiles), ncol = length(quantiles)*2, byrow = TRUE))

        #Add the quantiles names
        colnames(results_table)[c(5:(length(quantiles)*2+4))] <- names(results_quantiles[[1]])
    }

    #Round the results (number of decimals = maximum number of digits in the entire)
    results_table <- rounding.fun(results_table, rounding)

    #Make the rarefaction column numeric
    results_table[,2] <- as.numeric(results_table[,2])

    #----------------------
    # OUTPUT
    #----------------------
    if(recall == TRUE) {
        cat(data$call, sep = "\n")
    }

    return(results_table)

}
