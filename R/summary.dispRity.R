#' @title dispRity object summary
#'
#' @description Creates a summary of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param quantiles The quantiless to display (default is \code{quantiles = c(50,95)}; is ignored if the \code{dispRity} object is not bootstrapped).
#' @param cent.tend A function for summarising the bootstrapped disparity values (default is \code{\link[base]{median}}).
#' @param recall \code{logical}, whether to recall the \code{dispRity} parameters input (default = \code{FALSE}).
#' @param rounding Optional, a value for rounding the values in the output table (default = 2).
# ' @param results Optional, in the case of summarising a \code{\link{sequential.test}} which results to display (default = "coefficients")
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
#' sum_of_variances <- dispRity(bootstrapped_data,
#'      metric = c(sum, variances))
#'
#' ## Summarising the results
#' summary(sum_of_variances) # default
#' ## Using different options
#' summary(sum_of_variances, quantiles = 75, cent.tend = median,
#'      rounding = 0, recall = TRUE)
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{plot.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# source("make.metric.R")
# source("summary.dispRity_fun.R")
# data(BeckLee_mat50)
# factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12), rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_series <- cust.series(BeckLee_mat50, factors)
# bootstrapped_data <- boot.matrix(customised_series, bootstraps = 3)
# sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
# series <- extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = TRUE)
# data <- sequential.test(series, family = gaussian, correction = "hommel")
# data <- sum_of_variances

summary.dispRity <- function(data, quantiles = c(50,95), cent.tend = median, recall = FALSE, rounding){#, results = "coefficients") {

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
    if(make.metric(cent.tend, silent = TRUE) != "level1") {
        stop(paste(match_call$cent.tend), "can not be used for measuring the central tendency.")
    }
    ## Update match_call if argument is empty
    if(is.null(match_call$cent.tend)) match_call$cent.tend <- "median"

    #recall
    check.class(recall, "logical")

    #rounding
    if(missing(rounding)) {
        #Set to default (see below)
        rounding <- "default"
    } else {
        check.class(rounding, "numeric")
    }

    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #Check if it's a bootstrapped dispRity object
    if(is.null(data$disparity)) {
        stop("Disparity has not been calculated yet.\nUse the dispRity() function to do so.\n", sep = "")
    }
    
    #Check quantiles
    check.class(quantiles, "numeric", " must be any value between 1 and 100.")
    if(any(quantiles < 1) | any(quantiles > 100)) {
        stop("quantiles(s) must be any value between 1 and 100.")
    }

    # #check if is.distribution
    # is.distribution <- ifelse(length(data$disparity$observed[[1]][[1]][[1]]) == 1, FALSE, TRUE)

    #----------------------
    # TRANSFORMING THE DATA INTO A TABLE
    #----------------------

    ## Initialise the results
    summary_results <- data.frame(row.names = NULL,
            ## Get the series names
            "series" = names(data$series),
            ## Get the number of elements per series
            "n" = unlist(lapply(as.list(1:length(data$series)), lapply.fetch.elements, data)),
            ## Get the observed values
            "obs" = unlist(lapply(data$disparity, lapply.observed))
        )

    if(!is.null(data$call$bootstrap)) {
        ## Calculate the central tendencies and the quantiles
        summary_results <- cbind(summary_results, matrix(unlist(lapply(data$disparity, lapply.summary, cent.tend, quantiles)), byrow = TRUE, ncol = (1+length(quantiles)*2)))
        ## Adding the labels
        names(summary_results)[(length(summary_results)-(length(quantiles)*2)):length(summary_results)] <- c(paste("bs", match_call$cent.tend, sep="."), names(quantile(rnorm(5), probs = CI.converter(quantiles))))
    }

    #Round the results (number of decimals = maximum number of digits in the entire)
    summary_results <- rounding.fun(summary_results, rounding)

    #----------------------
    # OUTPUT
    #----------------------
    if(recall) print.dispRity(data)

    return(summary_results)

    #Summary sequential.test shortcut
    # if(length(class(data)) == 2) {
    #     if(class(data)[[1]] == "dispRity" && class(data)[[2]] == "seq.test") {
    #         #Results sanitizing
    #         check.class(results, "character") 
    #         #At least one must be "coefficients"
    #         if(is.na(match("coefficients", results))) {
    #             stop("At least one of the returned results must be 'coefficients'.")
    #         }
    #         #Results must be at least coefficients
    #         if(is.na(match("coefficients", results))) {
    #             results <- c(results, "coefficients")
    #         }

    #         #Creating the table results
    #         results_out <- summary.seq.test(data, quantiles, cent.tend, recall, rounding, results, match_call)

    #         #Checking if distribution
    #         is.distribution <- ifelse(length(data$models[[1]]) == 1, FALSE, TRUE)

    #         #Rounding the results
    #         if(is.distribution == FALSE) {
    #             results_out <- lapply(results_out, rounding.fun, rounding, seq.test = TRUE)
    #         } else {
    #             results_out <- lapply(results_out, lapply, rounding.fun, rounding, seq.test = TRUE)
    #         }

    #         return(results_out)
    #     }
    #     if(class(data)[1] == "dispRity" & class(data)[2] == "randtest") {
    #         #No summary
    #         return(data)
    #     }
    # }


}
