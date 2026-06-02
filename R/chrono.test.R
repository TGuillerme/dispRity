#' @title Disparity time series changes
#'
#' @description Check for changes in disparity time series
#'
#' @param data a \code{"dispRity"} object with time series (e.g. from \code{chrono.subsets}) and disparity values
#' @param method the method for measuring change (either \code{"itsa"}, \code{"citsa"}, \code{"area"}, \code{"average"} or any generic \code{h.test} function). See details.
#' @param changepoint
#' @param time.window
#' @param ... A named list of specific options for each different method. See details.
#' 
#' @details
#' The implemented disparity time series changes are:
#' \itemize{
#'      \item \code{"itsa"}  @@@...
#'      \item \code{"citsa"} @@@... 
#'      \item \code{"area"}  @@@...
#'      \item \code{"average"}  @@@...
#' }
#' 
#' Each method can take the following specific arguments
#' 
#' \itemize{
#'      \item \code{"itsa"}  @@@...
#'      \item \code{"citsa"} @@@... 
#'      \item \code{"area"}  @@@...
#'      \item \code{"average"}  @@@...
#' }
#' 
#' @returns
#' 
#' @@@
#' 
#' @examples
#' 
#' @@@
#' 
#' @seealso \code{\link{chrono.subsets}} \code{\link{dispRity}}
#' 
#' @author Caleb Scutt, Thomas Guillerme
#' @references
#' @@@
#' 


chrono.test <- function(data, method, changepoint, time.window, ...) {
    
    match_call <- match.call()
    
    ## Check the data
    is_data_error <- FALSE
    if(!is(data, "dispRity")) {
        is_data_error <- TRUE
    }
    if(!is.null(data$subsets) && !is.null(data$call)) {
        if(disparity$call$subsets[[1]] != "continuous") {
            is_data_error <- TRUE
            stop.call(call = NULL, "chrono.test is not implemented yes for customised subsets")
        }
    }
    if(is.null(data$disparity)) {
        is_data_error <- TRUE
    }
    if(is_data_error) {
        stop.call(call = match_call$data, msg = " must be a dispRity object with time series and disparity data.")
    }

    ## Check and set up the changepoint and time.window
    changepoint_class <- check.class(changepoint, c("numeric", "integer", "character"))
    if(changepoint_class == "character") {
        if(changepoint_class != "detect") {
            stop.call(call = NULL, msg = "changepoint argument must be a numeric or integer vector or \"detect\" to automatically detect the changepoint.")
        }
    }
    check.class(time.window, c("numeric", "integer"))

    ## Time checks
    changepoint <- check.time(changepoint, type = "changepoint", data = data)
    time.window <- check.time(time.window, type = "time.window", data = data)

    ## Check the method
    method_type <- check.class(method, c("character", "function"))

    if(method_type == "character") {
        ## Standard methods
        chrono_test_methods <- c("itsa", "citsa", "area", "average")
        method_out <- check.method(method, chrono_test_methods, "method argument")
    } else {
        ## Check if method is a h.test function
        method <- "average"
        stop.call(call = NULL, msg = "user function for method not implemented yet.")
    }




    #######################################################################################################

    

    # delta_df <- make.deltatronic(data, changepoint)

    # if(!is.null(time.window)) {
    #     delta_df <- set.time.window(delta_df, time.window, changepoint)
    # }


    




    return(NULL)

}

