#' @title Disparity time series changes
#'
#' @description Check for changes in disparity time series
#'
#' @param data a \code{"dispRity"} object with time series (e.g. from \code{chrono.subsets}) and disparity values
#' @param method the method for measuring change (either \code{"itsa"}, \code{"citsa"}, \code{"area"}, \code{"average"} or any generic \code{h.test} function). See details.
#' @param changepoint one or more \code{"numeric"} or \code{"integer"} values of where to measure the changes in disparity time series. This can be set to \code{"detect"} to test all the time subsets available.
#' @param time.window optional, either two \code{"numeric"} or \code{"integer"} values to narrow than the time window to perform the test on or a single value that can be either a proportion of subsets to use before and after the \code{changepoint} (up to \code{0.5}) or a number of subsets to use before and after the \code{changepoint}.
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

    # user_args <- list(...)
    
    ## Check the data
    error_disparity <- " must be a dispRity object with a tree, time series and disparity data."
    if(!is(data, "dispRity")) {
       stop.call(call = match_call$data, msg = error_disparity)
    }
    if(!is.null(data$subsets) && !is.null(data$call)) {
        if(data$call$subsets[[1]] != "continuous") {
            stop.call(msg = "chrono.test is not implemented yes for customised subsets")
        }
    }
    if(!is(data$tree[[1]], "phylo")) {
        stop.call(call = match_call$data, msg = error_disparity)
    }
    if(is.null(data$disparity)) {
        stop.call(call = match_call$data, msg = error_disparity)
    }

    ## Check and set up the changepoint and time.window
    all_time_range <- range(unlist(lapply(lapply(data$tree, tree.age), `[[`, "ages")))
    changepoint_class <- check.class(changepoint, c("numeric", "integer", "character"))
    if(changepoint_class == "character") {
        if(changepoint_class != "detect") {
            stop.call(msg = "changepoint argument must be a numeric or integer vector or \"detect\" to automatically detect the changepoint.")
        }
    } else {
        check.time.range <- function(x, all_time_range, name) {
            if(any(x < min(all_time_range)) || any(x > max(all_time_range))) {
                stop.call(msg.pre = paste0(name, add.s(x)), msg = paste0(" falls out of the time range of the data tree", add.s(data$tree)," (", min(all_time_range), " - ", max(all_time_range), ")."))
            }
        }
        check.time.range(changepoint, all_time_range, name = "changepoint")
    }

    if(missing(time.window)) {
        time.window <- all_time_range
    } else {
        check.class(time.window, c("numeric", "integer"))
        if(length(time.window) < 3) {
            if(length(time.window) == 2) {
                check.time.range(time.window, all_time_range, name = "time.window")
            }
        } else {
            stop.call(msg = "time.window can only contain 2 values (the time ranges) or 1 value (the proportion or number of subsets before and after the time).")
        }
    }

    ## Check the method
    method_type <- check.class(method, c("character", "function"))

    if(method_type == "character") {
        ## Standard methods
        chrono_test_methods <- c("itsa", "citsa", "area", "average")
        method_out <- check.method(method, chrono_test_methods, "method argument")
    } else {
        ## Check if method is a h.test function
        method <- "average"
        stop.call(msg = "user function for method not implemented yet.")
    }

    ## Toggling parameters
    is.multi.matrix <- length(data$matrix)

    dimension.level <- 1 #TG: is this for getting the number of dimensions in the data (then you can do:
    # dimension.level <- length(data$call$dimensions)
    #    or the dimension of the metric? then you can do something like
    # if(length(data$call$disparity$metrics$fun) == 1) {
    #     levels <- make.metric(data$call$disparity$metrics$fun, silent = TRUE)$type
    # } else {
    #     levels <- unlist(lapply(lapply(data$call$disparity$metrics$fun, make.metric, silent = TRUE), `[[`, "type"))
    # }
    # dimension.level <- as.integer(gsub("level", "", levels))

    # levels <- lapply(data$call$disparity$metrics, make.metric, silent = TRUE)
    # 
    if (any(unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x) >1)))) {
        dimension.level <- unlist(lapply(get.disparity(data, concatenate = FALSE), function(x) nrow(x)), use.names = FALSE)[1]
    }



    #######################################################################################################



    delta_df <- make.deltatronic(data, changepoint, time.window, dimension.level, is.multi.matrix)

    # if (is_multi){
        
    # }
    # dimension.level <- 1
    # if (all(unlist(lapply(delta_df, function(x) ncol(x$disparity) >1)))) { # if disparity is > 1-dim
    #     dimension.level <- ncol(x$disparity)
    # }

    # is_multi <- FALSE



    chrono_test_output <- switch(method,
        itsa={

            #TG: for here and for delta_df in general, is it not easier to just make a list of lists? So that it never has to toggle between either options? I.e. if it's a multi.matrix or not it always go double lapply?
            if(is.multi.matrix > 1){
                itsa <- lapply(delta_df, lapply, itsa.method, dimension.level,...)
            } else {
                itsa <- lapply(delta_df, itsa.method, dimension.level, ...)
            }        
        },
        citsa={

            changepoint <- set.changepoint(changepoint)
                    
            control <- lapply(changepoint, make.control, data = data)
            
            control_deltatronic <- make.deltatronic(control, changepoint, time.window, dimension.level, is.multi.matrix)
            # control_deltatronic <- lapply(control, make.deltatronic, changepoint, time.window)
            control_delta_df <- lapply(control_deltatronic, function(x) {
                x$emp_vs_null <- matrix(0, nrow = nrow(x$time))
                return(x)
            })


            if (is.multi.matrix > 1) {
                delta_df <- lapply(delta_df, lapply, function(x) {
                x$emp_vs_null <- matrix(1, nrow = nrow(x$time))
                return(x)
            })
            } else {
                delta_df <- lapply(delta_df, function(x) {
                    x$emp_vs_null <- matrix(1, nrow = nrow(x$time))
                    return(x)
                })
            }
            citsa <- Map(citsa.method, delta_df, control_delta_df)
            ## here will go `citsa.method`
        },
        area={
            if (is.multi.matrix > 1) {
                itsa <- lapply(delta_df, lapply, itsa.method, dimension.level, ...)
                area <- lapply(itsa, lapply, area.method, dimension.level) ## check on this 
            }
            itsa <- lapply(delta_df, itsa.method, dimension.level, ...)
            area <- lapply(itsa,  area.method, ...)
        },
        average={
            average <- lapply(delta_df, average.method, dimension.level, is.multi.matrix, ...)
        }
    )



    ## 


    

    return(list(
        test.output = chrono_test_output,
        disparity = data,
        call = list(method, changepoint, time.window)
    ))

}

