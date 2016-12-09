#' @title Prints a \code{dispRity} object.
#'
#' @description Summarises the content of a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise it's content (\code{FALSE} - default).
#'
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Displaying the summary of the object content
#' disparity
#' print(disparity) # the same
#' print.dispRity(disparity) # the same
#'
#' ## Displaying the full object
#' print.dispRity(disparity, all = TRUE)
#'
#' @seealso \code{\link{cust.series}}, \code{\link{time.series}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG dispRity.R")
# library(dispRity)
# source("sanitizing.R")
# source("dispRity.R")
# source("dispRity_fun.R")
# source("dispRity.metric.R")
# source("make.dispRity.R")
# source("fetch.dispRity.R")
# source("boot.matrix.R") ; source("boot.matrix_fun.R")
# source("time.series.R") ; source("time.series_fun.R")
# source("cust.series.R") ; source("cust.series_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data_simple <- BeckLee_mat50
# data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
# data_series_simple <- time.series(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
# data_series_boot <- boot.matrix(data_series_simple, bootstraps = 11, rarefaction = c(5,6))
# data <- dispRity(data_series_boot, metric = c(variances))

print.dispRity <- function(x, all = FALSE) {

    if(all == TRUE) {
        ## Print everything
        tmp <- x
        class(tmp) <- "list"
        print(tmp)

    } else {

        ## Head
        #cat("Disparity object with:")

        if(length(x$call) == 0) {
            cat("Empty dispRity object.\n")
            return()
        }

        cat(" ---- dispRity object ---- \n")

        ## Print the matrix informations
        if(any(names(x$call) == "series") && length(x$series) != 1) {
            ## Get the number of series (minus origin)
            series <- names(x$series)

            ## Check if there are more than one series
            if(length(series) != 1) {

                ## Get the method
                method <- x$call$series
                if(length(method) != 1) {
                    method <- paste(method[1], " (", method[2],")", sep = "")
                }
                cat(paste(length(series), method, "series for", nrow(x$matrix), "elements"))
                if(length(x$call$dimensions) != 0) cat(paste(" with", x$call$dimensions, "dimensions"), sep = "")
                cat(":\n")
                if(length(series) > 5) {
                    cat("    ",paste(series[1:5], collapse=", "),"...\n")
                } else {
                    cat("    ",paste(series, collapse=", "), ".\n", sep="")
                }
            }
        } else {
            cat(paste(nrow(x$matrix), "elements"))
            if(length(x$call$dimensions) != 0) cat(paste(" with", x$call$dimensions, "dimensions"), sep = "")
            cat(".\n")
        }
        
        ## Print the bootstrap information
        if(any(names(x$call) == "bootstrap")) {
            if(x$call$bootstrap[[1]] != 0) {
                cat(paste("Data was bootstrapped ", x$call$bootstrap[[1]], " times (method:\"", x$call$bootstrap[[2]], "\")", sep = ""))
            }
            if(!is.null(x$call$bootstrap[[3]])) {
                if(x$call$bootstrap[[3]][[1]] == "full") {
                    cat(" and fully rarefied")
                } else {
                    cat(paste(" and rarefied to ", paste(x$call$bootstrap[[3]], collapse = ", "), " elements", sep = ""))
                }
            }
            cat(".\n")
        }

        ## Print the disparity information
        if(any(names(x$call) == "disparity")) {
            
            #metrics <- as.character(x$call$disparity$metrics)
            #strsplit(strsplit(metrics, split = "c(", fixed = TRUE)[[1]], split = ")", fixed = TRUE)[[2]][1]

            cat(paste("Disparity was calculated as:", as.character(x$call$disparity$metrics)))
            cat(".\n")
        }
    }
}