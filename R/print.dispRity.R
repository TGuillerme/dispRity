#' @title Prints a \code{dispRity} object.
#'
#' @description Summarises the content of a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise it's content (\code{FALSE} - default).
#'
#' @examples
#' ## Load the Beck & Lee 2014 matrix
#' data(BeckLee_mat50)
#' 
#' ## Creating a dispRity object
#' disparity_object <- dispRity(BeckLee_mat50, metric = c(sum, ranges))
#' 
#' ## Displaying the summary of the object content
#' disparity_object
#' print(disparity_object)
#' print.dispRity(disparity_object)
#'
#' ## Displaying the full object
#' print.dispRity(disparity_object, all = TRUE)
#'
#' @author Thomas Guillerme

print.dispRity<-function(x, all=FALSE, ...) {

    #If all = TRUE, return the whole x (no summary)
    if(all == TRUE) {
        y <- x
        class(y)<-"list"
        print(y)
        
    } else {
        #Series
        if(length(x) == 3) {
            #head
            cat(paste((length(x$series)-1), x$series[1], "series for", length(x$elements), "elements"), "\n")
            
            #series
            #remove the method time
            x$series<-x$series[-1]
            
            if(length(x$series) == 1) {
                cat(paste(length(x$series), "unnamed series."))
            } else {
                cat("Series:\n")
                if(length(x$series) > 6) {
                    cat(paste(x$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(x$series, collapse=", "), ".", sep="")
                }
            }
        }

        #Bootstraps
        if(length(x) == 4) {
            #head
            cat(paste("Bootstrapped ordinated matrix with", length(x$elements), "elements"), "\n")

            #series
            if(length(x$series) == 1) {
                cat(paste(length(x$series), "unnamed series."))
            } else {
                cat("Series:\n")
                if(length(x$series) > 6) {
                    cat(paste(x$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(x$series, collapse=", "), ".", sep="")
                }
            }

            #call
            cat("\n", x$call, sep="")
        }

        #Disparity
        if(length(x) == 5) {
            #head
            cat(paste("Disparity measurements across ", length(x$series), " series for ", length(x$elements), " elements", sep=""), "\n")

            #series
            if(length(x$series) == 1) {
                cat(paste(length(x$series), "unnamed series."))
            } else {
                cat("Series:\n")
                if(length(x$series) > 6) {
                    cat(paste(x$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(x$series, collapse=", "), ".", sep="")
                }
            }

            #call
            cat("\n", x$call, sep="")
        }
    }
}