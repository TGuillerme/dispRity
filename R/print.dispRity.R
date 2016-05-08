#' @title Prints a \code{dispRity} object.
#'
#' @description Summarises the content of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise it's content (\code{FALSE} - default).
#'
#' @examples
#' ## Load the Beck & Lee 2014 matrix
#' data(BeckLee_mat50)
#' 
#' ## Creating a dispRity object
#' disparity_object <- dispRity(BeckLee_mat50, metric = c(sum, variances))
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

print.dispRity<-function(data, all=FALSE, ...) {


    #Sequential tests
    if(class(data)[1] == "dispRity" & class(data)[2] == "seq.test") {
        #Print sequential.tests
    }


    #If all = TRUE, return the whole data (no summary)
    if(all == TRUE) {
        y <- data
        class(y)<-"list"
        print(y)
        
    } else {
        #Series
        if(length(data) == 3) {
            #head
            cat(paste((length(data$series)-1), data$series[1], "series for", length(data$elements), "elements"), "\n")
            
            #series
            #remove the method time
            data$series<-data$series[-1]
            
            if(length(data$series) == 1) {
                cat(paste(data$series))
            } else {
                cat("Series:\n")
                if(length(data$series) > 6) {
                    cat(paste(data$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(data$series, collapse=", "), ".", sep="")
                }
            }
        }

        #Bootstraps
        if(length(data) == 4) {
            #head
            cat(paste("Bootstrapped ordinated matrix with", length(data$elements), "elements"), "\n")

            #series
            if(length(data$series) == 1) {
                cat(paste(data$series))
            } else {
                cat("Series:\n")
                if(length(data$series) > 6) {
                    cat(paste(data$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(data$series, collapse=", "), ".", sep="")
                }
            }

            #call
            cat("\n", data$call, sep="")
        }

        #Disparity
        if(length(data) == 5) {
            #head
            cat(paste("Disparity measurements across ", length(data$series), " series for ", length(data$elements), " elements", sep=""), "\n")

            #series
            if(length(data$series) == 1) {
                cat(paste(data$series))
            } else {
                cat("Series:\n")
                if(length(data$series) > 6) {
                    cat(paste(data$series[1:6], collapse=", "),"...")
                } else {
                    cat(paste(data$series, collapse=", "), ".", sep="")
                }
            }

            #call
            cat("\n", data$call, sep="")
        }
    }
}