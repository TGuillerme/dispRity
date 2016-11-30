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
#' @seealso \code{\link{cust.series}}, \code{\link{time.series}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

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

        ## Print disparity
        if(any(names(x$call) == "disparity")) {
        }

        ## Print series
        if(any(names(x$call) == "series")) {
            ## Get the number of series (minus origin)
            series <- names(x$series)[-1]

            ## Check if there are more than one series
            if(length(series) != 1) {

                ## Get the method
                method <- x$call$series
                if(length(method) != 1) {
                    method <- paste(method[1], " (", method[2],")", sep = "")
                }
                cat(paste(length(series), method, "series for", length(x$series$origin$elements), "elements:"), "\n")
                if(length(series) > 5) {
                    cat(paste(series[1:5], collapse=", "),"...")
                } else {
                    cat(paste(series, collapse=", "), ".", sep="")
                }
            }
        }

        ## Print bootstrap
        if(any(names(x$call) == "bootstrap")) {

            message <- paste("\nOrdinated matrix with ", x$call$dimensions, " dimensions.", sep = "")
            if(length(x$call$bootstrap[[1]] > 0)) {
                message <- paste(message, "\nBootstrapped ", x$call$bootstrap[[1]], " times (\"", x$call$bootstrap[[2]], "\" method)", sep = "")
                if(length(x$call$bootstrap[[3]] > 0)) {
                    if(length(x$call$bootstrap[[3]] == (nrow(x$matrix)-3))) {
                        message <- paste(message, " fully rarefied.\n", sep = "")
                    } else {
                        message <- paste(message, " and rarefied to ", paste(x$call$bootstrap[[3]], collapse = ", "), "elements.\n", sep = "")
                    }
                } else {
                    message <- paste(message, ".\n", sep = "")
                }
            }
            cat(message)
        }

    }



    #         #Bootstraps
    #         if(length(data) == 4) {
    #             #head
    #             cat(paste("Bootstrapped ordinated matrix with", length(data$elements), "elements"), "\n")

    #             #series
    #             if(length(data$series) == 1) {
    #                 cat(paste(data$series))
    #             } else {
    #                 cat("Series:\n")
    #                 if(length(data$series) > 5) {
    #                     cat(paste(data$series[1:5], collapse=", "),"...")
    #                 } else {
    #                     cat(paste(data$series, collapse=", "), ".", sep="")
    #                 }
    #             }

    #             #call
    #             cat("\n", data$call, sep="")
    #         }

    #         #Disparity
    #         if(length(data) == 5) {
    #             #head
    #             cat(paste("Disparity measurements across ", length(data$series), " series for ", length(data$elements), " elements", sep=""), "\n")

    #             #series
    #             if(length(data$series) == 1) {
    #                 cat(paste(data$series))
    #             } else {
    #                 cat("Series:\n")
    #                 if(length(data$series) > 5) {
    #                     cat(paste(data$series[1:5], collapse=", "),"...")
    #                 } else {
    #                     cat(paste(data$series, collapse=", "), ".", sep="")
    #                 }
    #             }

    #             #call
    #             cat("\n", data$call, sep="")
    #         }
    #     }
    # }




    # #Sequential tests
    # print.sequential.test <- function(x) {
    #     if(class(data)[1] == "dispRity" & class(data)[2] == "seq.test") {
    #         #Sequential test
    #         call_split <- strsplit(data$call, split = "@")
    #         cat(call_split[[1]][1])
        
    #         #Series
    #         series_names <- unique(unlist(strsplit(names(data$models), split = " - ")))
    #         if(length(series_names) == 1) {
    #             cat(paste(series_names))
    #         } else {
    #             cat("Series:\n")
    #             if(length(series_names) > 5) {
    #                 cat(paste(series_names[1:5], collapse=", "),"...")
    #             } else {
    #                 cat(paste(series_names, collapse=", "), ".", sep="")
    #             }
    #         }

    #         #call
    #         if(!is.na(call_split[[1]][2])) {
    #             cat(paste("\n",call_split[[1]][2], sep = ""))
    #         } else {
    #             cat("\nNo previous call found.\n")
    #         }
    #     }
    #     if(class(data)[1] == "dispRity" & class(data)[2] == "randtest") {
    #         #length_data variable initialisation
    #         length_data <- length(data)

    #         for(model in 1:length_data) {
    #             #The following is a modified version of print.randtest from ade4 v1.4-3
    #             if(length_data != 1) {
    #                 cat(paste("Monte-Carlo test on series", names(data)[[model]], "\n"))
    #             } else {
    #                 cat("Monte-Carlo test\n")
    #             }
    #             #cat("Call: ")
    #             #print(data[[model]]$call)
    #             cat("Observation:", data[[model]]$obs, "\n")
    #             cat("Based on", data[[model]]$rep, "replicates\n")
    #             cat("Simulated p-value:", data[[model]]$pvalue, "\n")
    #             cat("Alternative hypothesis:", data[[model]]$alter, "\n\n")
    #             print(data[[model]]$expvar)
    #             cat("\n")
    #         }
    #     }
    # } 

}