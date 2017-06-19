#' @title Prints a \code{dispRity} object.
#'
#' @description Summarises the content of a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise it's content (\code{FALSE} - default).
#' @param ... further arguments to be passed to \code{print} or to \code{print.dispRity}.
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
#' @seealso \code{\link{cust.subsamples}}, \code{\link{time.subsamples}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
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
# source("time.subsamples.R") ; source("time.subsamples_fun.R")
# source("cust.subsamples.R") ; source("cust.subsamples_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data_simple <- BeckLee_mat50
# data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
# data_subsamples_simple <- time.subsamples(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
# data_subsamples_boot <- boot.matrix(data_subsamples_simple, bootstraps = 11, rarefaction = c(5,6))
# data <- dispRity(data_subsamples_boot, metric = c(variances))

print.dispRity <- function(x, all = FALSE, ...) {


    if(all) {
        ## Print everything
        tmp <- x
        class(tmp) <- "list"
        print(tmp)

    } else {

        if(length(class(x)) > 1) {
            ## randtest
            if(class(x)[2] == "randtest") {

                ## Remove the call (messy)
                remove.call <- function(element) {
                    element$call <- "dispRity::null.test"
                    return(element)
                }
                x <- lapply(x, remove.call)


                if(length(x) == 1) {
                    print(x[[1]])
                } else {
                    tmp <- x
                    class(tmp) <- "list"
                    print(tmp) 
                }
                return()
            }
        }

        if(length(x$call) == 0) {
            cat("Empty dispRity object.\n")
            return()
        }

        cat(" ---- dispRity object ---- \n")

        ## Print the matrix informations
        if(any(names(x$call) == "subsamples") && length(x$subsamples) != 1) {
            ## Get the number of subsamples (minus origin)
            subsamples <- names(x$subsamples)

            ## Check if there are more than one subsamples
            if(length(subsamples) != 1) {

                ## Get the method
                method <- x$call$subsamples
                if(length(method) != 1) {
                    method <- paste(method[1], " (", method[2],")", sep = "")
                }
                if(method == "customised") {
                    cat(paste(length(subsamples), method, "subsamples for", nrow(x$matrix), "elements"))    
                } else {
                    cat(paste(length(subsamples), method, "time subsamples for", nrow(x$matrix), "elements"))
                }
                if(length(x$call$dimensions) != 0) cat(paste(" with", x$call$dimensions, "dimensions"), sep = "")
                cat(":\n")
                if(length(subsamples) > 5) {
                    cat("    ",paste(subsamples[1:5], collapse=", "),"...\n")
                } else {
                    cat("    ",paste(subsamples, collapse=", "), ".\n", sep="")
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

            cat(paste("Disparity was calculated as:", paste(as.character(x$call$disparity$metrics), collapse = ", ")))
            cat(".\n")
        }
    }
}