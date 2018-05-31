#' @title Prints a \code{dispRity} object.
#'
#' @description Summarises the content of a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise its contents (\code{FALSE} - default).
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
#' @seealso \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG dispRity.R")
# library(dispRity)
# source("sanitizing.R")
# source("dispRity.R")
# source("dispRity_fun.R")
# source("dispRity.metric.R")
# source("make.dispRity.R")
# source("fetch.dispRity.R")
# source("boot.matrix.R") ; source("boot.matrix_fun.R")
# source("chrono.subsets.R") ; source("chrono.subsets_fun.R")
# source("custom.subsets.R") ; source("custom.subsets_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data_simple <- BeckLee_mat50
# data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
# data_subsets_simple <- chrono.subsets(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
# data_subsets_boot <- boot.matrix(data_subsets_simple, bootstraps = 11, rarefaction = c(5,6))
# data <- dispRity(data_subsets_boot, metric = c(variances))

 
print.dispRity <- function(x, all = FALSE, ...) {

    match_call <- match.call()
    x_name <- match_call$x

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
            if(class(x)[2] == "model.test") {
                cat("Disparity evolution model fitting:\n")
                cat(paste0("Call: ", as.expression(x$call), " \n\n"))
                
                print(x$aic.models)

                cat(paste0("\nUse ", x_name, "$full.details for displaying the models details\n"))
                cat(paste0("or summary(", x_name, ") for summarising them.\n"))

                return()
            }

            if(class(x)[2] == "dtt" && length(x) != 2) {

                ## Tested dtt
                cat("Disparity-through-time test (modified from geiger:dtt)\n")
                cat(paste0("Call: ", as.expression(x$call), " \n\n"))

                cat(paste0("Observation: ", x$MDI , "\n\n"))

                cat(paste0("Model: ", x$call$model , "\n"))
                cat(paste0("Based on ", length(x$sim_MDI) , " replicates\n"))
                cat(paste0("Simulated p-value: ", x$p_value , "\n"))
                cat(paste0("Alternative hypothesis: ", x$call$alternative , "\n\n"))

                print(c("Mean.dtt" = mean(x$dtt), "Mean.sim_MDI" = mean(x$sim_MDI), "var.sim_MDI" = var(x$sim_MDI)))

                cat(paste0("\nUse plot.dispRity() to visualise."))
                return()
            } else {
                ## raw dtt
                ## Fake an object with no attributes
                x_tmp <- x
                class(x_tmp) <- "list"
                print(x_tmp)
                cat(paste0("- attr(*, \"class\") = \"dispRity\" \"dtt\"\n"))
                cat(paste0("Use plot.dispRity to visualise."))
            }
        }

        if(length(x$call) == 0) {
            if(!is.null(x$matrix) && class(x$matrix) == "matrix") {
                cat(" ---- dispRity object ---- \n")
                dims <- dim(x$matrix)
                cat(paste0("Contains only a matrix ", dims[1], "x", dims[2], "."))
            } else {
                cat("Empty dispRity object.\n")
            }
            return()
        }

        cat(" ---- dispRity object ---- \n")

        ## Print the matrix information
        if(any(names(x$call) == "subsets") && length(x$subsets) != 1) {
            ## Get the number of subsets (minus origin)
            subsets <- names(x$subsets)

            ## Check if there is more than one subset
            if(length(subsets) != 1) {

                ## Get the method
                method <- x$call$subsets
                if(length(method) != 1) {
                    method <- paste(method[1], " (", method[2],")", sep = "")
                }
                if(method == "customised") {
                    cat(paste(length(subsets), method, "subsets for", nrow(x$matrix), "elements"))    
                } else {
                    cat(paste(length(subsets), method, "time subsets for", nrow(x$matrix), "elements"))
                }
                if(length(x$call$dimensions) != 0) cat(paste(" with", x$call$dimensions, "dimensions"), sep = "")
                cat(":\n")
                if(length(subsets) > 5) {
                    cat("    ",paste(subsets[1:5], collapse=", "),"...\n")
                } else {
                    cat("    ",paste(subsets, collapse=", "), ".\n", sep="")
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