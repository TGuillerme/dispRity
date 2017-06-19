#' @title Creating disparity metrics
#'
#' @description Testing the level of disparity metrics
#'
#' @param fun Your very own \code{function}!
#' @param ... Some arguments to be passed to \code{fun}.
#' @param silent \code{logical}; if \code{FALSE} (default), the function will be verbose and give no output; if \code{TRUE}, the function will only output the function level.
#'
#' @details
#' This function tests:
#' \itemize{
#'   \item 1: if your function can deal with a matrix as an \code{input}.
#'   \item 2: which level is your function (level 1, 2 or 3, see \code{\link{dispRity.metric}}).
#'   \item 3: whether the function can properly be implemented in the \code{dispRity} function.
#' }
#' The 3 different metric levels correspond to the dimensions of the output and are:
#' \itemize{
#'   \item "level 1": for functions that decompose a \code{matrix} into a single value.
#'   \item "level 2": for functions that decompose a \code{matrix} into a \code{vector}.
#'   \item "level 3": for functions that transforme the \code{matrix} into anothrer \code{matrix}.
#' }
#' For example, the disparity metric \code{\link[base]{sum}} of \code{\link[dispRity]{variances}} is composed of two metric levels:
#' \itemize{
#'   \item The \code{\link[dispRity]{variances}} (level 2) that calculates the variances per column in a matrix (aggregates a \code{matrix} into a \code{vector}).
#'   \item The \code{\link[base]{sum}} (level 1) that transform the \code{vector} of variances into a single value.
#' }
#' See function example for a concrete illustration (three different level of the function \code{\link[base]{sum}}).
#'
#' @examples
#' ## A level 1 function
#' my_fun <- function(x) sum(x)
#' make.metric(my_fun)
#'
#' ## A level 2 function
#' my_fun <- function(x) apply(x, 2, sum)
#' make.metric(my_fun)
#'
#' ## A level 3 function
#' my_fun <- function(x) (x + sum(x))
#' make.metric(my_fun)
#'
#' @seealso \code{\link{dispRity}}, \code{\link{dispRity.metric}}.
#'
#' @author Thomas Guillerme


make.metric <- function(fun, ..., silent = FALSE) {
    ## Sanitizing
    ## fun
    check.class(fun, "function")

    ## Getting the function name
    match_call<-match.call()

    ## Testing the metric
    ## making the testing matrix
    matrix <- matrix(rnorm(20), 5,4)

    ## Testing the metric
    test <- NULL
    op <- options(warn = 2)
    test <- try(fun(matrix, ...), silent = TRUE)
    #try(test <- fun(matrix), silent = TRUE) ; warning("DEBUG")
    options(op)

    if(any(test == "try-error")) {
        if(silent != TRUE) {
            stop(paste("The provided metric function generated an error or a warning!\nDoes the following works?\n    ", as.expression(match_call$fun),"(matrix(rnorm(9),3,3))\n(the problem can also come from the optional arguments...)", sep = ""))
        }
    } else {

        ##########
        ## What is the output class of the function?
        ##########

        ## If class is matrix -> level3.fun
        if(class(test) == "matrix") {
            fun_type <- "level3"
            if(silent != TRUE) {
                cat(paste(as.expression(match_call$fun)," outputs a matrix object.\n", as.expression(match_call$fun), " is detected as being a level 3 function.", sep = ""))
                cat(paste("\nAdditional level 2 and/or level 1 function(s) will be needed.", sep = ""))
            }
        } else {
            ## If class is numeric
            if(class(test) == "numeric") {
                ## If only one value -> level1.fun
                if(length(test) == 1) {
                    fun_type <- "level1"
                    if(silent != TRUE) {
                        cat(paste(as.expression(match_call$fun)," outputs a single value.\n", as.expression(match_call$fun), " is detected as being a level 1 function.", sep = ""))
                    }
                ## If more than one value -> level1.fun
                } else {
                    fun_type <- "level2"
                    if(silent != TRUE) {
                        cat(paste(as.expression(match_call$fun)," outputs a matrix object.\n", as.expression(match_call$fun), " is detected as being a level 2 function.", sep = ""))
                    }
                }
            } else {
                ## Function provides a wrong output
                if(silent != TRUE) {
                    stop(paste("The provided function did not output a matrix or a numeric vector!\nDoes the following outputs a matrix or a numeric vector?\n", as.expression(match_call$fun),"(matrix(rnorm(9),3,3))", sep = ""))
                }
            }
        }
    }
    ##########
    ## Return the level type for dispRity
    ##########

    if(silent == TRUE) return(fun_type)
}