#' @title Creating disparity metrics
#'
#' @description IN DEVELOPEMENT
#'
#' @seealso \code{\link{dispRity}} and \code{\link{dispRity.metric}}.
#'
#' @author Thomas Guillerme


make.metric<-function(fun, silent=FALSE) {
    #Sanitizing
    #fun
    check.class(fun, "function")

    #Getting the function name
    match_call<-match.call()

    #Testing the metric
    #making the testing matrix
    matrix <- matrix(rnorm(20), 5,4)

    #Testing the metric
    test <- NULL
    try(test <- fun(matrix), silent=TRUE)

    #Did the test failed?
    if(is.null(test)) {
        if(silent == FALSE) stop(paste("The provided function did not output anything!\nDoes the following works?\n", match_call$fun,"(matrix(rnorm(9),3,3))", sep=""))
    } else {

        #What is the output class of the function?

        #If class is matrix -> mat.trans
        if(class(test) == "matrix") {
            fun_type <- "mat.trans"
            if(silent == FALSE) {
                cat(paste(match_call$fun," outputs a matrix object.\n", match_call$fun, " is detected as being a matrix transformation (mat.trans) function.", sep=""))
                cat(paste("\nAdditional vector aggregate (vec.aggr) and/or value aggregate\n    (val.aggr) function(s) will be needed.", sep=""))
            }
        } else {
            #If class is numeric
            if(class(test) == "numeric") {
                #If only one value -> val.aggr
                if(length(test) == 1) {
                    fun_type <- "val.aggr"    
                    if(silent == FALSE) {
                        cat(paste(match_call$fun," outputs a single value.\n", match_call$fun, " is detected as being a value aggregate (val.aggr) function.", sep=""))
                    }
                #If more than one value -> val.aggr
                } else {
                    fun_type <- "vec.trans"
                    if(silent == FALSE) {
                        cat(paste(match_call$fun," outputs a matrix object.\n", match_call$fun, " is detected as being a vector aggregate (vec.aggr) function.", sep=""))
                        cat(paste("\nAdditional value aggregate (val.aggr) function will be needed.", sep=""))
                    }
                }
            } else {
                #Function provides a wrong output
                if(silent == FALSE) stop(paste("The provided function did not output a matrix or a vector!\nDoes the following outputs a matrix or a vector?\n", match_call$fun,"(matrix(rnorm(9),3,3))", sep=""))
            }
        }

    }
}