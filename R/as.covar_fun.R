## Cleanly evalute the covarness of a function
eval.covar <- function(fun, null.return = NULL) {
    fun_is_covar <- FALSE
    if(!is.null(fun)) {
        ## Check if it can evaluate covar
        if(length(grep("fun_is_covar", as.character(body(fun)))) > 0) {
            ## evaluate the fun_is_covar variable 
            eval(body(fun)[[2]])
        } 
        # return(all(c("fun_is_covar", "TRUE") %in% as.character(body(fun)[[2]])))
        return(fun_is_covar)
    } else {
        return(null.return)
    }
}