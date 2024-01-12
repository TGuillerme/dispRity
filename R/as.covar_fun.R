## Cleanly evalute the covarness of a function
eval.covar <- function(fun, null.return = NULL) {
    is_covar <- FALSE
    if(!is.null(fun)) {
        return(all(c("fun_is_covar", "TRUE") %in% as.character(body(fun)[[2]])))
    } else {
        return(null.return)
    }
}