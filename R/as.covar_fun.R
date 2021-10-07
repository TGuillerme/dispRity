## Cleanly evalute the covarness of a function
eval.covar <- function(fun, null.return = NULL) {
    is_covar <- FALSE
    if(!is.null(fun)) {
        return(all(c("is_covar", "TRUE") %in% as.character(body(fun)[[length(body(fun))]])))
    } else {
        return(null.return)
    }
}