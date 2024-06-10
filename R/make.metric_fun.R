## Checking class metric
check.metric <- function(metric) {
    ## Testing the metric
    try(some_name.that.probAbly.doesnt.ExIsT <- metric(matrix(rnorm(25), 5, 5)), silent = TRUE)
    ## If function worked
    if(exists("some_name.that.probAbly.doesnt.ExIsT")) {
        ## Is it a summary metric (one value)?
        if(length(some_name.that.probAbly.doesnt.ExIsT) == 1) {
            return("summary.metric")
        } else {
            return("class.metric")
        }
    } else {
        stop("Invalid metric.", call. = FALSE)
    }
}

check.get.help <- function(metric) {
    ## Does it have the argument name?
    if(any("RAM.helper" %in% names(formals(metric)))) {
        ## Is the argument name not equal to null?
        if(!is.null(formals(metric)$RAM.helper)) {
            ## Is the argument not a logical?
            if(is(formals(metric)$RAM.helper, "logical")) {
                return(formals(metric)$RAM.helper)
            } else {
                return(TRUE)
            }
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}
