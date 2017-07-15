## Checking class metric
check.metric <- function(metric) {
    ## Testing the metric test
    try(some_name.that.probAbly.doesnt.ExIsT <- metric(matrix(rnorm(25),5,5)), silent=TRUE)
    ## If function worked
    if(exists("some_name.that.probAbly.doesnt.ExIsT")) {
        ## Is it a summary metric (one value)?
        if(length(some_name.that.probAbly.doesnt.ExIsT) == 1) {
            return("summary.metric")
        } else {
            return("class.metric")
        }
    } else {
        stop("Invalid metric.")
    }
}