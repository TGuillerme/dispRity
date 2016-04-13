#Checking class metric
check.metric<-function(metric) {
    #Testing the metric test
    try(some_name.that.probAbly.doesnt.ExIsT<-metric(matrix(rnorm(25),5,5)), silent=TRUE)
    #If function worked
    if(exists("some_name.that.probAbly.doesnt.ExIsT")) {
        #Is it a level 1 metric (one value)?
        if(length(some_name.that.probAbly.doesnt.ExIsT) == 1) {
            return("level1.metric")
        }
        #Is it a level 2 metric (a distribution of values)?
        if(is.vector(some_name.that.probAbly.doesnt.ExIsT) == TRUE) {
            return("level2.metric")
        }
        #Is it a level 3 metric (a distribution of values)?
        if(is.matrix(some_name.that.probAbly.doesnt.ExIsT) == TRUE) {
            return("level3.metric")
        }
    } else {
        stop("Invalid metric.")
    }
}