## Detecting the bin age lower or greater than a value
detect.bin.age <- function(data, value, greater = FALSE) {
    ## Detect the bin before the extinction time
    bin_times <- unlist(sapply(names(data$subsets), strsplit, split = " - ", simplify = FALSE), recursive = FALSE)

    ## Detecting the bin age
    detect.bin.ages.lapply <- function(one_bin, value, greater) {
        if(greater) {
            return(ifelse(as.numeric(one_bin)[2] >= value, TRUE, FALSE))
        } else {
            return(ifelse(as.numeric(one_bin)[1] <= value, TRUE, FALSE))
        }
    }

    return(unlist(lapply(bin_times, detect.bin.ages.lapply, value, greater)))
}