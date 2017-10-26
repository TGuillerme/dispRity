#Sampling from a distribution
sample.distribution <- function(n,args) {
    fun <- args[[1]]
    args[[1]] <- n
    return(do.call(fun, args))
}

## Function for creating a rand
rand.circle <- function(distribution, inner = 0, outer = Inf, ...) {
    ## Azimuth
    theta <- 2 * pi * abs(distribution(1, ...))
        
    ## Radius
    radius <- abs(distribution(1, ...))

    ## Resampling if radius is too big or too small
    while(radius < inner || radius > outer) {
        radius <- abs(distribution(1, ...))
    } 

    return(c(radius*cos(theta), radius*sin(theta)))
}
