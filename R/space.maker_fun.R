#Sampling from a distribution
sample.distribution <- function(n,args) {
    fun <- args[[1]]
    args[[1]] <- n
    return(do.call(fun, args))
}