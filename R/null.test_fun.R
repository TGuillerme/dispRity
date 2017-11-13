#Get specific elements from call
get.metric.from.call <- function(data) {
    if(length(data$call$disparity$metric[[1]]) > 1) {
        return(lapply(as.list(data$call$disparity$metric[[1]]), function(metric) eval(parse(text = metric)))[-1])
    } else {
         return(eval(parse(text = data$call$disparity$metric[[1]])))
    }
}

#Generating the null model§
make.null.model <- function(data, replicates, null.distrib, null.args, null.cor, null.scree, scale) {
    if(!scale) {
        null_models_result <- replicate(replicates, summary(dispRity(
            space.maker(nrow(data$matrix),
                        dimensions = data$call$dimensions,
                        distribution = null.distrib,
                        arguments = null.args,
                        cor.matrix = null.cor,
                        scree = null.scree)
            , metric = get.metric.from.call(data)), cent.tend = mean, quantiles = 1)$obs )
    } else {
        null_models_result <- replicate(replicates, summary(dispRity(
            scale(
            space.maker(nrow(data$matrix),
                        dimensions = data$call$dimensions,
                        distribution = null.distrib,
                        arguments = null.args,
                        cor.matrix = null.cor,
                        scree = null.scree)
                )
        , metric = get.metric.from.call(data)), cent.tend = mean, quantiles = 1)$obs )
    }
    return(null_models_result)
}