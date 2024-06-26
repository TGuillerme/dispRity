## Get specific elements from call
get.metric.from.call <- function(data, what) {
    # if(length(data$call$disparity$metrics[[1]]) > 1) {
    #     return(lapply(as.list(data$call$disparity$metrics[[1]]), function(metric) eval(parse(text = metric)))[-1])
    # } else {
    #      return(eval(parse(text = data$call$disparity$metrics[[1]])))
    # }
    return(unlist(data$call$disparity$metrics[[what]]))
}

## Generating the null model
make.null.model <- function(data, replicates, null.distrib, null.args, null.cor, null.scree, scale, metric, args) {

    options(warn = -1)

    if(!scale) {
        if(is.null(args)){
                null_models_result <- replicate(replicates, summary(dispRity(
                        space.maker(nrow(data$matrix[[1]]),
                                    dimensions = length(data$call$dimensions),
                                    distribution = null.distrib,
                                    arguments = null.args,
                                    cor.matrix = null.cor,
                                    scree = null.scree),
                            metric = metric, dimensions = 1:length(data$call$dimensions)),
                    cent.tend = mean, quantiles = 1)$obs)
        } else {
                null_models_result <- replicate(replicates, summary(dispRity(
                        space.maker(nrow(data$matrix[[1]]),
                                    dimensions = length(data$call$dimensions),
                                    distribution = null.distrib,
                                    arguments = null.args,
                                    cor.matrix = null.cor,
                                    scree = null.scree),
                            metric = metric, dimensions = 1:length(data$call$dimensions), args),
                    cent.tend = mean, quantiles = 1)$obs)
                }
    } else {
        if(is.null(args)) {
            null_models_result <- replicate(replicates, summary(dispRity(
                    scale(
                        space.maker(nrow(data$matrix[[1]]),
                                    dimensions = length(data$call$dimensions),
                                    distribution = null.distrib,
                                    arguments = null.args,
                                    cor.matrix = null.cor,
                                    scree = null.scree)
                        ),
                    metric = metric, dimensions = 1:length(data$call$dimensions)),
            cent.tend = mean, quantiles = 1)$obs)
        } else {
            null_models_result <- replicate(replicates, summary(dispRity(
                    scale(
                        space.maker(nrow(data$matrix[[1]]),
                                    dimensions = length(data$call$dimensions),
                                    distribution = null.distrib,
                                    arguments = null.args,
                                    cor.matrix = null.cor,
                                    scree = null.scree)
                        ),
                    metric = metric, dimensions = 1:length(data$call$dimensions), args),
            cent.tend = mean, quantiles = 1)$obs)
        }
    }
    options(warn = 0)
    return(null_models_result)
}


# unlist(extract.dispRity(dispRity(
#                     scale(
#                         space.maker(nrow(data$matrix[[1]]),
#                                     dimensions = length(data$call$dimensions),
#                                     distribution = null.distrib,
#                                     arguments = null.args,
#                                     cor.matrix = null.cor,
#                                     scree = null.scree)
#                         ),
#                     metric = metric, dimensions = data$call$dimensions))))