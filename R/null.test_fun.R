#Get specific elements from call
get.from.call <- function(data, what) {
    if(what == "metric") {
        call_out <- strsplit(strsplit(data$call, split = "Disparity calculated as: ")[[1]][[2]], split = " for ")[[1]][[1]]
    }
    if(what == "dimensions") {
        call_out <- strsplit(strsplit(data$call, split = " for ")[[1]][[2]], split = " dimensions")[[1]][[1]]
    }
    return(eval(parse(text = call_out)))
}

#Generating the null model
make.null.model <- function(data, replicates, null.distrib, null.args, scale) {
    if(scale == FALSE) {
        null_models_result <- replicate(replicates, summary(dispRity(
            space.maker(as.numeric(length(data$elements)), dimensions = get.from.call(data, "dimensions"), null.distrib, null.args)
        , metric = get.from.call(data, "metric")))$observed)
    } else {
        null_models_result <- replicate(replicates, summary(dispRity(
            scale(space.maker(as.numeric(length(data$elements)), dimensions = get.from.call(data, "dimensions"), null.distrib, null.args))
        , metric = get.from.call(data, "metric")))$observed)
    }
    return(null_models_result)
}
