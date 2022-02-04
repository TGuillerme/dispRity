## Reduce the matrix
make.reduce.space.args <- function(data, type, shift.options) {

    ## Default arguments
    args_list <- list("space" = data,
                      "type" = type,
                      "verbose" = FALSE,
                      "return.optim" = FALSE)

    ## Optional arguments
    if(!is.null(shift.options$parameters)) {
        args_list <- c(args_list, "parameters" = shift.options$parameters)
    }
    if(!is.null(shift.options$tuning)) {
        args_list <- c(args_list, "tuning" = shift.options$tuning)
    }

    return(args_list)
}

## Add the shifts
add.steps.to.args <- function(type_args, steps) {
    return(lapply(steps, function(x, args) return(c(args, "remove" = x)), type_args))
}

## Transform the reductions into dispRity format
transform.to.dispRity <- function(to_remove, type, data, steps, shift.options, verbose) {

    ## Setting up the reduce space call (verbose)
    available_data1 <- lapply(to_remove[-1], function(x, matrix) return(rownames(matrix)[x]), data[[1]]) # select inner (minimum, low, positive)
    names(available_data1) <- as.character(steps*100)

    if(type != "random") {
        available_data2 <- lapply(to_remove[-length(to_remove)], function(x, matrix) return(rownames(matrix)[!x]), data[[1]]) # select outer (maximum, high, negative)
        names(available_data2) <- rev(as.character(steps*100))
        output <- list("inner" = custom.subsets(data, group = available_data1),
                       "outer" = custom.subsets(data, group = available_data2))

        ## Renaming the types
        switch(type,
            "size"     = {names(output) <- c("size.inner", "size.outer")},
            "density"  = {names(output) <- c("density.higher", "density.lower")},
            "position" = {names(output) <- c("position.top", "position.bottom")},
            "evenness" = {names(output) <- c("evenness.flattened", "evenness.compacted")},
            )

        ## Make the dispRity objects
        return(output)
    } else {
        return(list("random" = custom.subsets(data, group = available_data1)))
    }
}

## Run the reduction for one type
reduce.space.one.type <- function(type, data, steps, shift.options, verbose) {

    ## Setting up the reduce space call (verbose)
    reduce.space.call <- reduce.space
    if(verbose) {
        body(reduce.space.call)[[2]] <- substitute(message(".", appendLF = FALSE))
    }

    ## Run the reductions
    to_remove <- lapply(add.steps.to.args(make.reduce.space.args(data[[1]], type, shift.options), c(0, steps)), function(args, fun) do.call(fun, args), fun = reduce.space.call)

    ## Make it look fancy
    if(type != "random") {
        output <- list(to_remove[-1], lapply(to_remove[-length(to_remove)], `!`))
        names(output[[1]]) <- as.character(steps*100)
        names(output[[2]]) <- rev(as.character(steps*100))
        switch(type,
            "size"     = {names(output) <- c("size.inner", "size.outer")},
            "density"  = {names(output) <- c("density.higher", "density.lower")},
            "position" = {names(output) <- c("position.top", "position.bottom")},
            "evenness" = {names(output) <- c("evenness.flattened", "evenness.compacted")},
            )
    } else {
        output <- list("random" = to_remove[-1])
    }
    return(output)
}

## Getting the disparity
get.reduced.dispRity <- function(reduction, metric, data, verbose, ...) {

    ## Run the disparity
    options(warn = -1)

    ## Calculating fast disparity
    output <- lapply(reduction, dispRity.fast, data[[1]], metric, ...)
    # output <- lapply(reduction, dispRity.fast, data[[1]], metric)

    options(warn = 0)
    if(verbose) message(".", appendLF = FALSE)
    return(output)
}

## Transforming the tables
make.reduction.tables <- function(one_type_results, steps) {
    ## Get the disparity values
    disparity_results <- as.list(rep(NA, length(one_type_results)))
    disparity_values <- lapply(one_type_results, function(x) unname(unlist(x)))
    result_lengths <- unlist(lapply(disparity_values, length))
    if(length(nas <- which(result_lengths == 0)) > 0) {
        disparity_results[-nas] <- disparity_values[-nas]
        result_lengths[nas] <- 1
    } else {
        disparity_results <- disparity_values
    }

    ## Make into a dataframe
    return(data.frame("reduction" = as.numeric(rep(as.character(steps*100), result_lengths)),
                      "disparity" = unname(unlist(disparity_results))))
}
