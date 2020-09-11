## Run the reduction for one type
reduce.space.one.type <- function(type, data, steps, shift.options, verbose) {

    ## Setting up the reduce space call (verbose)
    reduce.space.call <- reduce.space
    if(verbose) {
        body(reduce.space.call)[[2]] <- substitute(message(".", appendLF = FALSE))
    }

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

    ## Run the reductions
    to_remove <- lapply(add.steps.to.args(make.reduce.space.args(data[[1]], type, shift.options), c(0, steps)), function(args, fun) do.call(fun, args), fun = reduce.space.call)
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
            "position" = {names(output) <- c("position.bottom", "position.top")},
            "evenness" = {names(output) <- c("evenness.flattened", "evenness.compacted")},
            )

        ## Make the dispRity objects
        return(output)
    } else {
        return(list("random" = custom.subsets(data, group = available_data1)))
    }
}

## Getting the disparity
get.reduced.dispRity <- function(reduction, metric, dimensions, verbose, ...) {
    
    ## Duplicate function for verbose
    dispRity.verbose <- dispRity::dispRity
    if(verbose) {
        ## Remake the function verbose
        body(dispRity.verbose)[[16]] <- substitute(silent <- "silent")
        body(dispRity.verbose)[[18]] <- substitute(silent <- "silent")
    }

    ## Run the disparity
    options(warn = -1)
    return(dispRity.verbose(data = reduction, metric = metric, ..., dimensions = dimensions, verbose = verbose, between.groups = FALSE)$disparity)
    # return(dispRity.verbose(data = reduction, metric = metric, dimensions = dimensions, verbose = verbose, between.groups = FALSE)$disparity)
}

## Transforming the tables
make.reduction.tables <- function(one_type_results) {
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
    return(data.frame("reduction" = as.numeric(rep(names(disparity_values), result_lengths)),
                      "disparity" = unname(unlist(disparity_results))))
}
