#' @title Test disparity metric
#'
#' @description Test whether a metric captures changes trait space size, density and position.
#'
#' @param data A matrix or a \code{dispRity} object (see details).
#' @param metric A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details). If \code{data} is a \code{dispRity} object with disparity already calculated, this argument can be left empty (and the one from \code{data} is recycled)
#' @param ... Optional arguments to be passed to the metric.
#' @param shifts The types of shits to test, can be \code{"random"}, \code{"size"}, \code{"density"} and \code{"position"}.
#' @param shift.options Optional, a \code{list} of named arguments to be passed to \code{\link{reduce.space}}
#' @param model Optional, which model to fit for testing the metric. See details.
#' @param replicates A \code{numeric} number of replicates to increase variance. By default \code{replicates = 3}.
#' @param steps The number of steps in the space reduction to output between 1\% and 100\%. By default \code{steps = 10}.
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' 
#' @details
#' The default \code{model} is a linear model using the following function:
#'     \code{model = function(data) lm(reduction \~ disparity, data)}
#' You can provide your own as long as it is a single function with \code{data} as a single argument. The two terms from data should be called \code{reduction} for the variable on the x axis and \code{disparity} for the variable on the y axis. For example:
#'     \code{model = function(data) nls(disparity \~ a*reduction/(b+reduction), data)}
#' Note that models (like this example) should be specific to the dataset. Any type of model can be fitted but only the ones with an associated \code{summary} function will be correctly displayed by \code{\link{summary.test.metric}}.
#' 
#'
#' @examples
#' ## Load a disparity data
#' data(disparity)
#' 
#' ## Testing the metric used in the disparity data 
#' median_centroid_test <- test.metric(disparity, shifts = c("random", "size"))
#' 
#' ## Summarising the results
#' summary(sum_var_test)
#' 
#' ## Visualising the results
#' plot(sum_var_test)
#'  
#' @author Thomas Guillerme
#' 
#' @references
#' Guillerme T, Puttick MN, Marcy AE, Weisbecker V. \bold{2020} Shifting spaces: Which disparity or dissimilarity measurement best summarize occupancy in multidimensional spaces?. Ecol Evol. 2020;00:1-16. (doi:10.1002/ece3.6452)


# source("sanitizing.R")
# source("dispRity_fun.R")
# source("test.metric_fun.R")

test.metric <- function(data, metric, ..., shifts, shift.options, model, replicates = 3, steps = 10, dimensions, verbose = FALSE) {

    ## Saving the call
    match_call <- match.call()
    dots <- list(...)

    # warning("DEBUG") ; return(match_call)

    ## Check data input
    metric_name <- NULL
    if(!is(data, "dispRity")) {
        data <- check.dispRity.data(data)
    } else {
        ## See if the metric needs to be recycled
        if(missing(metric) && !is.null(data$call$disparity$metrics)) {
            metric <- data$call$disparity$metrics$fun
            metric_name <- data$call$disparity$metrics$name
        }
        data <- data$matrix
    }

    ## Get the metric list
    metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data[[1]]), ...)
    # metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data[[1]]))

    ## shift
    available_methods <- c("random", "size", "density", "displacement")
    check.method(shifts, available_methods, "shift method ")
    type <- as.list(shifts)

    ## Shift options
    if(missing(shift.options)) {
        shift.options <- NULL
    } else {
        check.class(shift.options, "list")
    }

    ## model options
    if(missing(model)) {
        ## Linear model
        model <- function(data) lm(reduction ~ disparity, data = data)
    } else {
        check.class(model, "function")
        ## Check the arguments
        arguments <- names(formals(model))
        if(length(arguments) > 1 || arguments != "data") {
            stop("model function argument can only take \"data\" as an argument.")
        }
    }

    ## replicates
    check.class(replicates, c("numeric", "integer"))
    check.length(replicates, 1, " must be a single numeric value.")

    ## steps
    check.class(steps, c("numeric", "integer"))
    check.length(steps, 1, " must be a single numeric value.")
    steps <- seq(from = 0, to = 0.9, length.out = round(steps))

    ## Verbose
    check.class(verbose, "logical")
    
    ## Run all the reductions
    if(verbose) message("Running the space reductions:", appendLF = FALSE)
    all_reductions <- replicate(replicates, lapply(type, reduce.space.one.type, data, steps, shift.options, verbose), simplify = FALSE)
    if(verbose) message("Done.\n", appendLF = FALSE)

    ## Measure disparity on all the shifts
    if(verbose) message("Calculating disparity:", appendLF = FALSE)
    all_disparity <- lapply(all_reductions, lapply, get.reduced.dispRity, metric, ..., dimensions, verbose)
    #all_disparity <- lapply(all_reductions, lapply, get.reduced.dispRity, metric, dimensions, verbose)
    if(verbose) message("Done.\n", appendLF = FALSE)

    ## Sort the output
    table_list <- lapply(all_disparity, lapply, make.reduction.tables)
    ## Combine the replicates
    results_list <- list()
    for(one_shift in 1:length(shifts)) {
        results_list[[one_shift]] <- do.call(rbind, lapply(table_list, `[[`, one_shift))
    }
    names(results_list) <- shifts

    ## Test the models
    models <- lapply(results_list, model)

    ## Output the results
    call <- list("shifts" = shifts,
                 "replicates" = replicates,
                 "model" = model,
                 "metric" = if(is.null(metric_name)){match_call$metric} else {metric_name})

    output <- list("call" = call, "results" = results_list, "models" = models)
    class(output) <- c("dispRity", "test.metric")

    return(output)

    #plot(results_list$size[, c(2,1)])
}