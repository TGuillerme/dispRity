#' @title Test disparity metric
#'
#' @description Test whether a metric captures changes trait space size, density and position.
#'
#' @param data A matrix or a \code{dispRity} object (see details).
#' @param metric A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details). If \code{data} is a \code{dispRity} object with disparity already calculated, this argument can be left empty (and the one from \code{data} is recycled)
#' @param ... Optional arguments to be passed to the metric.
#' @param shifts The types of shits to test, can be \code{"random"}, \code{"size"}, \code{"density"}, \code{"evenness"} and \code{"position"}. See details.
#' @param shift.options Optional, a \code{list} of named arguments to be passed to \code{\link{reduce.space}}
#' @param model Optional, which model to fit for testing the metric. See details.
#' @param replicates A \code{numeric} number of replicates to increase variance. By default \code{replicates = 3}. If \code{replicates = 1}, the \code{model} is not run.
#' @param steps The number of steps in the space reduction to output between 10\% and 100\%. By default \code{steps = 10}.
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' 
#' @details
#' For the three non-random shifts: \code{"size"}, \code{"density"}, \code{"evenness"} and \code{"position"}, the function returns both of shifts as:
#' \itemize{
#'      \item \code{"size.inner"} and \code{"size.outer"} removing data from the edges or the centre respectively (contracting the size and "hollowing" it respectively).
#'      \item \code{"density.higher"} and \code{"density.lower"} removing data to increase or decrease density respectively (increasing/decreasing nearest neighbour distance).
#'      \item \code{"evenness.flattened"} and \code{"evenness.compacted"} removing data to from the centre of the distribution or from the edges to resepectively "flatten" or "condense" the distribution.
#'      \item \code{"position.bottom"} and \code{"position.top"} removing data from one side or the other of the space (the sides are selected from the point with lowest/highest scores on each dimensions respectively).
#' }
#' See figure 2 in Guillerme et al. 2020 for more details.
#' 
#' The default \code{model} is a linear model using the following function:
#'     \code{model = function(data) lm(disparity \~ reduction, data)}
#' You can provide your own as long as it is a single function with \code{data} as a single argument. The two terms from data should be called \code{reduction} for the variable on the x axis and \code{disparity} for the variable on the y axis. For example:
#'     \code{model = function(data) nls(disparity \~ a*reduction/(b+reduction), data)}
#' Note that models (like this example) should be specific to the dataset. Any type of model can be fitted but only the ones with an associated \code{summary} function will be correctly displayed by \code{\link{summary.dispRity}}.
#' To not run any model, use \code{model = NULL}.
#' 
#' @return
#' This function outputs a \code{dispRity} object containing a list of simulated reductions in trait space. The results can be accessed through the usual S3 methods (\code{print}, \code{summary}, \code{plot}) or accessed directly through \code{x$<name_of_the_shift>} (e.g. \code{x$random} for the random shift results).  
#'
#' @seealso \code{\link{reduce.space}} \code{\link{dispRity}}
#' 
#' @examples
#' ## Creating a 2D uniform space
#' space <- space.maker(300, 2, runif)
#' 
#' ## A simple test with only 1 replicate for two shifts (random and size):
#' simple_test <- test.metric(space, metric = c(prod, ranges),
#'                            replicates = 1, shifts = c("random", "size")) 
#' 
#' ## Summarising the tests
#' summary(simple_test)
#' 
#' ## Visualising the test
#' plot(simple_test)
#' 
#' ## Note that the tests can take several minutes to run.
#' 
#' ## Testing the sum of variance on all shifts
#' sum_var_test <- test.metric(space, metric = c(sum, variances),
#'                             shifts = c("random", "size", "density", "position"))
#' 
#' ## Summarising the tests
#' summary(sum_var_test)
#' 
#' ## Visualising the test
#' plot(sum_var_test)
#' 
#' ## Applying the test directly on a disparity object
#' data(disparity)
#' median_centroid_test <- test.metric(disparity, shifts = "size")
#' 
#' ## Summarising the tests
#' summary(median_centroid_test)
#' 
#' ## Visualising the test
#' plot(median_centroid_test)
#'  
#' @author Thomas Guillerme
#' 
#' @references
#' Guillerme T, Puttick MN, Marcy AE, Weisbecker V. \bold{2020} Shifting spaces: Which disparity or dissimilarity measurement best summarize occupancy in multidimensional spaces?. Ecol Evol. 2020;00:1-16. (doi:10.1002/ece3.6452)


test.metric <- function(data, metric, ..., shifts, shift.options, model, replicates = 3, steps = 10, dimensions, verbose = FALSE) {

    ## Saving the call
    match_call <- match.call()
    dots <- list(...)

    # warning("DEBUG") ; return(match_call)

    ## Check data input
    metric_name <- NULL
    if(!is(data, "dispRity")) {
        options(warn = -1)
        data <- check.dispRity.data(data)
        options(warn = 0)
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
    metrics_list <- metrics_list$levels

    ## shift
    available_methods <- c("random", "size", "density", "position", "evenness")
    check.method(shifts, available_methods, "shift method ")
    type <- as.list(shifts)

    ## Shift options
    if(missing(shift.options)) {
        shift.options <- NULL
    } else {
        check.class(shift.options, "list")
    }

    ## model options
    if(missing(model) || is.null(model)) {
        ## Linear model
        model <- function(data) lm(disparity ~ reduction, data = data)
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
    ## No model if no replicates
    if(replicates == 1) {
        model <- NULL
    }

    ## steps
    check.class(steps, c("numeric", "integer"))
    check.length(steps, 1, " must be a single numeric value.")
    steps <- seq(from = 0.1, to = 1, length.out = round(steps))

    ## Verbose
    check.class(verbose, "logical")
    
    ## Run all the reductions
    if(verbose) message("Running the space reductions:", appendLF = FALSE)
    all_reductions <- replicate(replicates, lapply(type, reduce.space.one.type, data, steps, shift.options, verbose), simplify = FALSE)
    if(verbose) message("Done.\n", appendLF = FALSE)

    ## Flatten the list
    all_reductions <- lapply(all_reductions, unlist, recursive = FALSE)

    ## Measure disparity on all the shifts
    if(verbose) message("Calculating disparity:", appendLF = FALSE)
    options(warn = -1)
    all_disparity <- lapply(all_reductions, lapply, get.reduced.dispRity, metric, ..., dimensions, verbose)
    options(warn = 0)
    #all_disparity <- lapply(all_reductions, lapply, get.reduced.dispRity, metric, dimensions, verbose)
    if(verbose) message("Done.\n", appendLF = FALSE)

    ## Sort the output
    table_list <- lapply(all_disparity, lapply, make.reduction.tables)
    ## Combine the replicates
    results_list <- list()
    for(one_shift in 1:ifelse(any(shifts == "random"), (length(shifts)*2) - 1, length(shifts)*2)) {
        results_list[[one_shift]] <- do.call(rbind, lapply(table_list, `[[`, one_shift))
    }
    names(results_list) <- names(table_list[[1]])

    ## Test the models
    if(!is.null(model)) {
        models <- lapply(results_list, model)
    } else {
        models <- NULL
    }

    ## Output the results
    call <- list("shifts" = shifts,
                 "replicates" = replicates,
                 "model" = model,
                 "metric" = if(is.null(metric_name)){match_call$metric} else {metric_name})

    output <- list("call" = call, "results" = results_list, "models" = models)
    class(output) <- c("dispRity", "test.metric")

    return(output)
}