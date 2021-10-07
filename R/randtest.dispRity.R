#' @title Random (permutation) test
#'
#' @description Performs a random test (aka permutation test) on a \code{matrix} or a \code{dispRity} object.
#'
#' @param data The \code{matrix} to draw from.
#' @param subsets A \code{vector} of elements to test (or a \code{list} of \code{vectors}).
#' @param replicates A \code{numeric} value for the number of replicates (\code{default = 100}).
#' @param metric A \code{function} to be the statistic to apply to the subset.
#' @param resample \code{logical} whether to resample the full distribution (\code{TRUE}) or the distribution without the subset (\code{FALSE}).
#' @param alter The alternative hypothesis. Can be \code{"two-sided"} (default), \code{"greater"} or \code{"lesser"}.
#' @param ... optional arguments to be passed to \code{metric}.
#' 
#' @details 
#' This test checks whether the metric calculated on a given subset of the data is significantly different from the metric calculated on any random subset of the same size.
#' In other words: does the given subset have a clearly different disparity value than the rest of the data?
#' 
#' First, the \code{metric} (statistic) is applied to the \code{subset} sampled from the \code{data} (population).
#' Second, the \code{metric} is applied to random equally sized subsets from the \code{data}.
#' If the observed difference falls out of the random differences distribution, the differences are significant.
#' This algorithm is based on a similar procedure than in \code{link[ade4]{rantest}}.
#' 
#' If \code{data} is a \code{dispRity} object, the \code{subsets}, \code{metric} and \code{replicates} can be left missing and are automatically inherited from the \code{dispRity} if it contains respectively subsets (from \code{\link{chrono.subsets}} or \code{\link{custom.subsets}}) a \code{metric} (from \code{\link{dispRity}}) and bootstrap draws (from \code{boot.matrix}).
#' 
#' @return
#' This function returns a \code{"randtest"} object that can be passed to the generic S3 functions \code{\link[ade4]{print.randtest}} or \code{\link[ade4]{plot.randtest}}.
#' The output also contains to extra elements \code{output$observed} and \code{output$random} containing the raw results of respectively the observed and random tests.
#' 
#' @examples
#' ## Simple example
#' dummy_matrix <- matrix(rnorm(500), 100, 5)
#' 
#' ## Testing whether the mean of a random subset
#' ## is different than the means of 100 subsets
#' dummy_test <- randtest.dispRity(dummy_matrix,
#'                                 subset = sample(1:100, 20),
#'                                 metric = mean)
#' dummy_test ; plot(dummy_test)
#' 
#' ## Applying this on dispRity objects
#' data(disparity)
#' test_disparity <- test.dispRity(disparity,
#'                                 test = randtest.dispRity)
#' 
#' ## The summarised results
#' summary(test_disparity)
#' 
#' ## Plotting the results
#' plot(test_disparity)
#'
#' @seealso \code{\link[ade4]{randtest}}
#' 
#' @author Thomas Guillerme
#' @export

randtest.dispRity <- function(data, subsets, metric, replicates = 100, resample = TRUE, alter = "two-sided", ...) {
    match_call <- match.call()
    args <- list(...)

    ## Sanitizing
    ## Distribution and subset
    data_class <- check.class(data, c("matrix", "dispRity"))
    inherits_metrics <- inherits_subsets <- FALSE

    if(data_class == "dispRity") {
        pop_size <- nrow(data$matrix[[1]])

        if(missing(subsets)) {
            ## Take subsets from the dispRity object
            subsets <- lapply(data$subsets, function(x) return(x$elements))
            sub_names <- names(subsets)
            inherits_subsets <- TRUE
        }

        if(missing(metric)) {
            ## Take metric from the dispRity object
            metric <- get.metric.from.call(data, what = "fun")
            # args <- get.metric.from.call(data, what = "args")
        }

    } else {
        ## Check the data
        pop_size <- nrow(data)
        pop_names <- rownames(data)
        if(is.null(pop_names)) {
            pop_names <- rownames(data) <- 1:pop_size
        }

        ## Making the data into a dispRity like format
        data <- list(matrix = list(data), call = list(dimensions = ncol(data)))
    }

    if(!inherits_subsets) {
        ## Check the subsets
        sub_names <- names(subsets)
        if(!is(subsets, "list")) {
            ## Make the subsets into a list
            subsets <- list(matrix(subsets, ncol = 1))
        } else {
            ## Make the subsets into a matrix list
            subsets <- lapply(subsets, function(x) matrix(x, ncol = 1))
        }
        all.checks <- function(one_subset, pop_size, pop_names) {
            return(((is(one_subset, "numeric") || is(one_subset, "integer") || is(one_subset, "character")) && length(one_subset) > pop_size) || all(one_subset %in% pop_names))
        }
        if(any(!unlist(lapply(subsets, all.checks, pop_size, pop_names)))) {
            stop("Subsets must be a vector or a list of vector of integers or numeric values that can not exceed the number of rows in data.", call. = FALSE)
        }
    }

    ## Check the metric
    metrics_list <- get.dispRity.metric.handle(metric, match_call, data = data, ...)$levels

    ## Replicates
    check.class(replicates, c("numeric", "integer"))
    check.length(replicates, 1, msg = " must be a single numeric value.")
    if(replicates < 1) {
        stop("At least one replicate must be run.", call. = FALSE)
    }

    ## Resample
    check.class(resample, "logical")
    make.lapply.loop <- ifelse(resample, make.lapply.loop.resample, make.lapply.loop.nosample)

    ## Check alter
    check.method(alter, c("two-sided", "greater", "lesser"), msg = "alter")
    
    ## Set the p-value
    get.p.value <- switch(alter,
        "two-sided" = function(random, observed, replicates) {
            ## Centring the randoms and observed
            center_random <- abs(random - mean(random))
            center_observed <- abs(mean(observed) - mean(random))
            ## Getting the p
            return((sum(center_random >= center_observed) + 1)/(replicates + 1))
        },
        "greater" = function(random, observed, replicates) {
            # Getting the p
            return((sum(random >= mean(observed)) + 1)/(replicates + 1))
        },
        "lesser" = function(random, observed, replicates) {
            # Getting the p
            return((sum(random <= mean(observed)) + 1)/(replicates + 1))
        })
    
    ## Measure the observed and simulated values

    ## Fixed variables from dispRity
    matrix_decomposition <- TRUE
    verbose <- FALSE

    ## Make the lapply loop
    lapply_loop <- lapply(subsets, make.lapply.loop, replicates, pop_size)
    
    ## Calculate all the disparity values
    disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, ...)

    ## Get the observed values
    results <- lapply(disparity, one.randtest, replicates, resample, alter, get.p.value, match_call)
    add.n <- function(one_res, one_sub) {
        one_res$n <- nrow(one_sub)
        class(one_res) <- "randtest"
        return(one_res)
    }
    results <- mapply(add.n, results, subsets, SIMPLIFY = FALSE)

    if(length(results) == 1) {
        return(results[[1]])
    } else {
        ## For match_call
        results <- lapply(results, function(x) {x$call <- "dispRity.randtest"; x})
        ## Add names to the results (if any)
        if(!is.null(sub_names)) {
            names(results) <- sub_names
        }
        class(results) <- c("dispRity", "randtest")
        return(results)
    }
}