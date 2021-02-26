#' @title Random (permutation) test
#'
#' @description Performs a random test (aka permutation test) on a \code{matrix} or a \code{dispRity} object.
#'
#' @param data The \code{matrix} to draw from.
#' @param subset A \code{vector} of elements to test (or a \code{list} of \code{vectors}).
#' @param replicates A \code{numeric} value for the number of replicates (\code{default = 100}).
#' @param metric A \code{function} to be the statistic to apply to the subset.
#' @param resample \code{logical} wether to resample the full distribution (\code{TRUE}) or the distribution without the subset (\code{FALSE}).
#' @param alternative The alternative hypothesis. Can be \code{"two-sided"} (default), \code{"greater"} or \code{"lesser"}.
#' @param ... optional arguments to be passed to \code{metric}.
#' 
#' @details First, the \code{metric} (statistic) is applied to the \code{subset} sampled from the \code{data} (population).
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
#' ## Applying this on dispRity objects
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
#' @seealso \code{link[ade4]{randtest}}
#' 
#' @author Thomas Guillerme
#' @export
#' @importFrom stats sd var rnorm
#' @importFrom graphics hist

randtest <- function(data, subsets, metric, replicates, resample = TRUE, alternative = "two-sided", ...) {
    match_call <- match.call()

    ## Sanitizing
    ## Distribution and subset
    data_class <- check.class(data, c("numeric", "dispRity"))

    if(data_class == "dispRity") {
        pop_size <- nrow(data$matrix[[1]])

        if(missing(subsets)) {
            ## Take subsets from the dispRity object
            subsets <- 
            inherits_subsets <- TRUE
        } else {
            inherits_subsets <- FALSE
        }
        if(missing(metric)) {
            ## Take metric from the dispRity object
            metric <-
            inherits_metric <- TRUE
        } else {
            inherits_metric <- FALSE
        }
        if(missing(replicates)) {
            ## Take metric from the dispRity object
            # replicates <-
        } 

    } else {

        ## Check the data
        check.class(data, "matrix", " must be a matrix or a dispRity object.")
        pop_size <- nrow(data)
        pop_names <- rownames(data)
        if(pop_names) {
            pop_names <- rownames(data) <- 1:pop_size
        }


        ## Check the subsets
        if(!is(subsets, "list")) {
            subsets <- list(matrix(subsets, ncol = 1))
        }
        all.checks <- function(one_subset, pop_size, pop_names) {
            return(((is(one_subset, "numeric") || is(one_subset, "integer") || is(one_subset, "character")) && length(one_subset) > pop_size) || all(one_subset %in% pop_names))
        }
        if(any(!unlist(lapply(subsets, all.checks)))) {
            stop("Subset must be a vector or a list of vector of integers or numeric values that can not exceed the number of rows in data.")
        }

        ## Check the metric
        metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data), ...)
    }

    ## Replicates
    check.class(replicates, c("numeric", "integer"))
    check.length(replicates, 1, msg = " must be a single numeric value.")
    if(replicates < 1) {
        stop("At least one replicate must be run.")
    }

    ## Resample
    check.class(resample, "logical")

    ## Check alternative
    check.method(alternative, c("two-sided", "greater", "lesser"), msg = "alternative")
    
    ## Set the p-value
    get.p.value <- switch(alternative,
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
    verbose <- metric_is_between.groups <- FALSE

    ## Make the lapply loop
    make.lapply.loop <- function(one_subset) {
        return(list("elements" =one_subset, replicate(replicates, sample(1:pop_size, length(one_subset), replace = resample))))
    }
    lapply_loop <- lapply(subsets, make.lapply.loop)
    
    ## Calculate all the disparity values
    disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, metric_is_between.groups, ...)

    ## Get the observed values
    results <- lapply(disparity, one.randtest, replicates, resample, alternative)
    
    if(length(results) == 1) {
        return(results[[1]])
    } else {
        class(results) <- c("dispRity", "randtest")
    }

}