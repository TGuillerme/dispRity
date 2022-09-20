#' @title Random (permutation) test
#'
#' @description Performs a random test (aka permutation test) on a \code{matrix} or a \code{dispRity} object.
#'
#' @param data The \code{matrix} or a \code{dispRity} object to draw from.
#' @param subsets A \code{vector} of elements to test (or a \code{list} of \code{vectors} - see details).
#' @param replicates A \code{numeric} value for the number of replicates (\code{default = 100}).
#' @param metric A \code{function} to be the statistic to apply to the subset.
#' @param resample \code{logical} whether to resample the full distribution (\code{TRUE}; default) or the distribution without the subset (\code{FALSE}).
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
#' If \code{data} is a \code{dispRity} object subsets can be a list of subsets to compare for example \code{list(c("A", "B"), c("B", "A"))} will run two tests comparing respectively sample A to B and B to A. \emph{Note} that it will only compare these two samples and use their combined size as the population size, if you want to compare a subset to all the subsets you can use \code{list(c("A")} or write down the specific subsets to be used.
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
#' ## Applying this on a dispRity object with specific subset comparisons
#' test_disparity2 <- randtest.dispRity(disparity, subsets = list(
#'      ## Comparing subset 90 to the whole population (traitspace)
#'      c(observed = "90"),
#'      ## Comparing subset "70" to "90", "70" and "30"
#'      c(observed = "70", random = c("90", "70", "30"))))
#' 
#' ## Summarising and plotting the results
#' summary(test_disparity2)
#' plot(test_disparity2)
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
    subsets_names <- NULL

    if(data_class == "dispRity") {

        sample_pop <- 1:nrow(data$matrix[[1]])
        pop_names <- rownames(data)

        if(missing(subsets)) {
            ## Take subsets from the dispRity object
            subsets <- lapply(data$subsets, function(x) return(x$elements))
            sub_names <- names(subsets)
            inherits_subsets <- TRUE
            ## Set the sampling population list
            sample_pop <- replicate(length(subsets), list(sample_pop))
        } else {
            ## Get the subset names (if available)
            if(!is.null(names(subsets))) {
                subsets_names <- names(subsets)
            }

            ## Split the subsets in observed and random
            subsets_obs  <- lapply(subsets, `[`, 1)
            subsets_rand <- lapply(subsets, function(x) unname(x[-1]))

            ## Set the comparisons to match the subset list
            sample_pop <- lapply(subsets_rand, get.sample.pop, data)

            ## Set the sample pop names
            names(sample_pop) <- unlist(lapply(subsets_rand, get.sample.pop.name, data))

            ## Get the subsets
            subsets <- lapply(data$subsets[unlist(subsets_obs)], function(x) return(x$elements))
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
        sample_pop <- 1:nrow(data)
        pop_names <- rownames(data)
        if(is.null(pop_names)) {
            pop_names <- rownames(data) <- sample_pop
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
        all.checks <- function(one_subset, sample_pop, pop_names) {
            return(((is(one_subset, "numeric") || is(one_subset, "integer") || is(one_subset, "character")) && length(one_subset) > sample_pop) || all(one_subset %in% pop_names))
        }
        if(any(!unlist(lapply(subsets, all.checks, sample_pop, pop_names)))) {
            stop("Subsets must be a vector or a list of vector of integers or numeric values that can not exceed the number of rows in data.", call. = FALSE)
        }

        ## Make the sample pop list
        sample_pop <- replicate(length(subsets), list(sample_pop))
    }

    ## Check the metric
    metrics_list <- get.dispRity.metric.handle(metric, match_call, data = data, ...)$levels
    # warning("DEBUG randtest.dispRity") ; metrics_list <- get.dispRity.metric.handle(metric, match_call, data = data)$levels

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
    lapply_loop <- mapply(make.lapply.loop, subsets, sample_pop, MoreArgs = list(replicates = replicates), SIMPLIFY = FALSE)
    
    ## Calculate all the disparity values
    disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose, ...)
    # warning("DEBUG randtest.dispRity") ; disparity <- lapply(lapply_loop, lapply.wrapper, metrics_list, data, matrix_decomposition, verbose)

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
        ## Get the list of comparisons
        if(is.null(names(sample_pop))) {
            comparisons <- replicate(n = length(results), "the whole space", simplify = FALSE)
        } else {
            comparisons <- names(sample_pop)
        }
        ## Get the call for each test
        make.call <- function(subset_name, comparison, resample) {       
            return(paste0("dispRity.randtest: Subset ", subset_name, " (observed) compared to ", comparison, " (random) ", ifelse(resample, "with", "without"), " resampling."))
        }

        if(is.null(names(results))) {
            results_names <- as.list(1:length(results))
        } else {
            results_names <- as.list(names(results))
        }

        call_summary <- mapply(make.call, results_names, comparisons, MoreArgs = list(resample = resample), SIMPLIFY = FALSE)

        ## For match_call
        results <- mapply(function(results, call_summary) {results$call <- call_summary; return(results)}, results, call_summary, SIMPLIFY = FALSE)

        ## Add names to the results (if any)
        if(!is.null(subsets_names)) {
            ## Input names
            names(results) <- subsets_names
        } else {
            ## Auto naming (if possible)
            if(!is.null(sub_names)) {
                names(results) <- sub_names
            }
        }

        class(results) <- c("dispRity", "randtest")
        
        return(results)
    }
}