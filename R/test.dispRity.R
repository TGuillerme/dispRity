#' @title Testing disparity hypotheses
#'
#' @description Applying statistical tests to dispRity objects
#'
#' @param data A \code{dispRity} object.
#' @param test A test \code{function} to apply to the data.
#' @param comparisons If data contains more than two subsamples, the type of comparisons to apply: either \code{"pairwise"} (default), \code{"referential"}, \code{"sequential"}, \code{"all"} or a list of pairs of subsample names/number to compare (see details).
#' @param rarefaction A \code{numeric} value indicating whether to use a specific rarefaction level (default = \code{NULL}).
#' @param correction Which p-value correction to apply to \code{htest} category test (see \code{\link[stats]{p.adjust}}; default = \code{"none"}).
#' @param concatenate Logical, whether to concatenate bootstrapped disparity values (\code{TRUE}; default) or to apply the test to each bootstrapped value individually (\code{FALSE}).
#' @param conc.quantiles If \code{concatenate = TRUE}, must be a central tendency function and a vector of quantiles (default = \code{c(mean, c(95, 50))}).
#' @param details Whether to output the details of each test (non-formatted; default = \code{FALSE}).
#' @param ... Additional options to pass to the test \code{function}.
#'
#' @details  
#' The \code{comparison} argument can be:
#' \itemize{
#'   \item \code{"pairwise"}: pairwise comparisons of all the subsamples (default).
#'   \item \code{"referential"}: compares the first subsample to all the others.
#'   \item \code{"sequential"}: compares each subsample sequentially (e.g. first against second, second against third, etc.).
#'   \item \code{"all"}: compares all the subsamples simultaneously to the data (i.e. \code{bootstrapped disparity ~ subsamples names}). This argument is used for \code{\link[stats]{lm}} or \code{\link[stats]{glm}} type tests.
#'   \item A list of pairs of number of subsamples to compare. Each element of the list must contain two elements
#'      (e.g. \code{list(c("a","b"), ("b", "a"))} to compare "a" to "b" and then "b" to "a").
#'   \item {If the called test is \code{\link[dispRity]{null.test}}, the comparison argument is ignored.} 
#\code{\link[dispRity]{sequential.test}}
#\code{\link[dispRity]{model.test}}
#' }
#' IMPORTANT: if you are performing multiple comparisons (e.g. when using \code{"pairwise"}, \code{"referential"} or \code{"sequential"}),  don't forget about the Type I error rate inflation. You might want to use a \emph{p-value} correction (see \code{\link[stats]{p.adjust}}).
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity from customised subsamples
#' ## Generating the subsamples
#' groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
#'       dimnames =list(rownames(BeckLee_mat50))), ncol = 1)
#' customised_subsamples <- custom.subsamples(BeckLee_mat50, groups)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#'
#' ## Measuring the subsample overlap
#' test.dispRity(sum_of_variances, bhatt.coeff, "pairwise")
#' 
#' ## Measuring differences from a reference subsample
#' test.dispRity(sum_of_variances, wilcox.test, "referential")
#'
#' ## Running a linear model on the data
#' test.dispRity(sum_of_variances, lm, "all")
#'
#' ## Measuring disparity as a distribution
#' disparity_var <- dispRity(bootstrapped_data, metric = variances)
#' ## Differences between the concatenated bootstrapped values of the subsamples
#' test.dispRity(disparity_var, test = t.test, comparisons = "pairwise",
#'      concatenate = TRUE, correction = "bonferroni")
#' ## Differences between the subsamples bootstrapped
#' test.dispRity(disparity_var, test = t.test, comparisons = "pairwise",
#'      concatenate = FALSE, correction = "bonferroni",
#'      conc.quantiles = c(mean, c(95, 5)))
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{sequential.test}}, \code{\link{null.test}}, \code{\link{bhatt.coeff}}, \code{\link{pair.plot}}.
#'
#' @author Thomas Guillerme

#For testing:
# source("sanitizing.R")
# source("test.dispRity_fun.R")
# source("summary.dispRity_fun.R")
# source("make.metric_fun.R")
# source("dispRity.utilities.R")
# source("dispRity.utilities_fun.R")
# # source("sequential.test.R")
# # source("sequential.test_fun.R")
# data(BeckLee_mat50)
# groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_subsamples <- custom.subsamples(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 10)
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# data <- data_single
# test = t.test
# comparisons = "pairwise"
# rarefaction = NULL
# concatenate = TRUE
# conc.quantiles = c(mean, c(95, 50))
# test.dispRity(data, test = lm, comparisons = "all")
# test.dispRity(data, test = lm, comparisons = "all", concatenate = FALSE)
# data <- test.dispRity(data, test = sequential.test, family = gaussian, concatenate = FALSE)

test.dispRity <- function(data, test, comparisons = "pairwise", rarefaction = NULL, correction = "none", concatenate = TRUE, conc.quantiles = c(mean, c(95, 50)), details = FALSE, ...) { #format: get additional option for input format?

    ## get call
    match_call <- match.call()

    ## DATA
    ## must be class dispRity...
    check.class(data, "dispRity")
    ## ...and have disparity data
    if(is.null(data$call$disparity)) stop("Disparity has not been calculated yet.\nUse the dispRity() function to do so.\n", sep = "")
    ## ...and must have more than one subsamples
    if(length(data$subsamples) == 1) stop(paste(match_call$data, "must have more than one subsample."))

    ## Check if disparity is a value or a distribution
    is_distribution <- ifelse(length(data$disparity[[1]]$elements) != 1, TRUE, FALSE)

    ## Check the bootstraps
    is_bootstrapped <- ifelse(!is.null(data$call$bootstrap), TRUE, FALSE)

    ## Stop if disparity is not a distribution, nor bootstrapped
    if(!is_bootstrapped & !is_distribution) stop(paste(match_call$data, "is neither a distribution nor bootstrapped: impossible to compare single values."))
    
    ## Stop if disparity is not bootstrapped and rarefaction is required
    if(!is_bootstrapped & !is.null(rarefaction)) stop("Impossible to use a rarefaction level for non-bootstrapped data.")

    ## Test
    ## must be a single function
    check.class(test, "function", " must be a single function.")
    check.length(test, 1, " must be a single function.")

    ## Details
    check.class(details, "logical")

    ## Authorised methods
    all_comparisons <- c("referential", "sequential", "pairwise", "all")

    ## Check if the comparisons is not one of the inbuilt comparisons
    if(all(is.na(match(comparisons, all_comparisons)))) {
        
        ## Else must be a list
        check.class(comparisons, "list", paste(" must be either \"", paste(all_comparisons, collapse = "\", \""), "\" or list of one or more pairs of subsamples.", sep = ""))
        
        ## must be pairs
        if(length(unlist(comparisons))%%2 != 0) stop(paste(as.expression(match_call$comparisons), paste(" must be either \"", paste(all_comparisons, collapse = "\", \""), "\" or list of one or more pairs of subsamples.", sep = ""), sep=""))    
        
        ## If character, input must match the subsamples
        if(class(unlist(comparisons)) == "character") {
            if(any(is.na(match(unlist(comparisons), data$subsamples)))) stop(paste(as.expression(match_call$comparisons), ": at least one subsample was not found.", sep=""))
        }

        ## If numeric, input must match de subsamples numbers
        if(class(unlist(comparisons)) == "numeric") {
            if(any(is.na(match(unlist(comparisons), seq(1:length(data$subsamples)))))) stop(paste(as.expression(match_call$comparisons), ": at least one subsample was not found.", sep=""))
        }

        ## Comparison is "custom"
        comp <- "custom"
    
    } else {
        ## Make sure only one inbuilt comparison is given
        check.length(comparisons, 1, paste(" must be either", paste(all_comparisons, collapse = ", "), "."))        
        comp <- comparisons

        ## Set specific comparisons if needed
        if(as.character(match_call$test) == "sequential.test") {
            comp <- "sequential.test"
            comparisons <- "all"
        }

        if(as.character(match_call$test) == "null.test") {
            comp <- "null.test"
            comparisons <- "all"
        }
    }

    ## rarefaction
    if(!is.null(rarefaction)) {
        check.class(rarefaction, c("numeric", "integer"))
        check.length(rarefaction, 1, errorif = FALSE, msg = "Only one rarefaction level can be used.")
        if(is.na(match(rarefaction, data$call$bootstrap[[3]]))) {
            stop("Rarefaction level not found.")
        }
    } else {
        rarefaction <- FALSE
    }

    ## concatenate
    check.class(concatenate, "logical")

    ## concatenate (ignore if data is not bootstrapped)
    if(is_bootstrapped & is_distribution & !concatenate) {

        ## conc.quantiles must be a list
        check.class(conc.quantiles, "list", " must be a list of at least one function and one quantile value (in that order).")
        
        if(length(conc.quantiles) < 2) stop("conc.quantiles must be a list of at least one function and one quantile value (in that order).")
        
        ## first element of conc.quantiles must be a function
        con.cen.tend <- conc.quantiles[[1]]
        check.class(con.cen.tend, "function")
        check.metric(con.cen.tend) -> silent
        
        ## second and more elements must be numeric
        quantiles <- unlist(conc.quantiles[-1])
        if(class(quantiles) != "numeric") stop("Quantiles provided in conc.quantiles must be stated after the function and must be numeric.")
        if(sum(quantiles) != length(quantiles)) {
            conc.quantiles <- CI.converter(quantiles)
        } else {
            conc.quantiles <- quantiles
        }
    }

    ## correction
    check.method(correction, c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), "Correction methods")
    if(length(data$data$bootstrap) > 2 & correction == "none") {
        message("Multiple p-values will be calculated without adjustment!\nThis will inflate Type I error!")
    }

    ## ----------------------
    ##  APPLYING THE TEST
    ## ----------------------

    ## Extracting the data (sends error if data is not bootstrapped)

    extracted_data <- extract.dispRity(data, observed = FALSE, rarefaction = rarefaction, concatenate = concatenate)

    ## Custom, pairwise, sequential and referential
    if(comp == "custom" | comp == "pairwise" | comp == "sequential" | comp == "referential") {
        ## Get the list of comparisons
        comp_subsamples <- set.comparisons.list(comp, extracted_data, comparisons)

        ## Apply the test to the list of pairwise comparisons
        details_out <- test.list.lapply.distributions(comp_subsamples, extracted_data, test, ...)
        # details_out <- test.list.lapply.distributions(comp_subsamples, extracted_data, test) ; warning("DEBUG")

        ## Renaming the detailed results list
        comparisons_list <- save.comparison.list(comp_subsamples, extracted_data)
        names(details_out) <- comparisons_list
    }

    ## ANOVA/GLM type
    if(comp == "all") {

        ## Splitting the data by each bootstrap
        list_of_data <- list()
        for(bootstrap in 1:length(extracted_data[[1]])) {
            list_of_data[[bootstrap]] <- lapply(extracted_data, `[[`, bootstrap)
        }
        list_of_data <- lapply(list_of_data, list.to.table)

        ## running the tests
        try(details_out <- lapply(list_of_data, lapply.lm.type, test, ...), silent = TRUE)
        ## try(details_out <- lapply(list_of_data, lapply.lm.type, test), silent = TRUE) ; warning("DEBUG")
        if(is.null(details_out)) stop(paste("Comparison type \"all\" is not applicable with", match_call$test))
    }

    ## Sequential.test comparisons (one to each other)
    if(comp == "sequential.test") {
        ## Applying the test to the list of extracted data
        details_out <- test(extracted_data, correction, call = data$call, ...)
        ## details_out <- test(extracted_data, correction, call = data$call, family = gaussian)
        details_out <- test(extracted_data, call = data$call, ...)
        ## details_out <- test(extracted_data, call = data$call, family = gaussian)
    }

    ## Null testing
    if(comp == "null.test") {
        ## Applying the test to the data
        details_out <- test(data, ...)
        ## details_out <- test(data, replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "two-sided", scale = FALSE)
    }

    ## ----------------------
    ##  TEST OUTPUT
    ## ----------------------

    ## Formatting the output (if needed)

    if(!details & comp != "all") {
        ## Getting the output class
        out_class <- unique(unlist(lapply(details_out, lapply, class)))

        ## Numeric output
        if(out_class == "numeric") {
            if(concatenate) {
                table_out <- output.numeric.results(details_out, as.expression(match_call$test), comparisons_list)
            } else {
                table_out <- output.numeric.results(details_out, as.expression(match_call$test), comparisons_list, conc.quantiles, con.cen.tend)
            }
            return(table_out)
        }

        ## htest output
        if(out_class == "htest") {
            if(concatenate) {
                table_out <- output.htest.results(details_out, comparisons_list, correction = correction)
            } else {
                table_out <- output.htest.results(details_out, comparisons_list, conc.quantiles, con.cen.tend, correction = correction)
            }

            return(table_out)
        }

        ## no implemented output:
        return(details_out)

    } else {

        ## Dealing with lm class
        if(comp == "all" && unique(lapply(details_out, class))[[1]][[1]] == "lm") {
            ## If concatenate == TRUE
            if(is_distribution == TRUE && is_bootstrapped == TRUE && concatenate == FALSE) {
                # ## Transform results into a list
                # table_out <- output.lm.results(details_out, conc.quantiles, con.cen.tend)
                # return(table_out)
                return(details_out)
            } else {
                ## Results should be a single test 
                if(length(details_out) == 1) {
                    return(details_out[[1]])
                } else {
                    return(details_out)
                }
            }

            return(table_out)
        }

        ## Sequential test results
        if(details == FALSE && comp == "sequential.test") {
            ## Sequential test already formated
            return(details_out)
        }

        ## Null.test results
        if(details == FALSE && comp == "null.test") {
            if(length(data$subsamples) == 1) {
                ## Return a single randtest already formatted.
                return(details_out)
            } else {
                ## Saving the calling parameters
                call <- paste("Monte-Carlo test from ade4::as.randtest with ", match_call$replicates, " replicates and alternative hypothesis set to be ", details_out[[1]]$alter, ".\n", "Null model was defined as: ", match_call$null.distrib, ".\nDisparity was measured as: ", get.from.call(data, "metric", eval = FALSE), ".\n", sep ="")
                ## Creating the results table
                table_obs <- matrix(data = summary(data, round = 5)$observed, nrow = length(data$subsamples), ncol = 1, dimnames = list(c(data$subsamples)))
                table_sta <- matrix(data =  unlist(lapply(details_out, function(X) return(c(X$expvar, X$pvalue)))), nrow = length(data$subsamples), ncol = 4,  dimnames = list(c(data$subsamples)), byrow = TRUE)

                table_out <- cbind(table_obs, table_sta)
                colnames(table_out) <- c("Obs.", "Std.Obs", "Expect", "Var", "p-value")

                cat(call)
                return(table_out)
            }
        }

        ## returning the detailed output
        return(details_out)
    }
}