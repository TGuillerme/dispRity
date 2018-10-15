#' @title Testing disparity hypotheses
#'
#' @description Applying statistical tests to dispRity objects
#'
#' @param data A \code{dispRity} object.
#' @param test A test \code{function} to apply to the data.
#' @param comparisons If data contains more than two subsets, the type of comparisons to apply: either \code{"pairwise"} (default), \code{"referential"}, \code{"sequential"}, \code{"all"} or a list of pairs of subset names/number to compare (see details).
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
#'   \item \code{"pairwise"}: pairwise comparisons of all the subsets (default).
#'   \item \code{"referential"}: compares the first subset to all the others.
#'   \item \code{"sequential"}: compares each subset sequentially (e.g. first against second, second against third, etc.).
#'   \item \code{"all"}: compares all the subsets simultaneously to the data (i.e. \code{bootstrapped disparity ~ subsets names}). This argument is used for \code{\link[stats]{lm}} or \code{\link[stats]{glm}} type tests.
#'   \item A list of pairs of number of subsets to compare. Each element of the list must contain two elements
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
#' data(BeckLee_tree)
#'
#' ## Calculating the disparity from customised subsets
#' ## Generating the subsets
#' groups <- crown.stem(BeckLee_tree, inc.nodes = FALSE)
#' customised_subsets <- custom.subsets(BeckLee_mat50, groups)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#'
#' ## Measuring the subset overlap
#' test.dispRity(sum_of_variances, bhatt.coeff, "pairwise")
#' 
#' ## Measuring differences from a reference subset
#' test.dispRity(sum_of_variances, wilcox.test, "referential")
#'
#' ## Measuring disparity as a distribution
#' disparity_var <- dispRity(bootstrapped_data, metric = variances)
#' ## Differences between the concatenated bootstrapped values of the subsets
#' test.dispRity(disparity_var, test = t.test, comparisons = "pairwise",
#'      concatenate = TRUE, correction = "bonferroni")
#' ## Differences between the subsets bootstrapped
#' test.dispRity(disparity_var, test = t.test, comparisons = "pairwise",
#'      concatenate = FALSE, correction = "bonferroni",
#'      conc.quantiles = c(mean, c(95, 5)))
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{null.test}}, \code{\link{bhatt.coeff}}, \code{\link{pair.plot}}, \code{\link{adonis.dispRity}}.
# \code{\link{sequential.test}}
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
# customised_subsets <- custom.subsets(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 10)
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# data <- data_single
# test = t.test
# comparisons = "pairwise"
# rarefaction = NULL
# concatenate = TRUE
# conc.quantiles = c(mean, c(95, 50))
# correction = "none"
# details = TRUE
# match_call <- list(data = "data", test = "t.test")
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
    if(is.null(data$call$disparity)) {
        stop.call("", "Disparity has not been calculated yet.\nUse the dispRity() function to do so.\n")
    }
    ## ...and must have more than one subsets
    if(length(data$subsets) == 1){
        stop.call(match_call$data, " must have more than one subset.")
    }

    ## Check if disparity is a value or a distribution
    is_distribution <- ifelse(length(data$disparity[[1]]$elements) != 1, TRUE, FALSE)

    ## Check the bootstraps
    is_bootstrapped <- ifelse(!is.null(data$call$bootstrap), TRUE, FALSE)

    ## Stop if disparity is not a distribution, nor bootstrapped
    if(!is_bootstrapped & !is_distribution){
        stop.call(match_call$data, " is neither a distribution nor bootstrapped: impossible to compare single values.")
    }
    
    ## Stop if disparity is not bootstrapped and rarefaction is required
    if(!is_bootstrapped & !is.null(rarefaction)) {
        stop.call("", "Impossible to use a rarefaction level for non-bootstrapped data.")
    }

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
        check.class(comparisons, "list", paste(" must be either \"", paste(all_comparisons, collapse = "\", \""), "\" or list of one or more pairs of subsets.", sep = ""))
        
        ## must be pairs
        if(length(unlist(comparisons))%%2 != 0) {
            stop.call(match_call$comparisons, paste0(" must be either \"", paste(all_comparisons, collapse = "\", \""), "\" or list of one or more pairs of subsets."))
        }
        
        ## If character, input must match the subsets
        if(class(unlist(comparisons)) == "character") {
            if(any(is.na(match(unlist(comparisons), data$subsets)))){
                stop.call(match_call$comparions, ": at least one subset was not found.")
            }
        }

        ## If numeric, input must match de subsets numbers
        if(class(unlist(comparisons)) == "numeric") {
            if(any(is.na(match(unlist(comparisons), seq(1:length(data$subsets)))))){
                stop.call(match_call$comparisons, ": at least one subset was not found.")
            }
        }

        ## Comparison is "custom"
        comp <- "custom"
    
    } else {
        ## Make sure only one inbuilt comparison is given
        check.length(comparisons, 1, paste(" must be either", paste(all_comparisons, collapse = ", "), "."))
        comp <- comparisons

        ## Set specific comparisons if needed
        # if(as.character(match_call$test) == "sequential.test") {
        #     comp <- "sequential.test"
        #     comparisons <- "all"
        # }

        if(any(as.character(match_call$test) == "null.test")) {
            comp <- "null.test"
            comparisons <- "all"
        }
    }

    ## rarefaction
    if(!is.null(rarefaction)) {
        check.class(rarefaction, c("numeric", "integer"))
        check.length(rarefaction, 1, errorif = FALSE, msg = "Only one rarefaction level can be used.")
        if(is.na(match(rarefaction, data$call$bootstrap[[3]]))) {
            stop.call("", "Rarefaction level not found.")
        }
    } else {
        rarefaction <- FALSE
    }

    ## concatenate
    check.class(concatenate, "logical")
    if(!is_distribution && !concatenate) {
        stop.call("", "Disparity is not calculated as a distribution, data cannot be concatenated (set concatenate = FALSE).")
    }

    ## concatenate (ignore if data is not bootstrapped)
    if(is_bootstrapped & is_distribution & !concatenate) {

        ## conc.quantiles must be a list
        check.class(conc.quantiles, "list", " must be a list of at least one function and one quantile value (in that order).")
        
        if(length(conc.quantiles) < 2) {
            stop.call("", "conc.quantiles must be a list of at least one function and one quantile value (in that order).")
        }
        
        ## first element of conc.quantiles must be a function
        con.cen.tend <- conc.quantiles[[1]]
        check.class(con.cen.tend, "function")
        check.metric(con.cen.tend) -> silent
        
        ## second and more elements must be numeric
        quantiles <- unlist(conc.quantiles[-1])
        if(class(quantiles) != "numeric") {
            stop.call("", "Quantiles provided in conc.quantiles must be stated after the function and must be numeric.")
        }
        if(sum(quantiles) != length(quantiles)) {
            conc.quantiles <- CI.converter(quantiles)
        } else {
            conc.quantiles <- quantiles
        }
    }

    ## correction
    check.method(correction, c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"), "Correction methods")
    if((is_bootstrapped || is_distribution)  && correction == "none" && length(data$subsets) > 2 && match_call$test != "adonis.dispRity") {
        warning("Multiple p-values will be calculated without adjustment!\nThis can inflate Type I error!")
    }

    ## ----------------------
    ##  APPLYING THE TEST
    ## ----------------------

    if(length(grep("adonis", as.character(match_call$test))) > 0) {

        warning("adonis.dispRity test will be applied to the data matrix, not to the calculated disparity.\nSee ?adonis.dispRity for more details.")

        return(adonis.dispRity(data, ...))
        #adonis.dispRity(data)
    }


    ## Extracting the data (sends error if data is not bootstrapped)
    if(is_distribution && !is_bootstrapped) {
        extracted_data <- extract.dispRity(data, observed = TRUE, rarefaction = rarefaction, concatenate = concatenate)
        ## Transform into the listed structure
        extracted_data <- lapply(extracted_data, list)
    } else {
        extracted_data <- extract.dispRity(data, observed = FALSE, rarefaction = rarefaction, concatenate = concatenate)
    }

    ## Custom, pairwise, sequential and referential
    if(comp == "custom" | comp == "pairwise" | comp == "sequential" | comp == "referential") {
        ## Get the list of comparisons
        comp_subsets <- set.comparisons.list(comp, extracted_data, comparisons)

        ## Apply the test to the list of pairwise comparisons
        details_out <- test.list.lapply.distributions(comp_subsets, extracted_data, test, ...)
        # details_out <- test.list.lapply.distributions(comp_subsets, extracted_data, test) ; warning("DEBUG")

        ## Renaming the detailed results list
        comparisons_list <- save.comparison.list(comp_subsets, extracted_data)
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
        if(is.null(details_out)) {
            stop.call(match_call$test, ".", "Comparison type \"all\" is not applicable with ")
        }
        if(concatenate == FALSE) warning("Comparison type \"all\" is based on concatenated data.\nlm or aov type tests will have an inflated type I error!")
    }

    ## Sequential.test comparisons (one to each other)
    # if(comp == "sequential.test") {
    #     ## Applying the test to the list of extracted data
    #     details_out <- test(extracted_data, correction, call = data$call, ...)
    #     ## details_out <- test(extracted_data, correction, call = data$call, family = gaussian)
    #     details_out <- test(extracted_data, call = data$call, ...)
    #     ## details_out <- test(extracted_data, call = data$call, family = gaussian)
    # }

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

            ## Editing table names
            long_title <- unlist(lapply(table_out, function(X) ifelse(length(grep(" c\\(\"", colnames(X)[1])) > 0, TRUE, FALSE)))
            if(any(long_title)) {
                for(i in 1:length(table_out)) {
                    if(long_title[i]) {
                        colnames(table_out[[i]])[1] <- gsub("\\\".*", "", gsub("c\\(\"", "", colnames(table_out[[i]])[1]))
                    }
                }
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
        # if(details == FALSE && comp == "sequential.test") {
        #     ## Sequential test already formated
        #     return(details_out)
        # }

        ## returning the detailed output
        return(details_out)
    }
}