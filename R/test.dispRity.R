#' @title Disparity statistics 
#'
#' @description Applying statistical tests to dispRity objects
#'
#' @param data A \code{dispRity} object.
#' @param test A test \code{function} to apply to the data.
#' @param comparisons If data contains more than two series, the type of comparisons to apply: either \code{"pairwise"} (default), \code{"referential"}, \code{"sequential"}, \code{"all"} or a list of pairs of series names/number to compare (see details).
#' @param correction which p-value correction to apply to \code{htest} category test (see \code{\link[stats]{p.adjust}}). If missing, no correction is applied.
#' @param concatenate Logical, whether to concatenate eventual bootstrapped disparity values (\code{TRUE}; default) or to apply the test to each bootstrapped values individually (\code{FALSE}).
#' @param conc.quantiles If \code{concatenate = TRUE}, must be a central tendency function and a vector of quantiles.
#' @param details Whether to output the details of each test (non-formatted; default = \code{FALSE}).
#' @param ... Additional options to pass to the test \code{function}.
#'
#' @details  
#' The \code{comparison} argument can be:
#' \itemize{
#'   \item \code{"pairwise"}: pairwise comparisons of all the series (default).
#'   \item \code{"referential"}: compares the first series to all the others.
#'   \item \code{"sequential"}: compares each series sequentially (e.g. first against second, second against third, etc.).
#'   \item \code{"all"}: compares all the series simultaneously to the data (i.e. \code{bootstrapped disparity ~ series names}). This argument is used for \code{\link[stats]{aov}} or \code{\link[stats]{glm}} type tests.
#'   \item A list of pairs of number of series to compare. Each element of the the list must contain two elements
#'      (e.g. \code{list(c("a","b"), ("b", "a"))} to compare "a" to "b" and then "b" to "a").
#'   \item \bold{If the called test is \code{\link[dispRity]{sequential.test}} or \code{\link[dispRity]{null.test}}, the comparison argument is ignored.}
#' }
#' IMPORTANT: if you are performing multiple comparisons (e.g. when using \code{"pairwise"}, \code{"referential"} or \code{"sequential"}),  don't forget about the Type I error rate inflation. You might want to use a \emph{p-value} correction (see \code{\link[stats]{p.adjust}}).
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
#'       dimnames =list(rownames(BeckLee_mat50))), ncol = 1)
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#'
#' ## Measuring the series overlap
#' test.dispRity(sum_of_variances, bhatt.coeff, "pairwise", details)
#  
# bootstrapped_data <- boot.matrix(customised_series, bootstraps = 10, boot.type = "full")
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# data <- data_single
# test.dispRity(data, bhatt.coeff, "pairwise", details = TRUE)
# 
# 
#' 
#' ## Measuring differences from a reference_series
#' test.dispRity(sum_of_variances, wilcox.test, "referential")
#'
#' ## Testing the effect of the factors
#' test.dispRity(sum_of_variances, aov, "all")
#' ## warning: this violates some aov assumptions!
#'
#' @seealso \code{\link{dispRity}}, \code{\link{sequential.test}}, \code{\link{null.test}}, \code{\link{bhatt.coeff}}.
#'
#' @author Thomas Guillerme

#For testing:
#source("sanitizing.R")
#source("test.dispRity_fun.R")
#source("summary.dispRity_fun.R")
#source("make.metric_fun.R")


test.dispRity<-function(data, test, comparisons="pairwise", correction, concatenate=TRUE, conc.quantiles=c(mean, c(95, 50)), details=FALSE, ...) { #format: get additional option for input format?

    #get call
    match_call<-match.call()

    #----------------------
    # SANITIZING
    #----------------------
    #DATA
    #must be class dispRity
    check.class(data, "dispRity")
    #Check if it's a bootstrapped dispRity object
    if(class(data) == "dispRity" & length(data) == 4) stop(paste(data$call), "\nUse the dispRity function to calculate disparity.", sep="")
    #must have 5 elements
    check.length(data, 5, " must be a 'dispRity' object.")
    #must have one element called dispRity
    if(is.na(match("disparity", names(data)))) stop("Data must be a dispRity object.")
    OBSresults <- data$disparity$observed
    #is the data bootstrapped? 
    if(!is.na(match("bootstraps", names(data$data)))) {
        #must have more than one bootstrap!
        if(length(data$data$bootstraps[[1]][[1]]) > 1) {
            is.bootstrapped <- TRUE
            BSresults <- data$disparity$bootstrapped
        } else {
            is.bootstrapped <- FALSE
        }
    } else {
        is.bootstrapped <- FALSE
    }
    #check if is.distribution
    is.distribution <- ifelse(length(data$disparity$observed[[1]][[1]][[1]]) == 1, FALSE, TRUE)
    
    #Test
    #must be a single function
    check.class(test, "function", " must be a single function.")
    check.length(test, 1, " must be a single function.")

    #Details
    check.class(details, "logical")

    #Comparisons
    test_data_length <- extract.dispRity(data)
    #Stop if only one series
    if(length(test_data_length) == 1 && as.character(match_call$test) != "null.test") stop(paste(as.expression(match_call$data), " must have at least two series.", sep=""))

    # #Ignore if length data = 2
    # if(length(test_data_length) == 2) comparisons <- NULL

    #Else, check comparisons
    all_comparisons <- c("referential", "sequential", "pairwise", "all")

    #Check if the comparisons is not one of the inbuilt comparisons
    if(all(is.na(match(comparisons, all_comparisons)))) {
        #must be a list
        check.class(comparisons, "list", " must be a list of one or more pairs of series.")
        #must be pairs
        if(length(unlist(comparisons))%%2 != 0) stop(paste(as.expression(match_call$comparisons), " must be a list of one or more pairs of series.", sep=""))    
        #If character, input must match the series
        if(class(unlist(comparisons)) == "character") {
            if(any(is.na(match(unlist(comparisons), data$series)))) stop(paste(as.expression(match_call$comparisons), ": at least one series was not found.", sep=""))
        }
        #If numeric, input must match de series numbers
        if(class(unlist(comparisons)) == "numeric") {
            if(any(is.na(match(unlist(comparisons), seq(1:length(data$series)))))) stop(paste(as.expression(match_call$comparisons), ": at least one series was not found.", sep=""))
        }
        #Comparison is "custom"
        comp <- "custom"
    } else {
        #Make sure only one inbuilt comparison is given
        check.length(comparisons, 1, " must be either 'referential', 'sequential', 'pairwise', 'all' or a vector of series names/numbers.")
        comp <- comparisons

        #Set specific comparisons if needed
        if(as.character(match_call$test) == "sequential.test") {
            comp <- "sequential.test"
            comparisons <- "all"
        }
        if(as.character(match_call$test) == "null.test") {
            comp <- "null.test"
            comparisons <- "all"
        }
    }

    # #concatenate (ignore if data is not bootstrapped)
    if(is.bootstrapped == TRUE && is.distribution == TRUE) { #TG: multiple tests only works if disparity scores are distributions and are bootstrapped!
        #concatenate must be logical
        check.class(concatenate, "logical")
        #conc.quantiles must be a list
        check.class(conc.quantiles, "list", " must be a list of at least one function and one quantile value (in that order).")
        if(length(conc.quantiles < 2)) stop("Conc.quantiles must be a list of at least one function and one quantile value (in that order).")
        #first element of conc.quantiles must be a function
        con.cen.tend <- conc.quantiles[[1]]
        check.class(con.cen.tend, "function")
        check.metric(con.cen.tend) -> silent
        #second and more elements must be numeric
        quantiles <- unlist(conc.quantiles[-1])
        if(class(quantiles) != "numeric") stop("Quantiles provided in conc.quantiles must be stated after the function and must be numeric.")
        if(sum(quantiles) != length(quantiles)) {
            conc.quantiles <- CI.converter(quantiles)
        } else {
            conc.quantiles <- quantiles
        }
    } else {
        concatenate <- TRUE
    }

    #correction
    if(!missing(correction)) {
        check.class(correction, 'character')
        p.adjust_list<- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
        if(all(is.na(match(correction, p.adjust_list)))) {
            stop("correction type must be one of the p.adjust function options.")
        }
        if(length(data$data$bootstrap) > 2 & correction == "none") {
            message("Multiple p-values will be calculated without adjustment!\nThis will inflate the probability of having significant results.")
        }
    }

    #----------------------
    # APPLYING THE TEST
    #----------------------

    #Extracting the data (sends error if data is not bootstrapped)
    if(comp != "null.test") {
        extracted_data <- extract.dispRity(data, observed = FALSE, concatenate = concatenate, keep.structure = TRUE)
    }

    #Custom, pairwise and sequential
    if(comp == "custom" | comp == "pairwise" | comp == "sequential" | comp == "referential") {
        #Get the list of comparisons
        comp_series <- set.comparisons.list(comp, extracted_data, comparisons) 

        #Apply the test to the list of pairwise comparisons
        details_out <- test.list.lapply.distributions(comp_series, extracted_data, test, ...)
        #details_out <- test.list.lapply.distributions(comp_series, extracted_data, test) ; warning("DEBUG")

        #Saving the list of comparisons
        comparisons_list <- save.comparison.list(comp, comp_series, extract_data)

        #Renaming the detailed results list
        names(details_out) <- comparisons_list
    }

    #ANOVA/GLM type
    if(comp == "all") {
        #Transform the extracted data into a table
        data <- list.to.table(extracted_data)

        #Renaming the colnames
        colnames(data) <- c("data", "series")

        #running the test
        details_out <- test(data ~ series, data = data, ...)
        #details_out <- test(data ~ series, data = data) ; warning("DEBUG")
    }

    #Sequential.test comparisons (one to each other)
    if(comp == "sequential.test") {
        #Applying the test to the list of extracted data
        if(!missing(correction)) {
            details_out <- test(extracted_data, correction, ...)
            #details_out <- test(extracted_data, correction, family = gaussian)
        } else {
            details_out <- test(extracted_data, ...)
            #details_out <- test(extracted_data, family = gaussian)
        }
    }

    #Null testing
    if(comp == "null.test") {
        #Applying the test to the data
        details_out <- test(data, ...)
        #details_out <- test(data, replicates = 10, null.distrib = rnorm, null.args = NULL, alter = "two-sided", scale = FALSE)
    }

    #----------------------
    # TEST OUTPUT
    #----------------------

    #Formatting the output (if needed)
    options(warn=-1)

    if(details == FALSE & comparisons != "all") {
        #Getting the output class
        out_class <- unique(unlist(lapply(details_out, lapply, class)))

        #Numeric output
        if(out_class == "numeric") {
            if(concatenate == TRUE) {
                table_out <- output.numeric.results(details_out, match_call, comparisons_list)
            } else {
                table_out <- output.numeric.results(details_out, match_call, comparisons_list, conc.quantiles, con.cen.tend)
            }
            return(table_out)
        }

        #htest output
        if(any(out_class == "htest")) {

            #What's in the test?
            test_elements <- unique(unlist(lapply(details_out, names)))
            #Only select the numeric or integer elements
            test_elements <- test_elements[grep("numeric|integer", unlist(lapply(as.list(details_out[[1]]), class)))]
            #Remove null.value and conf.int (if present)
            remove <- match(c("null.value", "conf.int"), test_elements)
            if(any(is.na(remove))) {
                remove <- remove[-which(is.na(remove))]
            }
            if(length(remove) > 0) {
                test_elements <- test_elements[-remove]
            }
            #Getting the test elements of interest
            table_out <- lapply(details_out, htest.to.vector, print=as.list(test_elements))
            #Transforming list to table
            table_out <- do.call(rbind.data.frame, table_out)
            #Getting col names
            col_names <- unlist(lapply(as.list(test_elements), get.name, htest=details_out[[1]]))
            colnames(table_out) <- col_names
            #Getting row names (the comparisons)
            row.names(table_out) <- comparisons_list


            #Applying the correction
            if(!missing(correction)) {
                table_out$p.value <- p.adjust(table_out$p.value, method = correction)
            }

            return(table_out)

        }

        #no implemented output:
        return(details_out)

    } else {

        #Sequential test results
        if(details == FALSE && comp == "sequential.test") {
            #Sequential test already formated
            return(details_out)
        }

        #Null.test results
        if(details == FALSE && comp == "null.test") {
            if(length(data$series) == 1) {
                #Return a single randtest already formatted.
                return(details_out)
            } else {
                #Saving the calling parameters
                call <- paste("Monte-Carlo test from ade4::as.randtest with ", match_call$replicates, " replicates and alternative hypothesis set to be ", details_out[[1]]$alter, ".\n", "Null model was defined as: ", match_call$null.distrib, ".\nDisparity was measured as: ", get.from.call(data, "metric", eval = FALSE), ".\n", sep ="")
                #Creating the results table
                table_obs <- matrix(data = summary(data, round = 5)$observed, nrow = length(data$series), ncol = 1, dimnames = list(c(data$series)))
                table_sta <- matrix(data =  unlist(lapply(details_out, function(X) return(c(X$expvar, X$pvalue)))), nrow = length(data$series), ncol = 4,  dimnames = list(c(data$series)), byrow = TRUE)

                table_out <- cbind(table_obs, table_sta)
                colnames(table_out) <- c("Obs.", "Std.Obs", "Expect", "Var", "p-value")

                cat(call)
                return(table_out)

            }
        }

        #returning the detailed output
        return(details_out)
    }
    options(warn=0)
}