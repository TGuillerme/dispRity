#' @name sequential.test
#'
#' @title Sequential linear regressions
#'
#' @usage sequential.test(series, results = "coefficients", family, ...)
#'
#' @description Performs a sequential \code{\link[stats]{glm}} on the series by correcting for time autocorrelation. 
#'
#' @param series time series of which to estimate the slopes sequentially.
#' @param results which results from the \code{\link[stats]{glm}} to display (default = \code{"coefficients"}).
#' @param family the family of the \code{\link[stats]{glm}}.
#' @param correction optional, which p-value correction to apply (see \code{\link[stats]{p.adjust}}). If missing, no correction is applied.
#' @param ... optional arguments to be passed to the \code{\link[stats]{glm}}.
# ' @param add whether to add the results of the sequential test to the current plot (default = \code{FALSE}).
# ' @param lines.args a list of arguments to pass to \code{\link[graphics]{lines}} (default = \code{NULL}).
# ' @param token.args a list of arguments to pass to \code{\link[graphics]{text}} for plotting tokens (see details; default = \code{NULL}).
#'
#' @details
#' This test allows to correct for time autocorrelation by estimating the intercept of the \code{\link[stats]{glm}} using a predicted intercept using the preceding \code{\link[stats]{glm}}.
# ' the \code{token.args} argument intakes a list of arguments to be passed to \code{\link[graphics]{text}} for plotting the significance tokens. The plotted tokens are the standard p-value significance tokens from R:
# ' \code{0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1}
# ' Additionally, the \code{float} argument can be used for setting the height of the tokens compared to the slopes. For example one can use \code{sequential.test(..., token.args = list(float = 0.3, col = "blue", cex = 0.5))} for plotting blue tokens 50% smaller than normal and 30 higher than the slope.
#' 
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
#'      dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#' ## Extracting the row series
#' series <- extract.dispRity(sum_of_variances, observed = FALSE)
#'
#' ## The sequential test
#' sequential.test(series, family = gaussian)
#'
#' @seealso \code{\link{test.dispRity}}, \code{\link{bhatt.coeff}}, \code{\link{null.test}}.
#'
#'
#' @author Thomas Guillerme
#' @export

#Testing
# source("sanitizing.R")
# source("sequential.test_fun.R")
# source("test.dispRity_fun.R")
# data(BeckLee_mat50)
# factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_series <- cust.series(BeckLee_mat50, factors)
# bootstrapped_data <- boot.matrix(customised_series, bootstraps = 3)
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# series_single <- extract.dispRity(data_single, observed = FALSE)
# series_multi <- extract.dispRity(data_multi, observed = FALSE, concatenate = FALSE)
# results = "coefficients"
# family = gaussian
# sequential.test(series_multi, family = gaussian)

sequential.test <- function(series, results = "coefficients", family, correction, ...){#, add = FALSE, lines.args = NULL, token.args = NULL) {

    #SANITIZING
    
    #results must be at least coefficients!
    if(is.na(match("coefficients", results))) {
        results <- c(results, "coefficients")
    }

    #Family
    if(missing(family)) {
        stop("glm family type argument is necessary!")
    }
    # if(family(link="identity")[[1]] == "gaussian") {
    #     warning("Model family is set to gaussian, should it not be binomial?")
    # }

    #correction
    if(!missing(correction)) {
        check.class(correction, 'character')
        p.adjust_list<- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
        if(all(is.na(match(correction, p.adjust_list)))) {
            stop("correction type must be one of the p.adjust function options.")
        }
    }

    #Testing whether the results are distributions (BSed) or not.
    is.distribution <- ifelse(unique(unlist(lapply(series, class))) == "numeric", FALSE, TRUE)

    #If is not a distribution, reformat the list to be a list of MethodsListSelect
    if(is.distribution == FALSE) {
        series <- lapply(series, list)
    } 

    #APPLYING THE SEQUENTIAL TEST

    #Setting the sequence
    seq_series <- set.comparisons.list("sequential", series, 1)

    #Applying the first test to get the intercept origin
    first_model <- lapply(set.pair.series(series[seq_series[[1]]]), create.model, intercept = NULL, family, ...)
    #first_model <- lapply(set.pair.series(series[seq_series[[1]]]), create.model, intercept = NULL, family) ; warning("DEBUG")
    
    #Calculate the intercepts for each first models
    intercept_predict <- list()
    intercept_predict[[1]] <- lapply(first_model, set.intercept0)

    #Storing the first model
    models <- list()
    models[[1]] <- first_model

    #Loop through the other models
    for(model in 2:(length(seq_series))) {
        #Calculate the new intercept from the previous model
        intercept_predict[[model]] <- mapply(set.intercept.next, models[[model-1]], intercept_predict[[model-1]], SIMPLIFY = FALSE)

        #Create the new model 
        models[[model]] <- lapply(set.pair.series(series[seq_series[[model]]], intercept = intercept_predict[[model-1]]), create.model, intercept = "in.data", family, ...)
        #models[[model]] <- lapply(set.pair.series(series[seq_series[[model]]], intercept = intercept_predict[[model-1]]), create.model, intercept = "in.data", family) ; warning("DEBUG")
    }

    #OUTPUT

    output_raw <- list(models)#, intercept_predict)
    class(output_raw) <- c("dispRity", "seq.test")
    return(output_raw)
}