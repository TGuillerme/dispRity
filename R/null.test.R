#' @name null.test
#'
#' @title Testing a null hypothesis on multidimensional data.
#'
#' @description Testing the difference between the observed disparity and disparity under a null model.
#'
#' @param data a \code{dispRity} object.
#' @param replicates the number of replicates for the test (default = \code{100}).
#' @param null.distrib one or more distribution functions to generate the null model to be passed to \code{\link{space.maker}}.
#' @param null.args any additional distributions arguments to be passed to \code{\link{space.maker}} (see \code{arguments} within; \code{default = NULL}).
#' @param null.cor an additional correlation matrix to be passed to \code{\link{space.maker}} (see \code{cor.matrix} within; \code{default = NULL}).
#' @param alter the type of alternative hypothesis (H1) as used in \code{\link[ade4]{randtest}} (\code{default = "two-sided"}).
#' @param scale whether to scale the simulated and the observed data.
#' @param ... optional arguments to be passed to \code{\link[ade4]{as.randtest}}.
#'
#' @details
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' ## Calculating the disparity as the ellipsoid volume
#' obs_disparity <- dispRity(BeckLee_mat50, metric = ellipse.volume)
#' ## Testing against normal distribution
#' results <- null.test(obs_disparity, replicates = 100, null.distrib = rnorm)
#' results ; plot(results)
#' 
#' \dontrun{
#' ## Running the test on multiple series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12),
#'      rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 100)
#' ## Calculating variances of each dimension
#' sum_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#' ## Testing against normal distribution
#' results <- null.test(sum_variances, replicates = 100, null.distrib = rnorm)
#' results ; plot(results)
#' }
#' 
#' @seealso \code{\link{space.maker}}, \code{\link{test.dispRity}}, \code{\link{sequential.test}}.
#'
#' @references
#' Diaz, S., Kattge, J., Cornelissen, J.H., Wright, I.J., Lavorel, S., Dray, S., Reu, B., Kleyer, M., Wirth, C., Prentice, I.C. and Garnier, E., \bold{2016}. The global spectrum of plant form and function. Nature, 529(7585), pp.167-171.
#'
#' @author Thomas Guillerme
#' @export

#For testing:
# source("sanitizing.R")
# source("null.test_fun.R")
# data(BeckLee_mat50)
# factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)), dimnames =list(rownames(BeckLee_mat50))), ncol = 1)
# customised_series <- cust.series(BeckLee_mat50, factors)
# bootstrapped_data <- boot.matrix(customised_series, bootstraps = 10)
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# single <- dispRity(boot.matrix(BeckLee_mat50, bootstraps = 10), metric = c(sum, variances))
# data <- data_single

# data <- null.test(data_single, replicates = 100, null.distrib = rnorm, null.args = NULL, null.cor = NULL, alter = "two-sided", scale = FALSE)

# summary(data)
# plot(data)

# replicates = 100
# null.distrib = rnorm
# alter = "two-sided"
# null.args = NULL
# null.cor = NULL
# scale = FALSE

null.test <- function(data, replicates = 100, null.distrib, null.args = NULL, null.cor = NULL, alter = "two-sided", scale = FALSE, ...) {
    
    match_call <- match.call()

    #Sanitizing
    check.class(data, "dispRity")
    check.length(data, 5, " must be a dispRity object with calculated observed disparity.")

    #Is distribution?
    is.distribution <- ifelse(length(data$disparity$observed[[1]][[1]][[1]]) == 1, FALSE, TRUE)
    if(is.distribution != FALSE) {
        stop(paste("null.test cannot intake disparity distributions yet. Try averaging disparity to a level 1 using:\n",
            paste("  ", match_call$data, " <- dispRity(", match_call$data, ", metric = median)\n", sep = "")))
    }

    #replicates
    check.class(replicates, "numeric")
    check.length(replicates, 1, " must be a single numeric value.")

    #alter
    alternative_hypothesis <- c("greater", "less", "two-sided")
    if(all(is.na(match(alter, alternative_hypothesis)))) stop("Alternative hypothesis must be one of the following: ", paste(alternative_hypothesis, collapse=", "), sep="")

    #scaling
    check.class(scale, "logical")

    #NULL TESTING

    #Generating the null models
    if(length(data$series) != 1) {
        #Subdivide the data per series
        sub_data <- lapply(as.list(data$series), function(X) get.dispRity(data, what = X))
        #Apply the data to all series
        null_models_results <- lapply(sub_data, make.null.model, replicates, null.distrib, null.args, null.cor, scale)
    } else {
        #Apply the data to one series
        null_models_results <- make.null.model(data, replicates, null.distrib, null.args, null.cor, scale)
    }

    #testing the null hypothesis
    if(class(null_models_results) != "list") {
        #Apply the randtest to one series
        test_out  <- ade4::as.randtest(obs = summary(data, round = 10)[,3], sim = null_models_results, alter = alter, ...)
        #test_out  <- ade4::as.randtest(obs = summary(data, round = 10)[,3], sim = null_models_results, alter = alter)
        #Store it as a list of one element (to be consistent for S3 methods)
        test_out <- list(test_out)
    } else {
        #Extracting the observed data for each series
        summary_observed <- as.list(summary(data, round = 10)[,3])
        test_out <- mapply(ade4::as.randtest, null_models_results, summary_observed, MoreArgs = list(alter = alter, ...), SIMPLIFY = FALSE)
        #test_out <- mapply(ade4::as.randtest, null_models_results, summary_observed, MoreArgs = list(alter = alter), SIMPLIFY = FALSE) ; warning("DEBUG")
        #Attributing the series names
        names(test_out) <- data$series
    }

    class(test_out) <- c("dispRity", "randtest")

    return(test_out)
}