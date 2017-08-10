#' @name null.test
#'
#' @title Testing a null hypothesis on multidimensional data.
#'
#' @description Testing the difference between the observed disparity and disparity under a null model.
#'
#' @param data a \code{dispRity} object.
#' @param replicates the number of replicates for the test (default = \code{100}).
#' @param null.distrib one or more distribution functions to generate the null model to be passed to \code{\link{space.maker}}.
#' @param null.args any additional distribution arguments to be passed to \code{\link{space.maker}} (see \code{arguments} within; \code{default = NULL}).
#' @param null.cor an additional correlation matrix to be passed to \code{\link{space.maker}} (see \code{cor.matrix} within; \code{default = NULL}).
#' @param alter the type of alternative hypothesis (H1) as used in \code{\link[ade4]{randtest}} (\code{default = "two-sided"}).
#' @param scale whether to scale the simulated and the observed data.
#' @param ... optional arguments to be passed to \code{\link[ade4]{as.randtest}}.
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' ## Calculating the disparity as the ellipsoid volume
#' obs_disparity <- dispRity(BeckLee_mat50, metric = ellipse.volume)
#' ## Testing against normal distribution
#' null.test(obs_disparity, replicates = 100, null.distrib = rnorm)
# results ; plot(results)
#' 
#' \dontrun{
#' ## Running the test on multiple subsamples
#' ## Generating the subsamples
#' groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12),
#'      rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
#' customised_subsamples <- custom.subsamples(BeckLee_mat50, groups)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 100)
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
# groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_subsamples <- custom.subsamples(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 10)
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# single <- dispRity(boot.matrix(BeckLee_mat50, bootstraps = 10), metric = c(sum, variances))
# data <- data_multi

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

    ## Sanitizing
    check.class(data, "dispRity")
    if(is.null(data$call$disparity)) stop("Disparity has not been calculated yet.\nUse the dispRity() function to do so.\n", sep = "")

    ## is_distribution?
    is_distribution <- ifelse(length(data$disparity[[1]]$elements) != 1, TRUE, FALSE)
    if(is_distribution) {
        stop(paste("null.test cannot take disparity distributions yet. Try averaging disparity to a dimension-level 1 using:\n",
            paste("  ", match_call$data, " <- dispRity(", match_call$data, ", metric = median)\n", sep = "")))
    }

    ## replicates
    check.class(replicates, "numeric")
    check.length(replicates, 1, " must be a single numeric value.")

    ## alter
    alternative_hypothesis <- c("greater", "less", "two-sided")
    check.method(alter, alternative_hypothesis, "Alternative hypothesis")

    ## scaling
    check.class(scale, "logical")

    ## NULL TESTING

    ## Generating the null models
    if(length(data$subsamples) != 1) {
        ## Subdivide the data per subsamples
        sub_data <- lapply(as.list(seq(1:length(data$subsamples))), function(X) get.subsamples(data, X))
        ## Apply the data to all subsamples
        null_models_results <- lapply(sub_data, make.null.model, replicates, null.distrib, null.args, null.cor, scale)
    } else {
        ## Apply the data to one subsamples
        null_models_results <- make.null.model(data, replicates, null.distrib, null.args, null.cor, scale)
    }

    ## testing the null hypothesis
    if(class(null_models_results) != "list") {
        ## Apply the randtest to one subsamples
        test_out  <- ade4::as.randtest(obs = summary(data, round = 10)[, 3], sim = null_models_results, alter = alter, ...)
        # test_out  <- ade4::as.randtest(obs = summary(data, round = 10)[,3], sim = null_models_results, alter = alter)
        ## Store it as a list of one element (to be consistent for S3 methods)
        test_out <- list(test_out)
    } else {
        ## Extracting the observed data for each subsamples
        summary_observed <- as.list(summary(data, round = 10)[, 3])
        test_out <- mapply(ade4::as.randtest, null_models_results, summary_observed, MoreArgs = list(alter = alter, ...), SIMPLIFY = FALSE)
        # test_out <- mapply(ade4::as.randtest, null_models_results, summary_observed, MoreArgs = list(alter = alter), SIMPLIFY = FALSE) ; warning("DEBUG")
        ## Attributing the subsamples names
        names(test_out) <- names(data$subsamples)
    }

    class(test_out) <- c("dispRity", "randtest")

    return(test_out)
}