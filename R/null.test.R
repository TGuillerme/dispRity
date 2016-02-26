#' @name null.test
#'
#' @title Testing a null hypothesis on multidimensional data.
#'
#' @usage null.test(data, replicates, null.distribution, null.arguments = NULL, alter = "two-sided", ...)
#'
#' @description Testing the difference between the observed disparity and disparity under a null model.
#'
#' @param data a \code{dispRity} object.
#' @param replicates the number of replicates for the test.
#' @param null.distrib one or more distribution functions to generate the null model to be passed to \code{\link{space.maker}}.
#' @param null.args any additional arguments to be passed to \code{\link{space.maker}} (\code{default = NULL}).
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
#' ## Testing 
#' results <- null.test(obs_disparity, replicates = 100, null.distrib = rnorm)
#' results
#' plot(results)
#'
#' @seealso \code{\link{space.maker}}, \code{\link{test.dispRity}}, \code{\link{sequential.test}}, \code{\link{sequential.test}}.
#'
#' @references
#' Díaz, S., Kattge, J., Cornelissen, J.H., Wright, I.J., Lavorel, S., Dray, S., Reu, B., Kleyer, M., Wirth, C., Prentice, I.C. and Garnier, E., \bold{2016}. The global spectrum of plant form and function. Nature, 529(7585), pp.167-171.
#'
#' @author Thomas Guillerme
#' @export


null.test <- function(data, replicates, null.distrib, null.args = NULL, alter = "two-sided", scale = FALSE, ...) {
    #Sanitizing
    check.class(data, "dispRity")
    check.length(data, 5, " must be a dispRity object with calculated observed disparity.")

    #replicates
    check.class(replicates, "numeric")
    check.length(replicates, 1, " must be a single numeric value.")

    #null.distrib and null.args
    try(test_space_maker <- space.maker(3, as.numeric(length(null.distrib)), null.distrib, null.args), silent = TRUE)
    #if(exists(test_space_maker)) {
        if(class(test_space_maker) != "matrix") {
            stop("Wrong format for null.distrib and/or null.args.\nSee ?space.maker for more information.")
        }
    #}

    #alter
    alternative_hypothesis <- c("greater", "less", "two-sided")
    if(all(is.na(match(alter, alternative_hypothesis)))) stop("Alternative hypothesis must be one of the following: ", paste(alternative_hypothesis, collapse=", "), sep="")

    #scaling
    check.class(scale, "logical")
    #Null testing

    #Generating the null models
    if(scale == FALSE) {
        null_models_result <- replicate(replicates, summary(dispRity(
            space.maker(as.numeric(length(data$elements)), dimensions = get.from.call(data, "dimensions"), null.distrib, null.args)
        , metric = get.from.call(data, "metric")))$observed)
    } else {
        null_models_result <- replicate(replicates, summary(dispRity(
            scale(space.maker(as.numeric(length(data$elements)), dimensions = get.from.call(data, "dimensions"), null.distrib, null.args))
        , metric = get.from.call(data, "metric")))$observed)
    }
    #testing the null hypothesis
    test_out  <- as.randtest(obs = summary(data)$observed, sim = nulls_models_result, alter = alter, ...)
}