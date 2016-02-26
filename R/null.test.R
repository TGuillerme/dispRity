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
#' @param null.distribution one or more distribution functions to generate the null model to be passed to \code{\link{space.maker}}.
#' @param null.arguments any additional arguments to be passed to \code{\link{space.maker}} (\code{default = NULL}).
#' @param alter the type of alternative hypothesis (H1) as used in \code{\link[ade4]{randtest}} (\code{default = "two-sided"}).
#' @param ... optional arguments to be passed to \code{\link[ade4]{as.randtest}}.
#'
#' @details
#'
#' @examples
#'
#' @seealso \code{\link{space.maker}}, \code{\link{test.dispRity}}, \code{\link{sequential.test}}, \code{\link{sequential.test}}.
#'
#' @references
#' Díaz, S., Kattge, J., Cornelissen, J.H., Wright, I.J., Lavorel, S., Dray, S., Reu, B., Kleyer, M., Wirth, C., Prentice, I.C. and Garnier, E., \bold{2016}. The global spectrum of plant form and function. Nature, 529(7585), pp.167-171.
#'
#' @author Thomas Guillerme



null.test <- function(data, replicates, null.distribution, null.arguments = NULL, alter = "two-sided", ...) {

    # Series can be an observed value or a bootstrapped one

    # null.rule must be the type of null model (or the null model "rule") (can invoke space.maker)

    # replicates (of the null.rule)

    # ... any optionals to be passed to test space.maker

    #Final example (should look like that)
    #test.dispRity(sum_of_ranges, test = randtest, "null", null = make.null())
}

data <- data
replicates <- 100
alter <- "less"
distribution <- rnorm
arguments <- NULL

# In fun

metrics <- strsplit(strsplit(data$call, split = "Disparity calculated as: ")[[1]][[2]], split = " for ")[[1]][[1]]
dimensions <- strsplit(strsplit(data$call, split = " for ")[[1]][[2]], split = " dimensions")[[1]][[1]]


test_out <- as.randtest(obs = summary(data)$observed,
    sim = replicate(replicates, summary(dispRity(space.maker(as.numeric(length(data$elements)), dimensions = eval(parse(text = dimensions)), distribution, arguments), metric = eval(parse(text = metrics))))$observed),
    alter = alter)


plot(test_out)