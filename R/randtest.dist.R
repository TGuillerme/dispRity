#' @title Randtest distance
#'
#' @description Measures the distance between the observed statistic from a \code{"randtest"} object and some specific quantile of the simulated data.
#'
#' @param xtest an object of class \code{"randtest"}
#' @param quantile a \code{numeric} value for the quantile edges to compare the observed data to on either sides (by default \code{quantile = c(0.025. 0.975)}).
#' @param abs \code{logical}, whether to calculate the distance as an absolute value (\code{TRUE}) or not (\code{FALSE} - default).
#' 
#' @details
#' To compare the observed value to the simulated median value, you can use \code{quantile = 0.5}.
#' Also note that when using \code{abs = FALSE} (default), a negative value means that the observed statistic is within the request quantiles.
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
#' ## The distance between the observed data and the 95% quantile
#' randtest.dist(dummy_test)
#' 
#' ## The absolute distance from the median
#' randtest.dist(dummy_test, quantile = 0.5, abs = TRUE)
#' 
#' @seealso \code{\link[ade4]{randtest}} \code{\link{randtest.dispRity}}
#' 
#' @author Thomas Guillerme
#' @export

randtest.dist <- function(xtest, quantile = c(0.025, 0.975), abs = FALSE) {

    randtest <- xtest

    ## Checking randtest
    check.class(randtest, "randtest")

    ## Checking abs
    check.class(abs, "logical")

    ## Checking the quantiles
    check.class(quantile, "numeric")

    ## Checking left or right side of the data
    right_side <- ifelse(randtest$obs - median(randtest$sim) > 0, TRUE, FALSE)

    ## Measuring the quantiles
    quants <- quantile(randtest$sim, prob = quantile)

    ## Measuring the distance
    if(right_side) {
        distance <- randtest$obs - quants[length(quants)]
    } else {
        distance <- quants[1] - randtest$obs
    }

    ## Returning the value
    if(abs){
        return(abs(distance))
    } else {
        return(distance)
    }
}