#' @title Models for changes in disparity
#' 
#' @usage
#' BM(data.model.test, pool.variance, control.list)
#' OU(data.model.test, pool.variance, control.list, fixed.optima)
#' Stasis(data.model.test, pool.variance, control.list)
#' Trend(data.model.test, pool.variance, control.list)
#'
#' @description Different models for changes in disparity through time (\code{BM}, \code{OU}, \code{Stasis} and \code{Trend})
#'
#' @param data.model.test A \code{list} of at least four elements: \code{$central_tendency}, \code{$variance}, \cod{$sample_size} and \code{$subsamples}.
#' @param pool.variance A \code{logical}, whether to pool the variance or not.
#' @param control.list A \code{list} for fine-tuning control inputs to be passed to the optim function.
#' @param fixed.optima A \code{logical} to use an estimated optimum value in OU-style models.
#' @param ... Any optional arguments to be passed to the model.
#' 
#' @details
#' \itemize{
#'  \item{"BM"}{Fits a unbiased random walk model of evolution (Felsenstein 1985; Hunt 2006). The model optimises the ancestral state and the 'step-variance' (sigma-squared)}
#'  \item{"OU"}{The Ornstein-Uhlenbeck model of evolution in which the change in variance is constrained to an optimum value (Hansen 1997). In this model there are three parameters: optima, alpha, and ancestral state. The strength of attraction based on the parameter alpha and the ancestral state is estimated from the data. The optima value is estimated from the data, and this can lead to optima being found outside the known data values. If this is the case the model is similar to a trend model. If the argument, fixed.optima is set to TRUE, the model will not estimate optima but constrain it to the first value in the sequence}
#'  \item{"Trend"}{Fits a Brownian motion model with a directional component. This model is also known as the General Random Walk (Hunt 2006). This model has three parameters: the ancestral state, the 'step-variance' (sigma-squared), and the trend component
#"Stasis" in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#'  \item{"Stasis"}{Fits a model in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#' 
#' @return
#' A list of some sort... @@@@ TO DEVELOP
#' 
#' @examples
#'
#' @seealso \code{\link{model.test}}
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export
#' 
#'
#' @name dispRity.models
NULL

#' @export
my_fun <- function() {

    return()
}

#' @export
my_fun <- function() {

    return()
}