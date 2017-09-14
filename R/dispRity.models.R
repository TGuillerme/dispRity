#' @title Models for changes in disparity
#' 
#' @usage
#' BM(data.model.test, pool.variance, control.list)
#' OU(data.model.test, pool.variance, control.list, fixed.optima, n.optima)
#' EB(data.model.test, pool.variance, control.list)
#' Stasis(data.model.test, pool.variance, control.list, fixed.optima, n.optima)
#' Trend(data.model.test, pool.variance, control.list)
#'
#' @description Different models for changes in disparity through time (\code{BM}, \code{OU}, \code{Stasis} and \code{Trend})
#'
#' @param data.model.test A \code{list} of at least four elements: \code{$central_tendency}, \code{$variance}, \code{$sample_size} and \code{$subsamples}.
#' @param pool.variance A \code{logical}, whether to pool the variance or not.
#' @param control.list A \code{list} for fine-tuning control inputs to be passed to the optim function.
#' @param fixed.optima A \code{logical} to use an estimated optimum value in OU-style models.
#' @param n.optima An \code{integer} being the number of optima.
#' @param ... Any optional arguments to be passed to the model.
#' 
#' @details
#' \itemize{
#'  \item{"BM"}{Fits a unbiased random walk model of evolution (Felsenstein 1985; Hunt 2006). The model optimises the ancestral state and the 'step-variance' (sigma-squared)}
#' 
#'  \item{"OU"}{The Ornstein-Uhlenbeck model of evolution in which the change in variance is constrained to an optimum value (Hansen 1997). In this model there are three parameters: optima, alpha, and ancestral state. The strength of attraction based on the parameter alpha and the ancestral state is estimated from the data. The optima value is estimated from the data, and this can lead to optima being found outside the known data values. If this is the case the model is similar to a trend model. If the argument, fixed.optima is set to TRUE, the model will not estimate optima but constrain it to the first value in the sequence}
#' 
#' \item{"Trend"}{Fits a Brownian motion model with a directional component. This model is also known as the General Random Walk (Hunt 2006). This model has three parameters: the ancestral state, the 'step-variance' (sigma-squared), and the trend component}
#'  
#'  \item{"Stasis"}{Fits a model in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#' }
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
BM <- function(data.model.test, pool.variance, control.list, fixed.optima = NULL, n.optima = NULL, ...) {

    ## Pooling the variance
    if(pool.variance) {
        data.model.test <- pooled.variance(data.model.test, rescale.variance = TRUE) #TG: why rescaling?
    }
    
    ## Selecting parameters
    input_parameters <- get.input.parameters(data.model.test, model.name = "BM")

    ## Updating the control list
    control_list <- update.control.list(control.list, input_parameters)

    ## Optimise the model
    optimised_model <- stats::optim(input_parameters, fn = optim.bm.ml, control = control_list, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = data.model.test)

    #~~~~~~~~
    #TG: We should find a smart way to allow these optimisation options to be changed easily.
    #~~~~~~~~

    ## Return parameters list
    return(extract.argument(optimised_model, data.model.test, model.name = "BM"))
}

#' @export
OU <- function(data.model.test, pool.variance, control.list, fixed.optima, n.optima, ...) { 

    ## Extra arguments: #TG: to be dealt with properly!
    n.optima = 1
    time_split = NULL
    time_split <- ifelse(is.null(time_split), 0, time_split)
    time_split <- which.min(abs(data.model.test$subsamples - time_split))
    
    ## Pooling the variance
    if(pool.variance) {
        data.model.test <- pooled.variance(data.model.test, rescale.variance = TRUE)
    }

    ## Selecting parameters
    input_parameters <- get.input.parameters(data.model.test, model.name = "OU", fixed.optima = fixed.optima, n.optima = n.optima)

    ## Control list
    control_list <- update.control.list(control.list, input_parameters)

    ## Select the lower model
    lower_model <- c(NA, 1e-10, 1e-08, rep(NA, ifelse(fixed.optima, n.optima - 1, n.optima)))

    ## Optimise the model
    optimised_model <- stats::optim(input_parameters, fn = optim.ou.ml, control = control_list, method = "L-BFGS-B", lower = lower_model, hessian = FALSE, data.model.test = data.model.test, time.split = time_split, n.optima = n.optima, fixed.optima = fixed.optima)

    ## Return the parameters list
    parameter_return <- extract.argument(optimised_model, data.model.test, model.name = "OU", fixed.optima = fixed.optima)

    ## Adding the time splits
    if(n.optima > 1) { 
        n_par <- length(parameter_return)
        parameter_return <- c(parameter_return,  input_time_split)
        names(parameter_return)[-c(1:n_par)] <- paste0("time_split", 1:length(input_time_split))
    }

    return(parameter_return)
}

#' @export
EB <- function (data.model.test, pool.variance, control.list, fixed.optima = NULL, n.optima = NULL, ...) {

    ## Pooling the variance
    if(pool.variance) {
        data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
    }

    ## Selecting the parameters
    input_parameters <- get.input.parameters(data.model.test, model.name = "EB")

    ## Updating the control list
    control_list <- update.control.list(control.list, input_parameters)
        
    ## Optimising the model
    optimised_model <- stats::optim(input_parameters, fn = optim.eb.ml, control = control_list, method = "L-BFGS-B", lower = c(NA, 0, -20), upper=c(Inf, Inf, -1e-6), data.model.test = data.model.test)
    
    ## Return parameters list
    return(extract.argument(optimised_model, data.model.test, model.name = "EB"))
}

#' @export
Stasis <- function(data.model.test, pool.variance, control.list, fixed.optima = NULL, n.optima, ...) {

    ## Extra arguments: #TG: to be dealt with properly!
    n.optima = 1
    time_split = NULL
    time_split <- ifelse(is.null(time_split), 0, time_split)
    time_split <- which.min(abs(data.model.test$subsamples - time_split))
    input_time_split <- max(data.model.test[[4]]) - time_split
    time_split <- which.min(abs(data.model.test$subsamples - time_split))
    
    ## Pooling the variance
    if(pool.variance)  {
        data.model.test <- pooled.variance(data.model.test, rescale.variance = TRUE)
    }

    ## Selecting the parameters
    input_parameters <- get.input.parameters(data.model.test, model.name = "Stasis", n.optima = n.optima)

    ## Updating the control list
    control_list <- update.control.list(control.list, input_parameters)

    ## Optimising the model
    optimised_model <- stats::optim(input_parameters, fn = optim.stasis.ml, control = control_list, method = "L-BFGS-B", lower = c(0, rep(NA, n.optima)), data.model.test = data.model.test, time.split = time_split, n.optima = n.optima)
       
    ## Return the parameters list
    parameter_return <- extract.argument(optimised_model, data.model.test, model.name = "Stasis", n.optima = n.optima)
    
    ## Adding the eventual time splits
    if(n.optima > 1) { 
        n_par <- length(parameter_return)
        parameter_return <- c(parameter_return,  input_time_split)
        names(parameter_return)[-c(1:n_par)] <- paste0("time_split", 1:length(input_time_split))
    }

    return(parameter_return)
}

#' @export
Trend <- function (data.model.test, pool.variance, control.list, fixed.optima = NULL, n.optima = NULL, ...) {

    ## Pooling the variance
    if(pool.variance) {
        data.model.test <- pooled.variance(data.model.test, rescale.variance = TRUE)
    }

    ## Selecting the parameters
    input_parameters <- get.input.parameters(data.model.test, model.name = "Trend")

    ## Updating the control list
    control_list <- update.control.list(control.list, input_parameters)

    ## Optimising the model
    optimised_model <- stats::optim(input_parameters, fn = optim.trend.ml, control = control_list, method = "L-BFGS-B", lower = c(NA, 0, -100), data.model.test = data.model.test)

    ## Return the parameters list
    return(extract.argument(optimised_model, data.model.test, model.name = "Trend"))
}
