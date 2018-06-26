#' @name rank.model.test
#' @title rank.model.test
#'
#' @description A wrapper function for \code{\link{model.test.sim}} to summarise relative fit of multiple models from \code{\link{model.test}}. Gives the relative fit of \code{model.test} output using log-likelihood and AICc values, as well as a significance test to elucidate if empirical data is significantly different to simulated data modelled using the estimated model parameters from \code{model.test}
#'
#' @param sim The number of separate simulations.
#' @param model An object of class \code{dispRity} returned from \code{model.test} function.
#' 
#' @return A matrix with the relative fit, parameter values, and Rank Envelope test p values for each model
#'
#' @examples
#' 
#' ## Mammal disparity through time
#' data(BeckLee_disparity)
#' models <- list("BM", "OU", "multi.OU", "Trend")
#' models.out <- model.test(BeckLee_disparity, models, time.split = 66)
#' 
#' ## assess the significance of simulated data against the empirical distribution
#' rank.model.test(sim=1000, model=models.out)
#' 
#' @seealso \code{\link{model.test}} and \code{\link{model.test.sim}}.
#'
#' @references Murrell DJ. 2018. A global envelope test to detect non‐random bursts of trait evolution. Methods in Ecology and Evolution. DOI: 10.1111/2041-210X.13006
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export
#' 

rank.model.test <- function(sim=1000, model) {
	
	match_call <- match.call()

    ## Sanitizing
    ## sim must be a positive whole number
    silent <- check.class(sim, c("numeric", "integer"))
    check.length(sim, 1, msg = " must be the number of simulations to run.")
    sim <- round(sim)
    if(sim < 0) {
        sim <- abs(sim)
    }
    
    ## Model
    class <- check.class(model, "dispRity", msg = " must be a dispRity object from model.test().")
  	if(class(model)[[2]] != "model.test") {
            stop(paste0(match_call$model, " must be a dispRity object output from model.test.\nTry running model.test(", match_call$model, ") first."), call. = FALSE)
        }
  	
	summary.models <- summary(model)
	outputs <- lapply(1:dim(summary.models)[1], function(x) model.test.sim(sim, model, model.rank=x))
	p.int <- t(sapply(outputs, function(u) c(u$p[[4]], u$p[[5]])))
	results <- cbind(summary.models, p.int)
	colnames(results)[(dim(results)[2]-2):dim(results)[2]] <- c("median p value", "lower p value", "upper p value")
	results <- results[order(results[,2]),]
	print(results)
}