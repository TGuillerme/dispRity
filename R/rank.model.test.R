#' @name rank.model.test
#' @title rank.model.test
#'
#' @description A wrapper function for \code{\link{model.test.sim}} to summarise relative fit of multiple models from \code{\link{model.test}}. Gives the relative fit of \code{model.test} output using log-likelihood and AICc values, as well as the Rank Envelope Test significance to elucidate if empirical data is significantly different to simulated data modelled using the estimated model parameters from \code{model.test}
#'
#' @param sim The number of separate simulations.
#' @param model An object of class \code{dispRity} returned from \code{model.test} function.
#' @param plot.sim Logical. If \code{TRUE} the plots of the simulated and observed disparity are returned for all models
#' @param col.simulation Colour options used for the plotting of simulated values. See \code{\link{plot.dispRity}} for more details
#' @param col.observed Colour of the observed data on the plot
#' @param lwd.obs Line width of the observed value
#' @param show.p Logical. If \code{TRUE} the p values from the Rank Envelope Test are shown on each plot
#' 
#' @return A matrix with the relative fit, parameter values, and Rank Envelope test p values for each model, and a plot of simulated data from each model alongside observed data for each model if plot.sim is \code{TRUE} 
#'
#' @examples
#' 
#' ## Mammal disparity through time
#' data(BeckLee_disparity)
#' models <- list("BM", "OU", "multi.OU", "Trend")
#' models.out <- model.test(BeckLee_disparity, models, time.split = 66)
#' 
#' ## assess the significance of simulated data against the empirical distribution
#' rank.model.test(sim=1000, model=models.out, plot.sim=TRUE)
#' 
#' @seealso \code{\link{model.test}} and \code{\link{model.test.sim}}.
#'
#' @references Murrell DJ. 2018. A global envelope test to detect non-random bursts of trait evolution. Methods in Ecology and Evolution. DOI: 10.1111/2041-210X.13006
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export
#' 

rank.model.test <- function(sim=1000, model, plot.sim=TRUE, col.simulation=c("grey30", "#00000020", "#00000020"), col.observed="hotpink", lwd.obs=2, show.p=FALSE) {
	
	match_call <- match.call()

    ## Sanitizing
    ## sim must be a positive whole number
    silent <- check.class(sim, c("numeric", "integer"))
    check.length(sim, 1, msg = " must be the number of simulations to run.")
    sim <- round(sim)
    if(sim < 0) {
        sim <- abs(sim)
    }
    
    ## Model
    class <- check.class(model, "dispRity", msg = " must be a dispRity object from model.test().")
  	if(class(model)[[2]] != "model.test") {
            stop(paste0(match_call$model, " must be a dispRity object output from model.test.\nTry running model.test(", match_call$model, ") first."), call. = FALSE)
        }
        
    summary.models <- summary(model)
    n.models <- dim(summary.models)[1]
    outputs <- suppressWarnings(lapply(1:n.models, function(x) model.test.sim(sim, model, model.rank = x)))
    p.int <- t(sapply(outputs, function(u) c(u$p[[4]], u$p[[5]])))
    results <- cbind(summary.models)
    results <- results[order(results[, 2]), ]
    results <- cbind(results, p.int)
    colnames(results)[(dim(results)[2] - 2):dim(results)[2]] <- c("median p value", "lower p value", "upper p value")
    
 	if(plot.sim) {
	    n.sq <- ceiling(sqrt(n.models))
	    obs.data <- cbind(model$model.data$subsets, model$model.data$central_tendency)
	    max.range <- range(c(sapply(outputs, function(x) range(unlist(x[[1]]$sim)))), obs.data[,2])
	    par(mfrow=c(n.sq, n.sq))
	    
	    for(u in 1:n.models) {
	    	plot(outputs[[u]], ylim=max.range, col=col.simulation)
	    	lines(obs.data[,1], obs.data[,2], col=col.observed, lwd=lwd.obs)
	    	mtext(paste0(rownames(results)[u]), cex=0.6)
	    	if(show.p) legend("bottomleft", paste0("Rank Env. Test, p = ", signif( p.int[u,2], 3), "-", signif( p.int[u,3], 3)), cex=0.5 )
	    	}
	    }
	
	print(results)
}
