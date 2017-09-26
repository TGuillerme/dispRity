#' @name model.test
#'
#' @title Model Test
#'
#' @description Fit models of disparity change through time
#'
#' @param input_disparity A dispRity object used to test models of evolution through time
#' @param model The named model of evolution to allow for changes in disparity-through-time using a homogenous or hetergenous model. A single model can be used as input or multiple modes can be supplied in a vector. If a vector with multiple modes is supplied then the model will test for shifts in modes at the time supplied by 'time.split'. If a list is supplied different models will be fit to the data (See \bold{Details}) 
#' @param pool.variance If NULL the difference in variances will be calculated using Bartlett's Test of equal variances. If there is no significant difference among variances, then variance in samples will be pooled and the same variance will be used for all samples. A significance difference will not pool variances and the original variance will be used for model-testing. If argument TRUE or FALSE are used, Bartlett's test will be ignored and the analyses will use the user-set pooling of variances
#' @param time.split The age of the change in mode. The age is measured as the time before the most recent sample, and multiple ages can be supplied in a vector. If no age is supplied for models then all possible time shifts are fit in the model, and the highest likelihood model is returned. Note this only applies to heterogenous models (See \bold{Details}) 
#' @param return.model.full Logical argument to return full parameter values from each tested model (default = FALSE)
#' @param plot.disparity Logical argument to plot the measure of disparity through time and a barplot of the relative support for each model (default = FALSE)
#' @param fixed.optima Logical to use an estimated optimum value in OU models (fixed.optima = FALSE), or whether to set the OU optimum to the ancestral value (fixed.optima = TRUE)
#' @param control.list Fine-tune control inputs for the optim function
#' @details The models are fit using maximum likelihood optimisation using the function optim. Fine-tuning of the search algorithms can be applied using the control.list argument. Models can be fit using a homogenous model with the same process applied to the entire sequence or models with time splits that represent a change in parameters or a shift in mode. For the time split model if a time value is provided, then the shift is tested at that value only. If no time shift is supplied then all shift times that allow for there to be at least 10 samples in each time bin are tested. If the sample is fewer than 30 samples long then no time splits are searched for (unless a time split is supplied by the user). Parameters are shared across different modes. For example, c("BM", "OU") would fit a model in which the process starts with a BM model and shifts toan OU process. The ancestral value at the start of the sequence and sigma squared value are shared across the models. Any combination of the following homogenous models (with the exception of 'multi.OU') can be fit to the data:
#' \itemize{
#'  \item{""BM""}{Fits a unbiased random walk model of evolution (Felsenstein 1985; Hunt 2006). The model optimises the ancestral state and the 'step-variance' (sigma-squared)}
#'  \item{""OU""}{The Ornstein-Uhlenbeck model of evolution in which the change in variance is constrained to an optimum value (Hansen 1997). In this model there are three parameters: optima, alpha, and ancestral state. The strength of attraction based on the parameter alpha and the ancestral state is estimated from the data. The optima value is estimated from the data, and this can lead to optima being found outside the known data values. If this is the case the model is similar to a trend model. If the argument, fixed.optima is set to TRUE, the model will not estimate optima but constrain it to the first value in the sequence}
#'  \item{""Trend""}{Fits a Brownian motion model with a directional component. This model is also known as the General Random Walk (Hunt 2006). This model has three parameters: the ancestral state, the 'step-variance' (sigma-squared), and the trend component
#"Stasis" in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#'  \item{""Stasis""}{Fits a model in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#'  \item{""Early Burst""}{Trait variance accumulates early in the evolution of a trait and decreases exponentially through time (Blomberg et al. 2003; Harmon et al. 2010). This model has three parameters: ancestral state, sigma-squared, and the exponential rate of decrease}
#'  \item{""multi.OU""}{Fits a model in which the value of the optima shifts at one or more time splits. The values of the 'step-variance' (sigma squared) and attraction to the optima (alpha) are shared across all the samples. This model can not be fit with other models - the multiOU system can be be fit to the model only}
#' }
#'
#' @examples
#' ## To Add
#' @seealso \code{\link{model.test.sim}}.
#'
#' @references
#' To Add: Hunt 2006, Hunt 2008, Harmon 2010, 
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export

# input_disparity <- sumVar
# pool.variance=NULL
# return.model.full=FALSE
# plot.disparity=TRUE
# fixed.optima=FALSE
# control.list=list(fnscale = -1)
# pool.variance=F
# model=models.to.test

model.test <- function(input_disparity, model, pool.variance=NULL, time.split.model=NULL, return.model.full=FALSE, plot.disparity=TRUE, fixed.optima=FALSE, control.list=list(fnscale = -1)) {
	
	n.models <- length(model)
		
	## convert dispRity to model.test object
	model.test_input <- select.model.list(input_disparity)
	
	## use Bartlett's test of variance to decide whether to pool variance or not (not used if pool variance is specified as TRUE or FALSE before-hand)
	
	if(is.null(pool.variance)) {
		p.test <- bartlett.variance(model.test_input)
		if(p.test < 0.05) {
			pool.variance <- FALSE
			cat("evidence of unequal variance, Bartlett's test of equal variances p = ", signif(p.test, 3), ". Variance not pooled")
			cat("\n")
			} else {
			pool.variance <- TRUE
			cat("evidence of equal variance, Bartlett's test of equal variances p = ", signif(p.test, 3), ". Variance pooled")
			cat("\n")
			model.test_input <- pooled.variance(model.test_input, T)
		}
	} else {
		if(pool.variance) model.test_input <- pooled.variance(model.test_input, T)
	} 
	
	model.out <- lapply(1:n.models, function(model_n) {
		
		model.type <- model[[model_n]]
		time.split <- time.split.model
		
		if(length(model.type) == 1 && model.type != "multi.OU") {
			time.split <- NULL
			} else {
			time.split <- time.split.model
		}
			
		if(is.null(time.split) && length(model.type) == 2 || is.null(time.split) && model.type == "multi.OU") {
			
			all.times <- max(model.test_input[[4]]) - model.test_input[[4]]				
			
			if(length(all.times) > 31) {
				ten.times <- 9 : (length(all.times) - 11)
				run.time.split <- TRUE
				cat("Running",  model.type ,"on", length(ten.times), "shift times")
				cat("\n")
				cat("model ", 1, " of ", length(ten.times))
				model.test.all.times <- lapply(ten.times, function(x) {
					cat("\r", "model ", x - 8, " of ", length(ten.times))
					model.test.lik(model.test_input, model.type, time.split=x, control.list, fixed.optima=fixed.optima)
				})
				best.model <- which.max(sapply(model.test.all.times, function(x) x[[2]]))
				model.return <- model.test.all.times[best.model][[1]]	
				cat(" best split time found at", ten.times[best.model])
				cat(" log-likelihood: ", model.return$value)
				cat(". Finished.")
				cat("\n")		
			} else {
				warning("fewer than 30 samples - time split models not run")
				run.time.split <- FALSE
			}
		
		} else {
		
				cat("Running ", model.type, "model")
				model.return <- model.test.lik(model.test_input, model.type.in = model.type, time.split, control.list, fixed.optima=fixed.optima)
				cat(" log-likelihood: ", model.return$value)
				cat(" Finished.")
				cat("\n")
		}
		model.return
	})
	
	## judge all models using AICc values
	model.parameters <- sapply(model, length) - 1 + sapply(model.out, function(x) length(x[[1]]))
	model.likelihoods <- unlist(sapply(model.out, function(x) x[2]))
	sample_size <- length(model.test_input[[1]])
	 
	 aic <- aicc <- c()
	 
	 for(y in 1:length(model.likelihoods)) {
	 	aic <- c(aic, (-2 * model.likelihoods[y]) + (2 * model.parameters[y]))
	 	aicc <- c(aicc, (-2 * model.likelihoods[y]) + (2 * model.parameters[y]) * (sample_size / ( sample_size - model.parameters[y] - 1)))
	}
	
	model.names <- sapply(model, function(x) paste(x, collapse="_"))
	
	names(aic) <- model.names
	delta.aicc <- aicc - min(aicc)
	weight.aicc <- exp(-0.5 * delta.aicc) / sum(exp(-0.5 * delta.aicc))
	order.aicc <- order(weight.aicc, decreasing=T)

	return.out <- list()
	return.out$aicc.models <- cbind(aicc, delta.aicc, weight.aicc)
	rownames(return.out$aicc.models) <- model.names
	
	if(plot.disparity) {
	
		par(mfrow=c(1, 2), mar=c(4,4,2,2), oma=c(10, 4, 4, 4))
		xAxis <- max(model.test_input[[4]]) - model.test_input[[4]]
		plot(xAxis, model.test_input[[1]], type="l", xlim=c(max(xAxis), 0), xlab="Time", ylab="central tendency", las=1)
		varUp <- model.test_input[[1]] + model.test_input[[2]]
		varDown <- model.test_input[[1]] - model.test_input[[2]]
		polygon(x=c(xAxis, rev(xAxis)), c(varUp, rev(varDown)), col="grey", border=F)
		lines(xAxis, model.test_input[[1]], col="grey50")
		abline(v=time.split.model, lty=2, lwd=2)
		plotcor <- barplot(weight.aicc[order.aicc], las=1, ylim=c(0, 1), col="grey30", border=F, ylab="Akaike weights", names=F)
		mtext(model.names[order.aicc], 1, las=2, at=plotcor[,1], line=1)
	}
	
	if(return.model.full) {
		return.out$full.details <- model.out
		names(return.out$full.details) <- model.names
	}
	
	class(return.out) <- "dispRity.model.test"
	invisible(return.out)
}	
