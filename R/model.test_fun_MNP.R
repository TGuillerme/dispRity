#' @name model.test
#'
#' @title Model Test
#'
#' @description Fit models of disparity change through time
#'
#' @param input_disparity A dispRity object used to test models of evolution through time
#' @param named.model The named model of evolution to allow for changes in disparity-through-time using a homogenous or hetergenous model (See \bold{Details}) 
#' @param custom.model The custom model of evolution to describe the change in disparity-through-time using a hetergenous model with two distinct modes of evolution. This argument requires a time.split argument (See \bold{Details}) 
#' @param pool.variance If NULL the difference in variances will be calculated using Bartlett's Test of equal variances. If there is no significant difference among variances, then variance in samples will be pooled and the same variance will be used for all samples. A significance difference will not pool variances and the original variance will be used for model-testing. If argument TRUE or FALSE are used, Bartlett's test will be ignored and the analyses will use the user-set pooling of variances
#' @param time.split The age of the change in mode. The age is measured as the time before the most recent sample, and multiple ages can be supplied in a vector. If no age is supplied for models then all possible time shifts are fit in the model, and the highest likelihood model is returned. Note this only applies to heterogenous models (See \bold{Details}) 
#' @param return.model.full Logical argument to return full parameter values from each tested model (default = FALSE)
#' @param plot.disparity Logical argument to plot the measure of disparity through time and a barplot of the relative support for each model (default = FALSE)
#' @param fixed.optima Logical to use an estimated optimum value in OU models (fixed.optima = FALSE), or whether to set the OU optimum to the ancestral value (fixed.optima = TRUE)
#' @param control.list Fine-tune control inputs for the optim function
#' @details The models are fit using maximum likelihood optimisation using the function optim. Fine-tuning of the search algorithms can be applied using the control.list argument. Models can be fit using a homogenous model with the same process applied to the entire sequence or models with time splits that represent a change in parameters or a shift in mode. For the time split model if a time value is provided, then the shift is tested at that value only. If no time shift is supplied then all shift times that allow for there to be at least 10 samples in each time bin are tested. If the sample is fewer than 30 samples long then no time splits are searched for (unless a time split is supplied by the user). The following homogenous models can be fit to the data
#' \itemize{
#'  \item{""BM""}{Fits a unbiased random walk model of evolution (Felsenstein 1985; Hunt 2006). The model optimises the ancestral state and the 'step-variance' (sigma-squared)}
#'  \item{""OU""}{The Ornstein-Uhlenbeck model of evolution in which the change in variance is constrained to an optimum value (Hansen 1997). In this model there are three parameters: optima, alpha, and ancestral state. The strength of attraction based on the parameter alpha and the ancestral state is estimated from the data. The optima value is estimated from the data, and this can lead to optima being found outside the known data values. If this is the case the model is similar to a trend model. If the argument, fixed.optima is set to TRUE, the model will not estimate optima but constrain it to the first value in the sequence}
#'  \item{""Trend""}{Fits a Brownian motion model with a directional component. This model is also known as the General Random Walk (Hunt 2006). This model has three parameters: the ancestral state, the 'step-variance' (sigma-squared), and the trend component
#"Stasis" in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#'  \item{""Stasis""}{Fits a model in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#'  \item{""Early Burst""}{Trait variance accumulates early in the evolution of a trait and decreases exponentially through time (Blomberg et al. 2003; Harmon et al. 2010). This model has three parameters: ancestral state, sigma-squared, and the exponential rate of decrease}
#'  \item{""multiOU""}{Fits a model in which the value of the optima shifts at one or more time splits. The values of the 'step-variance' (sigma squared) and attraction to the optima (alpha) are shared across all the samples}
#'  \item{""multiStasis""}{Fits a model in which the value of the mean stasis value (theta) shifts at one or more time splits. The value of the noise around these mean values (omega) is shared across the entire sequence}
#'  \item{""BM.to.Trend", "BM.to.EB", "BM.to.Stasis", "BM.to.OU", "OU.to.BM", "OU.to.Trend", "OU.to.EB", "Stasis.to.BM", "Stasis.to.EB", "Stasis.to.Trend", "Trend.to.OU", "Trend.to.Stasis""}{In these models the ancestral state is taken as from the mode in the first sequence and is used as the starting ancestral state in the second sequence}
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

model.test <- function(input_disparity, named.model = c("BM", "OU", "Stasis", "EB", "Trend", "multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "BM.to.Stasis", "BM.to.OU", "OU.to.BM", "OU.to.Trend", "OU.to.EB", "Stasis.to.BM", "Stasis.to.EB", "Stasis.to.Trend", "Trend.to.OU", "Trend.to.Stasis"), custom.model=NULL, pool.variance=NULL, time.split=NULL, return.model.full=FALSE, plot.disparity=TRUE, fixed.optima=FALSE, control.list=list(fnscale = -1)) {
	
	## convert dispRity to model.test object
	model.test_input <- select.model.list(input_disparity)
	
	## use Bartlett's test of variance to decide whether to pool variance or not (not used if pool variance is specified as TRUE or FALSE before-hand)
	
	if(is.null(pool.variance)) {
		p.test <- bartlett.variance(model.test_input)
		if(p.test < 0.05) {
			pool.variance <- FALSE
			cat("evidence of unequal variance, Bartlett's test of equal variances p = ", signif(p.test, 3), ". Variance not pooled")
			} else {
			pool.variance <- TRUE
			cat("evidence of equal variance, Bartlett's test of equal variances p = ", signif(p.test, 3), ". Variance pooled")
			model.test_input <- pooled.variance(model.test_input, T)
		}
	}
	
	## identify models to run
	
	model.test.names <- c("BM", "OU", "Stasis", "EB", "Trend", "multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "BM.to.Stasis", "BM.to.OU", "OU.to.BM", "OU.to.Trend", "OU.to.EB", "Stasis.to.BM", "Stasis.to.EB", "Stasis.to.Trend", "Trend.to.OU", "Trend.to.Stasis")
	test.model <- match(named.model, model.test.names)
	try.models <- model.test.names[test.model]
	
	## identify time shifts if none supplied
	
	any.shift.model <- any(!is.na(match(try.models,  c("multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "BM.to.Stasis", "BM.to.OU", "OU.to.BM", "OU.to.Trend", "OU.to.EB", "Stasis.to.BM", "Stasis.to.EB", "Stasis.to.Trend", "Trend.to.OU", "Trend.to.Stasis"))))
	
	## identify shifts that allow at least 10 samples before or after shift. The model will not run (with a warning) if there are too few samples for ten samples before/after shift
	
	if(any.shift.model) {
		all.times <- max(model.test_input[[4]]) - model.test_input[[4]]
		if(length(all.times) > 31) {
			ten.times <- 9 : (length(all.times) - 11)
			run.time.split <- TRUE
			cat("\n")
			cat("time split models will be tested on", length(ten.times), "shift times")
		} else {
			warning("fewer than 30 samples - time split models not run")
			run.time.split <- FALSE
		}
	}
	
	## run models
	model.list <- list()
	
	if(any(try.models == "BM")) {
		cat("\n", "testing BM model")
		model.list$bm <- model.test.bm(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list)
		cat(", AICc =", model.list$bm["AICc"])
	}
	
	if(any(try.models == "OU")) {
		cat("\n", "testing OU model")
		model.list$ou <- model.test.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, fixed.optima=fixed.optima)
		cat(", AICc =", model.list$ou["AICc"])
	}
	
	if(any(try.models == "Stasis")) {
		cat("\n", "testing Stasis model")
		model.list$stasis <- model.test.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list)
		cat(", AICc =", model.list$stasis["AICc"])
	}
	
	if(any(try.models == "EB")) {
		cat("\n", "testing Early Burst model")
		model.list$eb <- model.test.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list)
		cat(", AICc =", model.list$eb["AICc"])
	}
	
	if(any(try.models == "Trend")) {
		cat("\n", "testing Trend model")
		model.list$trend <- model.test.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list)
		cat(", AICc =", model.list$trend["AICc"])
	}
	
	## For multi-mode and shift models if the user supplies a shift time a priori only that shift time is test. If shift time is not supplied, the fit of a shift is tested at all time points and returns the model with the highest likelihood
	
	if(any(try.models == "multiOU")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "multiOU model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.ou <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				#flush.console()
				model.test.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x], n.optima=2, fixed.optima=fixed.optima)
			})
			
			cat(". Finished.")
			high.mod.ou <- which(model.test.all.ou[1, ] == max(model.test.all.ou[1, ]))
			out.best.model.ou <- model.test.all.ou[ , high.mod.ou]
			model.list$multi.ou <- out.best.model.ou
		}
		
		## if time.split supplied, test that time only	
		if(!is.null(time.split)) {
			cat("\n", "multiOU model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$multi.ou <- model.test.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split, n.optima=n.split, fixed.optima=fixed.optima)
		}	
		cat("\n", "AICc =", model.list$multi.ou["AICc"])	
	}
	
	if(any(try.models == "multiStasis")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "multiStasis model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.stasis <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x], n.optima=2)
			})
			
			cat(". Finished.")
			high.mod.stasis <- which(model.test.all.stasis[1, ] == max(model.test.all.stasis[1, ]))
			out.best.model.stasis <- model.test.all.stasis[ , high.mod.stasis]
			model.list$multi.stasis <- out.best.model.stasis
		}
		
		## if time.split supplied, test that time only	
		if(!is.null(time.split)) {
			cat("\n", "multiStasis model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$multi.stasis <- model.test.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split, n.optima=n.split)
		}
		cat("\n", "AICc =", model.list$multi.stasis["AICc"])	
	}

	if(any(try.models == "BM.to.Trend")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "BM.to.Trend model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.bm.to.trend <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.bm.to.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			
			cat(". Finished.")
			high.mod.bm.to.trend <- which(model.test.all.bm.to.trend[1, ] == max(model.test.all.bm.to.trend[1, ]))
			out.best.model.bm.to.trend <- model.test.all.bm.to.trend[ , high.mod.bm.to.trend]
			model.list$bm.to.trend <- out.best.model.bm.to.trend
		}
		
		## if time.split supplied, test that time only	
		if(!is.null(time.split)) {
			cat("\n", "BM.to.Trend model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$bm.to.trend <- model.test.bm.to.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$bm.to.trend["AICc"])	
	}
	
	if(any(try.models == "BM.to.EB")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "BM.to.EB model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.bm.to.eb <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.bm.to.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			
			cat(". Finished.")
			high.mod.bm.to.eb <- which(model.test.all.bm.to.eb[1, ] == max(model.test.all.bm.to.eb[1, ]))
			out.best.model.bm.to.eb <- model.test.all.bm.to.eb[ , high.mod.bm.to.eb]
			model.list$bm.to.eb <- out.best.model.bm.to.eb
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "BM.to.EB model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$bm.to.eb <- model.test.bm.to.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$bm.to.eb["AICc"])
	}
	
	if(any(try.models == "BM.to.Stasis")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "BM.to.Stasis model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.bm.to.stasis <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.bm.to.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			
			cat(". Finished.")
			high.mod.bm.to.stasis <- which(model.test.all.bm.to.stasis[1, ] == max(model.test.all.bm.to.stasis[1, ]))
			out.best.model.bm.to.stasis <- model.test.all.bm.to.stasis[ , high.mod.bm.to.stasis]
			model.list$bm.to.stasis <- out.best.model.bm.to.stasis
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "BM.to.Stasis model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$bm.to.stasis <- model.test.bm.to.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$bm.to.stasis["AICc"])
	}
	
	if(any(try.models == "BM.to.OU")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "BM.to.OU model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.bm.to.ou <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.bm.to.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x], fixed.optima = fixed.optima)
			})
			
			cat(". Finished.")
			high.mod.bm.to.ou <- which(model.test.all.bm.to.ou[1, ] == max(model.test.all.bm.to.ou[1, ]))
			out.best.model.bm.to.ou <- model.test.all.bm.to.ou[ , high.mod.bm.to.ou]
			model.list$bm.to.ou <- out.best.model.bm.to.ou
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "BM.to.OU model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$bm.to.ou <- model.test.bm.to.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split, fixed.optima = fixed.optima)
		}
		cat("\n", "AICc =", model.list$bm.to.ou["AICc"])
	}
	
	if(any(try.models == "OU.to.BM")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "OU.to.BM model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.ou.to.bm <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.ou.to.bm(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x], fixed.optima = fixed.optima)
			})
			
			cat(". Finished.")
			high.mod.ou.to.bm <- which(model.test.all.ou.to.bm[1, ] == max(model.test.all.ou.to.bm[1, ]))
			out.best.model.ou.to.bm <- model.test.all.ou.to.bm[ , high.mod.ou.to.bm]
			model.list$ou.to.bm <- out.best.model.ou.to.bm
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "OU.to.BM model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$ou.to.bm <- model.test.ou.to.bm(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split, fixed.optima = fixed.optima)
		}
	}
	
	if(any(try.models == "OU.to.Trend")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "OU.to.Trend model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.ou.to.trend <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.ou.to.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x], fixed.optima = fixed.optima)
			})
			
			cat(". Finished.")
			high.mod.ou.to.trend <- which(model.test.all.ou.to.trend[1, ] == max(model.test.all.ou.to.trend[1, ]))
			out.best.model.ou.to.trend <- model.test.all.ou.to.trend[ , high.mod.ou.to.trend]
			model.list$ou.to.trend <- out.best.model.ou.to.trend
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "OU.to.Trend model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$ou.to.trend <- model.test.ou.to.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split, fixed.optima = fixed.optima)
		}
		cat("\n", "AICc =", model.list$ou.to.trend["AICc"])
	}
	
	if(any(try.models == "OU.to.EB")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "OU.to.EB model. Running ", length(ten.times), " time split models:")
			cat("\n", "")
			model.test.all.ou.to.eb <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.ou.to.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x], fixed.optima = fixed.optima)
			})
			
			cat(". Finished.")
			high.mod.ou.to.eb <- which(model.test.all.ou.to.eb[1, ] == max(model.test.all.ou.to.eb[1, ]))
			out.best.model.ou.to.eb <- model.test.all.ou.to.eb[ , high.mod.ou.to.eb]
			model.list$ou.to.eb <- out.best.model.ou.to.eb
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "OU.to.EB model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$ou.to.eb <- model.test.ou.to.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split, fixed.optima = fixed.optima)
		}
		cat("\n", "AICc =", model.list$ou.to.eb["AICc"])	
	}
	
	if(any(try.models == "Stasis.to.BM")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "Stasis.to.EB model. Running ", length(ten.times), " time split models:")
			cat("\n", "\n", "")
			model.test.all.stasis.to.bm <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.stasis.to.bm(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			
			cat(". Finished.")
			high.mod.stasis.to.bm <- which(model.test.all.stasis.to.bm[1, ] == max(model.test.all.stasis.to.bm[1, ]))
			out.best.model.stasis.to.bm <- model.test.all.stasis.to.bm[ , high.mod.stasis.to.bm]
			model.list$stasis.to.bm <- out.best.model.stasis.to.bm
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "Stasis.to.BM model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$stasis.to.bm <- model.test.stasis.to.bm(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$stasis.to.bm["AICc"])
	}

	if(any(try.models == "Stasis.to.EB")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "Stasis.to.EB model. Running ", length(ten.times), "models:")
			cat("\n", "")
			model.test.all.stasis.to.eb <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.stasis.to.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			
			cat(". Finished.")
			high.mod.stasis.to.eb <- which(model.test.all.stasis.to.eb[1, ] == max(model.test.all.stasis.to.eb[1, ]))
			out.best.model.stasis.to.eb <- model.test.all.stasis.to.eb[ , high.mod.stasis.to.eb]
			model.list$stasis.to.eb <- out.best.model.stasis.to.eb
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "Stasis.to.EB model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$stasis.to.eb <- model.test.stasis.to.eb(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$stasis.to.eb["AICc"])	
	}
	
	if(any(try.models == "Stasis.to.Trend")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "Stasis.to.Trend model. Running ", length(ten.times), "models:")
			cat("\n", "")
			model.test.all.stasis.to.trend <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.stasis.to.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			
			cat(". Finished.")
			high.mod.stasis.to.trend <- which(model.test.all.stasis.to.trend[1, ] == max(model.test.all.stasis.to.trend[1, ]))
			out.best.model.stasis.to.trend <- model.test.all.stasis.to.trend[ , high.mod.stasis.to.trend]
			model.list$stasis.to.trend <- out.best.model.stasis.to.trend
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "Stasis.to.Trend model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$stasis.to.trend <- model.test.stasis.to.trend(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$stasis.to.trend["AICc"])	
	}
	
	if(any(try.models == "Trend.to.OU")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "Trend.to.OU model. Running ", length(ten.times), "models:")
			cat("\n", "")
			model.test.all.trend.to.ou <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.trend.to.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			cat(". Finished.")
			high.mod.trend.to.ou <- which(model.test.all.trend.to.ou[1, ] == max(model.test.all.trend.to.ou[1, ]))
			out.best.model.trend.to.ou <- model.test.all.trend.to.ou[ , high.mod.trend.to.ou]
			model.list$trend.to.ou <- out.best.model.trend.to.ou
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "Trend.to.OU model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$trend.to.ou <- model.test.trend.to.ou(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$trend.to.ou["AICc"])
	}
	
	if(any(try.models == "Trend.to.Stasis")) {
		
		## if no time.split supplied, test all times (with both modes having at least 10 samples)
		if(is.null(time.split) && run.time.split) {
			cat("\n", "Trend.to.Stasis model. Running ", length(ten.times), "models:")
			cat("\n", "")
			model.test.all.trend.to.stasis <- sapply(ten.times, function(x) {
				cat("\r", "model ", x - 8, " of ", length(ten.times))
				model.test.trend.to.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=all.times[x])
			})
			cat(". Finished.")
			high.mod.trend.to.stasis <- which(model.test.all.trend.to.stasis[1, ] == max(model.test.all.trend.to.stasis[1, ]))
			out.best.model.trend.to.stasis <- model.test.all.trend.to.stasis[ , high.mod.trend.to.stasis]
			model.list$trend.to.stasis <- out.best.model.trend.to.stasis
		}
		
		## if time.split supplied, test those times only	
		if(!is.null(time.split)) {
			cat("\n", "Trend.to.Stasis model. Time split at", time.split)
			n.split <- length(time.split)  + 1
			model.list$trend.to.stasis <- model.test.trend.to.stasis(data.model.test = model.test_input, pool.variance = pool.variance, cl = control.list, time.split=time.split)
		}
		cat("\n", "AICc =", model.list$trend.to.stasis["AICc"])
	}
	
	## run custom models if user-supplied
	
	if(is.null(time.split) && !is.null(custom.model)) {
		warning("custom model with no time.split - custom model not run")
	}
	
	if(!is.null(custom.model) && !is.null(time.split)) {
		cat("\n", "running custom models")
		model.one <- custom.model[,1]
		model.two <- custom.model[,2]
		n.models <- length(custom.model[,1])
		custom.model.out <- lapply(1:n.models, function(x) model.test.shift.mode(data.model.test = model.test_input, time.split=time.split, mode.one=model.one[x], mode.two=model.two[x], pool.variance = pool.variance, cl=control.list))
		custom.model.names <- apply(custom.model, 1, function(x) paste0(x, collapse="_"))
		model.list <- c(model.list, custom.model.out)
		names(model.list)[-c(1:length(test.model))] <- custom.model.names
		cat(" .Done.")
	}

	## judge all models using AICc values
	aic.out <- sapply(	model.list, function(x) x["AICc"])
	names(aic.out) <- names(model.list)
	delta.aicc <- aic.out - min(aic.out)
	weight.aicc <- exp(-0.5 * delta.aicc) / sum(exp(-0.5 * delta.aicc))
	order.aicc <- order(weight.aicc, decreasing=T)
	
	if(plot.disparity) {
	
		par(mfrow=c(1, 2), mar=c(4,4,2,2), oma=c(10, 4, 4, 4))
		xAxis <- max(model.test_input[[4]]) - model.test_input[[4]]
		plot(xAxis, model.test_input[[1]], type="l", xlim=c(max(xAxis), 0), xlab="Time", ylab="central tendency", las=1)
		varUp <- model.test_input[[1]] + model.test_input[[2]]
		varDown <- model.test_input[[1]] - model.test_input[[2]]
		polygon(x=c(xAxis, rev(xAxis)), c(varUp, rev(varDown)), col="grey", border=F)
		lines(xAxis, model.test_input[[1]], col="grey50")
		abline(v=time.split, lty=2, lwd=2)
		plotcor <- barplot(weight.aicc[order.aicc], las=1, ylim=c(0, 1), col="grey30", border=F, ylab="Akaike weights", names=F)
		mtext(names(aic.out)[order.aicc], 1, las=2, at=plotcor[,1], line=1)
	}

	return.out <- list()
	return.out$aicc.models <- cbind(aic.out, delta.aicc, weight.aicc)
	
	if(return.model.full) {
		return.out$full.details <- c(model.list)
	}
	
	class(return.out) <- "dispRity.model.test"
	invisible(return.out)
}	