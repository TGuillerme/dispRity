#' @name model.test.wrapper
#' @title model.test.wrapper
#'
#' @description A wrapper function for \code{\link{model.test}} to summarise relative fit of multiple models. Gives the relative fit of \code{model.test} output using log-likelihood and AICc values, as well as the Rank Envelope Test significance to elucidate if empirical data is significantly different to simulated data modelled using the estimated model parameters from \code{model.test.sim}
#'
#' @param data A \code{dispRity} object used to test models of evolution through time.
#' @param model The model(s) of evolution to allow for changes in disparity-through-time using a homogenous or hetergenous model, either using a single input or a list containing different models (See \bold{Details}). If a vector with multiple modes is supplied then the model will test for shifts in modes at the time supplied by \code{time.split}.
#' @param pool.variance If \code{NULL} (default) the difference in variances will be calculated using \code{\link[stats]{bartlett.test}} of equal variances. If there is no significant difference among variances, then variance in samples will be pooled and the same variance will be used for all samples. A significance difference will not pool variances and the original variance will be used for model-testing. If argument \code{TRUE} or \code{FALSE} are used, Bartlett's test will be ignored and the analyses will use the user-set pooling of variances.
#' @param time.split The age of the change in mode (\code{numeric}). The age is measured in positive units as the time before the most recent sample, and multiple ages can be supplied in a vector. If no age is supplied for models then all possible time shifts are fit in the model, and the highest likelihood model is returned. Note this only applies to heterogenous models (See \bold{Details}).
#' @param fixed.optima A \code{logical} value, whether to use an estimated optimum value in OU models (\code{FALSE} - default), or whether to set the OU optimum to the ancestral value (\code{TRUE}).
#' @param control.list A \code{list} of fine-tune control inputs for the optim function.
#' @param verbose \code{logical}, whether to display the model results as computed (\code{TRUE} - default).
#' @param sim The number of separate simulations.
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
#' ## models to be fit to disparity data
#' models <- list("BM", "OU", "multi.OU", "Trend")
#' 
#' ## test all models, and assess the significance of simulated data against the empirical distribution for each
#' model.test.wrapper(sim=1000, data=BeckLee_disparity, model=models, fixed.optima=TRUE, plot.sim=TRUE, time.split=66)
#' 
#' @seealso \code{\link{model.test}}, \code{\link{model.test.sim}}.
#'
#' @references Blomberg SP, Garland T Jr, & Ives AR. 2003. Testing for phylogenetic signal in comparative data: behavioral traits are more labile. Evolution.  \bold{57}, 717-745.
#' @references Hansen TF. 1997. Stabilizing selection and the comparative analysis of adaptation. Evolution. \bold{51}, 1341-1351.
#' @references Harmon LJ, \emph{et al}. 2010. Early bursts of body size and shape evolution are rare in comparative data. \bold{64}, 2385-2396.
#' @references Hunt G. 2006. Fitting and comparing models of phyletic evolution: random walks and beyond. Paleobiology. \bold{32}, 578-601. DOI: 10.1666/05070.1.
#' @references Hunt G, Hopkins MJ & Lidgard S. 2015. Simple versus complex models of trait evolution and stasis as a response to environmental change. Proceedings of the National Academy of Sciences. \bold{112}, 4885-4890. DOI: 10.1073/pnas.1403662111
#' @references Felsenstein J. 1973. Maximum-likelihood estimation of evolutionary trees from continuous characters. American Journal of Human Genetics. \bold{25}, 471-492.
#' @references Felsenstein J. 1985. Phylogenies and the comparative method. The American Naturalist. \bold{51}, 1-15.
#' @references Murrell DJ. 2018. A global envelope test to detect non-random bursts of trait evolution. Methods in Ecology and Evolution. DOI: 10.1111/2041-210X.13006
#
#' @author Mark N Puttick and Thomas Guillerme
#' @export
#' 

model.test.wrapper <- function(data, model, pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = TRUE, sim=1000, plot.sim=TRUE, col.simulation=c("grey30", "#00000020", "#00000020"), col.observed="hotpink", lwd.obs=2, show.p=FALSE) {
	
	
	match_call <- match.call()
	
	## Sanitizing
    ## sim must be a positive whole number
    silent <- check.class(sim, c("numeric", "integer"))
    check.length(sim, 1, msg = " must be the number of simulations to run.")
    sim <- round(sim)
    if(sim < 0) {
        sim <- abs(sim)
    }
	
	# run model.test
	models.out <- model.test(data = data, model = model, pool.variance = pool.variance, time.split = time.split, fixed.optima = fixed.optima, control.list = control.list, verbose = verbose)

    summary.models <- summary(models.out)
    n.models <- dim(summary.models)[1]
    outputs <- suppressWarnings(lapply(1:n.models, function(x) model.test.sim(sim, models.out, model.rank = x)))
    p.int <- t(sapply(outputs, function(u) c(u$p[[4]], u$p[[5]])))
    results <- cbind(summary.models)
    results <- results[order(results[, 2]), ]
    results <- cbind(results, p.int)
    colnames(results)[(dim(results)[2] - 2):dim(results)[2]] <- c("median p value", "lower p value", "upper p value")
    
 	if(plot.sim) {
	    n.sq <- ceiling(sqrt(n.models))
	    obs.data <- cbind(models.out$model.data$subsets, models.out$model.data$central_tendency)
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
