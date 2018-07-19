#' @name model.test.wrapper
#' @title Model test wrapper
#'
#' @description A wrapper function for \code{\link{model.test}} to perform a model fitting analysis on disparity through time data.
#' 
#' @details This function gives the relative fit of \code{model.test} output using log-likelihood and AICc values, as well as the Rank Envelope Test significance to elucidate if empirical data is significantly different to simulated data modelled using the estimated model parameters from \code{\link{model.test.sim}}. This is equivalent to running \code{test <- model.test.sim(sim = 1000, model = model.test(data, model)); summary(test) ; plot(test) ; plot(data, add = TRUE)}.
#'
#' @param data A \code{dispRity} object used to test models of evolution through time.
#' @param model The model(s) of evolution to allow for changes in disparity-through-time using a homogenous or hetergenous model, either using a single input or a list containing different models (see list in \code{\link{model.test}}). If a vector with multiple modes is supplied then the model will test for shifts in modes at the time supplied by \code{time.split}.
#' @param pool.variance If \code{NULL} (default) the difference in variances will be calculated using \code{\link[stats]{bartlett.test}} of equal variances. If there is no significant difference among variances, then variance in samples will be pooled and the same variance will be used for all samples. A significance difference will not pool variances and the original variance will be used for model-testing. If argument \code{TRUE} or \code{FALSE} are used, Bartlett's test will be ignored and the analyses will use the user-set pooling of variances.
#' @param time.split The age of the change in mode (\code{numeric}). The age is measured in positive units as the time before the most recent sample, and multiple ages can be supplied in a vector. If no age is supplied for models then all possible time shifts are fit in the model, and the highest likelihood model is returned. Note this only applies to heterogenous models (See \bold{Details}).
#' @param fixed.optima A \code{logical} value, whether to use an estimated optimum value in OU models (\code{FALSE} - default), or whether to set the OU optimum to the ancestral value (\code{TRUE}).
#' @param control.list A \code{list} of fine-tune control inputs for the optim function.
#' @param verbose \code{logical}, whether to display the model results as computed (\code{TRUE} - default).
#' @param sim The number of separate simulations (default = 1000).
#' @param plot.sim Logical. If \code{TRUE} (default) the plots of the simulated and observed disparity are returned for all models.
#' @param col.sim Colour options used for the plotting of simulated values. See \code{\link{plot.dispRity}} for more details. If missing, the default colours \code{c("black", "lightgrey", "grey")} are used.
#' @param col.obs Colour of the observed data on the plot. Default colour is \code{"hotpink"}
#' @param lwd.obs Line width of the observed value.
#' @param show.p Logical, when \code{plot.sim = TRUE}, whether to display the p-value of rank envelope tests (\code{TRUE}) or not (\code{FALSE} - default).
#' @param cex.p A numerical value for the the font size of the displayed p-value (if \code{show.p = TRUE}). If missing, the value is set to 1.
#' @param legend Logical, when \code{plot.sim = TRUE}, whether to display the legend in the first panel (\code{TRUE}) or not (\code{FALSE} - default).
#' @param ... Any additional arguments to be passed to \code{\link{plot.dispRity}} or \code{\link{summary.dispRity}}.
#'
#' @return A matrix with the relative fit, parameter values, and Rank Envelope test p values for each model, and a plot of simulated data from each model alongside observed data for each model if plot.sim is \code{TRUE} 
#'
#' @examples
#' 
#' \dontrun{
#' ## Mammal disparity through time
#' data(BeckLee_disparity)
#' 
#' ## The models to be fit to disparity data
#' models <- list("BM", "OU", "multi.OU", "Trend")
#' 
#' ## test all models, and assess the significance of simulated data
#' ## against the empirical distribution for each
#' model.test.wrapper(data = BeckLee_disparity, model = models, fixed.optima = TRUE,
#'                    time.split = 66, show.p = TRUE)
#' }
#' 
#' @seealso \code{\link{model.test}}, \code{\link{model.test.sim}}, \code{\link{summary.dispRity}} and \code{\link{plot.dispRity}}
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

# DEBUG
# data(BeckLee_disparity)
# data = BeckLee_disparity
# model <- list("BM", "OU", "multi.OU", "Trend")
# fixed.optima = TRUE
# time.split = 66
# show.p = TRUE
# pool.variance = NULL
# control.list = list(fnscale = -1)
# verbose = TRUE
# sim = 1000
# plot.sim = TRUE
# col.sim = c("grey30", "#00000020", "#00000020")
# col.obs = "hotpink"
#  lwd.obs = 2
#  show.p = FALSE
# model.test.wrapper(data = BeckLee_disparity, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, legend = TRUE)

model.test.wrapper <- function(data, model, pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = TRUE, sim = 1000, plot.sim = TRUE, col.sim, col.obs = "hotpink", lwd.obs = 2, show.p = FALSE, cex.p, legend = FALSE, ...) {
	
	match_call <- match.call()
	
	## Sanitizing
    ## sim must be a positive whole number
    silent <- check.class(sim, c("numeric", "integer"))
    check.length(sim, 1, msg = " must be the number of simulations to run.")
    sim <- round(sim)
    if(sim < 0) {
        sim <- abs(sim)
    }
    ## The rest of model.test arguments are tested by the model.test functions

    check.class(plot.sim, "logical")
    if(!missing(col.sim)) {check.class(col.sim, "character")}
    check.class(col.obs, "character")
    silent <- check.class(lwd.obs, c("numeric", "integer"))
    check.length(lwd.obs, 1, msg = " must be a single numeric value for the observed central tendency line plot thickness.", errorif = FALSE)
    check.class(show.p, "logical")
    if(show.p) {
        if(missing(cex.p)) {
            cex.p <- 1
        } else {
            check.class(cex.p, "numeric")
        }
    }

	## Run model.test
	models.out <- model.test(data = data, model = model, pool.variance = pool.variance, time.split = time.split, fixed.optima = fixed.optima, control.list = control.list, verbose = verbose)

    ## Summarise the models
    summary.models <- summary(models.out)
    n.models <- dim(summary.models)[1]
    outputs <- suppressWarnings(lapply(1:n.models, function(x) model.test.sim(sim, model = models.out, model.rank = x)))
    p.int <- t(sapply(outputs, function(u) c(u$p[[4]], u$p[[5]])))
    results <- cbind(summary.models)
    results <- results[order(results[, 2]), ]
    # MP: allow a single model to be used as input for model.test.wrapper (may be a bit pointless but prevents an error)
    if(class(results)[1] == "matrix") {
    	results <- cbind(results, p.int)
    	colnames(results)[(dim(results)[2] - 2):dim(results)[2]] <- c("median p value", "lower p value", "upper p value")
    } else {
    	name.res <- names(results)
    	results <- matrix(c(results, p.int), nrow = 1, dimnames = list("", c(name.res, "median p value", "lower p value", "upper p value")))	
    }
    
    
 	if(plot.sim) {

        ## Handling optional arguments
        dots <- list(...)
        if(is.null(dots$xlab)) {
            if(data$call$subsets[1] == "continuous") {
                dots$xlab <- "Time (Mya)"
            } else {
                dots$xlab <- "Subsets"
            }
        }
        if(is.null(dots$ylab)) {
            dots$ylab <- as.character(as.expression(data$call$disparity$metrics[[1]]))
        }
        ## Colours
        if(missing(col.sim)) {
            dots$col <- c("black", "lightgrey", "grey")
        } else {
            dots$col <- col.sim
        }

	    obs.data <- cbind(models.out$model.data$subsets, models.out$model.data$central_tendency)
	    max.range <- range(c(sapply(outputs, function(x) range(unlist(x[[1]]$sim)))), obs.data[,2])
	    op <- par(mfrow = c(ceiling(sqrt(n.models)), round(sqrt(n.models))))
	    
	    for(one_model in 1:n.models) {

            ## Plot the simualted model
	    	plot(outputs[[one_model]], ylim = max.range, col = dots$col, xlab = dots$xlab, ylab = dots$ylab, main = paste0(rownames(results)[one_model], " (Delta aicc = ", round(results[one_model,2], digits = 2) ,")"))

            ## Plot the observed data
	    	lines(obs.data[,2], col = col.obs, lwd = lwd.obs)
	    	
            ## Add the legend
            if(legend && one_model == 1) {
                legend("topleft", c("Simulated", "Observed"), lty = 1, lwd = lwd.obs, col = c(dots$col[1], col.obs))
            }

	    	if(show.p) {
                legend("bottomleft", paste0("Rank Env. Test, p = ", round( p.int[one_model, 2], 3), ":", round( p.int[one_model, 3], 3)), cex = cex.p)
	    	}
	    }
        ## Reset default
        par(op)
    }
	
	return(results)
}
