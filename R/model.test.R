#' @name model.test
#'
#' @title Model Test
#'
#' @description Fit models of disparity change through time
#'
#' @param data A \code{dispRity} object used to test models of evolution through time.
#' @param model The model(s) of evolution to allow for changes in disparity-through-time using a homogenous or hetergenous model, either using a single input or a list containing different models (See \bold{Details}). If a vector with multiple modes is supplied then the model will test for shifts in modes at the time supplied by \code{time.split}.
#' @param pool.variance If \code{NULL} (default) the difference in variances will be calculated using \code{\link[stats]{bartlett.test}} of equal variances. If there is no significant difference among variances, then variance in samples will be pooled and the same variance will be used for all samples. A significance difference will not pool variances and the original variance will be used for model-testing. If argument \code{TRUE} or \code{FALSE} are used, Bartlett's test will be ignored and the analyses will use the user-set pooling of variances.
#' @param time.split The age of the change in mode (\code{numeric}). The age is measured in positive units as the time before the most recent sample, and multiple ages can be supplied in a vector. If no age is supplied for models then all possible time shifts are fit in the model, and the highest likelihood model is returned. Note this only applies to heterogenous models (See \bold{Details}).
#' @param fixed.optima A \code{logical} value, whether to use an estimated optimum value in OU models (\code{FALSE} - default), or whether to set the OU optimum to the ancestral value (\code{TRUE}).
#' @param control.list A \code{list} of fine-tune control inputs for the \code{\link[stats]{optim}} function.
#' @param verbose \code{logical}, whether to display the model results while they are computed (\code{TRUE} - default).
#' 
#' @details The models are fit using maximum likelihood optimisation using the function optim. Fine-tuning of the search algorithms can be applied using the \code{control.list} argument. Models can be fit using a homogenous model with the same process applied to the entire sequence or models with time splits that represent a change in parameters or a shift in mode. When a heterogeneous and/or a time-shift model is specified with a specified \code{time.split} then the shift is tested at that value only. If no time shift is supplied then multiple shift times are tested, with all bins that allow for at least 10 bins either side of the split. If the entire sample is fewer than 30 samples long then no time splits are searched for (unless a time split is supplied by the user). Parameters are shared across different modes. For example, \code{c("BM", "OU")} would fit a model in which the process starts with a BM model and shifts to an OU process. The ancestral value at the start of the sequence and sigma squared value are shared across the models. Any combination of the following homogenous models (with the exception of \code{"multi.OU"}) can be fit to the data:
#' 
#' \itemize{
#'         \item{BM}{ Fits a unbiased random walk model of Brownian motion evolution (Felsenstein 1973; 1985; Hunt 2006). The model optimises the ancestral state and the 'step-variance' (sigma-squared)}
#'  
#'         \item{OU}{ The Ornstein-Uhlenbeck model of evolution in which the change in variance is constrained to an optimum value (Hansen 1997). In this model there are three parameters: optima, alpha, and ancestral state. The strength of attraction based on the parameter alpha and the ancestral state is estimated from the data. The optima value is estimated from the data, and this can lead to optima being found outside the known data values, and thus the model can resemble a trend. If the argument \code{fixed.optima = TRUE}, the model will not estimate optima but constrain it to the first (ancestral) value in the sequence as is done in phylogenetic OU models}
#' 
#'         \item{Trend}{ Fits a Brownian motion model with a directional component. This model is also known as the General Random Walk (Hunt 2006). This model has three parameters: the ancestral state, the 'step-variance' (sigma-squared), and the positive or negative trend.}
#' 
#'         \item{Stasis}{ Fits a model in which traits evolve with variance (omega) around a mean (theta). This model is time-independent in that the model is guided only by the variance and attraction to the mean (Hunt 2006)}
#' 
#'         \item{EB}{ Early-Burst. Trait variance accumulates early in the evolution of a trait and decreases exponentially through time (Blomberg et al. 2003; Harmon et al. 2010). This model has three parameters: ancestral state, sigma-squared, and the exponential rate of decrease. Note this model expects the mean to remain unchanged through the model, so does not explicitly model a rapid change to a new mean or optimum value.}
#' 
#'         \item{multi.OU}{ Fits a model in which the value of the optima shifts at one or more time splits. The values of the 'step-variance' (sigma squared) and attraction to the optima (alpha) are shared across all the samples. This model can not be fit with other models - the multi.OU system can be fit to the model only}
#' }
#' 
#' @return A list of class \code{dispRity} and \code{model.test} that can be plotted and summarised via \code{\link{summary.dispRity}} and \code{\link{plot.dispRity}}.
#' The list is composed of:
#' \itemize{
#' 	\item{$aic.models}{ summary for each model's small sample Akaike Information Criterion (AICc), delta AICc, and AICc weight}
#' 	\item{$full.models}{ the list of the full models outputs from \code{\link{optim}} with the estimated parameters, log-likelihood, convergence statistics, and the split.time if applicable }
#' 	\item{$call}{ the model input}
#' 	\item{$models.data}{ input data used by the model(s)}
#' 	\item{$fixed.optima}{ Logical indicating whether a fixed optima was assumed for OU model(s)}
#' }
#' 
#' @examples
#' 
#' \dontrun{
#' ## Mammal disparity through time
#' data(BeckLee_disparity)
#' 
#' ## The four models to fit
#' models <- list("BM", "OU", "multi.OU", c("BM", "OU"))
#' 
#' ## Fitting the four models to the disparity data
#' tests <- model.test(BeckLee_disparity, models, time.split = 66)
#' 
#' ## Summarising the models
#' summary(tests)
#' 
#' ## Plotting only the models support
#' plot(tests)
#' }
#' 
#' @seealso \code{\link{model.test.wrapper}}, \code{\link{model.test.sim}}, \code{\link{summary.dispRity}} and \code{\link{plot.dispRity}}
#' 
#' @references Blomberg SP, Garland T Jr, & Ives AR. 2003. Testing for phylogenetic signal in comparative data: behavioral traits are more labile. Evolution. \bold{57}, 717-745.
#' @references Hansen TF. 1997. Stabilizing selection and the comparative analysis of adaptation. Evolution. \bold{51}, 1341-1351.
#' @references Harmon LJ, \emph{et al}. 2010. Early bursts of body size and shape evolution are rare in comparative data. \bold{64}, 2385-2396.
#' @references Hunt G. 2006. Fitting and comparing models of phyletic evolution: random walks and beyond. Paleobiology. \bold{32}, 578-601. DOI: 10.1666/05070.1.
#' @references Hunt G, Hopkins MJ & Lidgard S. 2015. Simple versus complex models of trait evolution and stasis as a response to environmental change. Proceedings of the National Academy of Sciences. \bold{112}, 4885-4890. DOI: 10.1073/pnas.1403662111
#' @references Felsenstein J. 1973. Maximum-likelihood estimation of evolutionary trees from continuous characters. American Journal of Human Genetics. \bold{25}, 471-492.
#' @references Felsenstein J. 1985. Phylogenies and the comparative method. The American Naturalist. \bold{51}, 1-15.
#' @references Murrell DJ. 2018. A global envelope test to detect non-random bursts of trait evolution. Methods in Ecology and Evolution. DOI: 10.1111/2041-210X.13006
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export
#' 

# warning("DEBUG model.test")
# source("sanitizing.R")
# source("model.test_fun.R")
# set.seed(123)
# data(BeckLee_mat99)
# data(BeckLee_ages)
# data(BeckLee_tree)
# continuousData <- time.subsets(BeckLee_mat99, BeckLee_tree, method="continuous", time=seq(120, 0, length.out=120), model="proximity")
# data_bootstrapped <- boot.matrix(continuousData)
# data <- dispRity(data_bootstrapped, c(sum, variances))
# models <- list("BM", "OU", "multi.OU", c("BM", "Trend"))
# pool.variance=NULL
# fixed.optima=FALSE
# verbose = TRUE
# control.list=list(fnscale = -1)
# time.split <- 66
# test <- model.test(data, models, time.split = 66)

    # data = data
    # model = models
    # pool.variance = NULL
    # time.split=66
    # fixed.optima = TRUE
    # control.list = list(fnscale = -1)
    # verbose = TRUE

model.test <- function(data, model, pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = TRUE) {
    
    match_call <- match.call()

    ## data
    check.class(data, "dispRity")
    model_test_input <- select.model.list(data)

    ## models
    
    # MP: allow a single 'multi-mode' model to be used as an input without an error - could this be incorporated into an existing function, or is it ok here as it'll only be used once?
    # TG: Good call, I've modified check.method so that the condition is now explicit (all or any)
        
    check.method(unlist(model), c("BM", "OU", "Trend", "Stasis", "EB", "multi.OU"), msg = "model", condition = any)
    n_models <- length(model)

    ## pool.variance
    if(!is.null(pool.variance)) {
        check.class(pool.variance, "logical")
    }

    ## time.split
    if(!missing(time.split) && !is.null(time.split)) {
        silent <- check.class(time.split, c("numeric", "integer"))
    } else {
        time.split <- NULL
    }

    ## fixed optima
    check.class(fixed.optima, "logical")

    ## control.list
    check.class(control.list, "list")
        
    ## use Bartlett's test of variance to decide whether to pool variance or not (not used if pool variance is specified as TRUE or FALSE before-hand)
    if(is.null(pool.variance)) {
        p_test <- stats::bartlett.test(model_test_input)$p.value
        if(p_test < 0.05) {
            pool.variance <- FALSE
            if(verbose) cat(paste0("Evidence of equal variance (Bartlett's test of equal variances p = ", round(p_test, 3), ").\nVariance is not pooled.\n"))
        } else {
            pool.variance <- TRUE
            if(verbose) cat(paste0("Evidence of unequal variance (Bartlett's test of equal variances p = ", round(p_test, 3), ").\nVariance is pooled.\n"))
        }
    } 
    ## Pool the variance (or not)
    if(pool.variance) {
        model_test_input <- pooled.variance(model_test_input, TRUE)
    }
    
    models_out <- lapply(1:n_models, function(model_n) {
        model.return <- NULL
        model.type <- model[[model_n]]
        
        if(length(model.type) == 1 && model.type != "multi.OU") {
            time.split <- NULL
        }
        
        if((is.null(time.split) && length(model.type) == 2) || (is.null(time.split) && model.type == "multi.OU")) {
            
            all.times <- max(model_test_input[[4]]) - model_test_input[[4]]                

            if(length(all.times) > 31) {
                
                ten.times <- all.times[(9: (length(all.times) - 11))]
                run.time.split <- TRUE

                if(verbose) cat(paste0("Running ",  paste0(model.type, collapse=":") ," on ", length(ten.times), " shift times...\n"))

                model.test.all.times <- lapply(ten.times, function(x) {
                        
                    if(verbose) cat(paste0("    model ", match(x, ten.times), " of ", length(ten.times), " at ", signif(x, 3), "\n"))
                    model.test.lik(model_test_input, model.type, time.split=x, control.list, fixed.optima=fixed.optima)
                })
                
                best.model <- which.max(sapply(model.test.all.times, function(x) x[[2]]))
                model.return <- model.test.all.times[best.model][[1]]    
                model.return$split.time <- ten.times[best.model]
                
                if(verbose) {
                    cat(paste0("    best split time found at ", signif(ten.times[best.model],4), "\n"))
                    cat(paste0("Done. Best log-likelihood = ", round(model.return$value, digits = 3), " at ",  signif(ten.times[best.model],4), ".\n"))
                
                }

            } else {

                ## Can't run multi.OU
                stop.call("", "Fewer than 30 samples are available.\nThe \"multi.OU\" model cannot be run with time.split = NULL.")
            }
        
        } else {
        
            if(verbose) cat(paste0("Running ", paste(model.type, collapse = ":"), " model..."))
            model.return <- model.test.lik(model.test_input = model_test_input, model.type.in = model.type, time.split, control.list, fixed.optima=fixed.optima)
            if(length(model.type) || model.type == "multi.OU") {
                    model.return$split.time <- time.split
                }
            if(verbose) cat(paste0("Done. Log-likelihood = ", round(model.return$value, digits = 3), "\n"))
        }
        return(model.return)
    })

    model_names <- sapply(model, function(x) paste(x, collapse = ":"))

    if(any(sapply(models_out, is.null))) {
        drop.model <- which(sapply(models_out, is.null))
        models_out <- models_out[-drop.model]
        model_names <- model_names[-drop.model]
        }

    ## judge all models using AICc values
    ## Calculate the models AIC and AICc
    
    model.len <- sapply(models_out, function(x) length(x[[2]])) - 1
    model_parameters <- model.len + sapply(models_out, function(x) length(x[[1]]))
    model_likelihoods <- unlist(sapply(models_out, function(x) x[2]))
    sample_size <- length(model_test_input[[1]])
    aic <- (-2 * model_likelihoods) + (2 * model_parameters)
    aicc <- (-2 * model_likelihoods) + (2 * model_parameters) * (sample_size / ( sample_size - model_parameters - 1))
    delta_aicc <- aicc - min(aicc)
    weight_aicc <- exp(-0.5 * delta_aicc) / sum(exp(-0.5 * delta_aicc))

    ## Get the model names
    names(weight_aicc) <- names(delta_aicc) <- names(aicc) <- names(aic) <- names(models_out) <- model_names
    
    ## Generate the output format
    output <- list("aic.models" = cbind(aicc, delta_aicc, weight_aicc), "full.details" = models_out, "call" = match_call, "model.data" = model_test_input, "fixed.optima" = fixed.optima)
    class(output) <- c("dispRity", "model.test")
    return(output)
}    
