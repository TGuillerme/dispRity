#' @name model.test.sim
#'
#' @title Simulate Model Test
#'
#' @description Simulate models of disparity change through time
#'
#' @param sim The number of separate simulations to run.
#' @param model Either (i) the named model of evolution to simulate for changes in disparity-through-time using a homogenous or hetergenous model (see list in \code{\link{model.test}}) or (ii) an object of class \code{dispRity} returned from \code{model.test} function. If a \code{dispRity} object is supplied, all remaining arguments apart from \code{sim} and \code{model.rank} and \code{alternative} are ignored as the model specified by the input model is used.
#' @param model.rank If a \code{dispRity} object is supplied, which model is used for simulation. The rank refers to the order of models as specified by AICc, so if \code{model.rank = 1} (default) the best-fitting model is used for simulation.
#' @param alternative If the simulation is based on a \code{dispRity} object, what is the alternative hypothesis: can be \code{"two-sided"} (default), \code{"greater"} or \code{"lesser"}.
#' @param time.split The age of the change in mode. The age is measured as the time before the most recent sample, and multiple ages can be supplied in a vector. Note this only applies to heterogenous models.
#' @param time.span The length of the sequence (\code{numeric}). If one number is supplied this is treated as the length of the sequence and the time span is treated as sequence from 0 to \code{time.span} in unit increments. If a vector of length > 1 is supplied, this is treated as the the age of each sample in the sequence.
#' @param variance The variance of each sample (\code{numeric}). If one number is supplied this is the variance for all samples in the sequence. If a vector of equal length to the \code{time.span} vector is supplied, this is used for the variance of each sample in the sequence
#' @param sample.size The sample size of each sample (\code{numeric}). If one number is supplied this is the sample size for all samples in the sequence. If a vector of equal length to the \code{time.span} vector is supplied, this is used for the sample size of each sample in the sequence
#' @param parameters A \code{list} of model parameters used for simulations. See details.
#' @param fixed.optima A \code{logical} value, whether to use an estimated optimum value in OU models (\code{FALSE} - default), or whether to set the OU optimum to the ancestral value (\code{TRUE}).
#' 
#' @return A list of class \code{dispRity} and \code{model.sim}. Each list element contains the simulated central tendency, as well as the variance, sample size, and subsets used to simulate the data.
#' 
#' @details
#' The \code{parameters} is a list of arguments to be passed to the models.
#' These arguments can be:
#' \itemize{
#'      \item{\code{ancestral.state}}, ancestral value of the disparity applicable to all models (default = \code{0.01}).
#'      \item{\code{sigma.squared}}, rate of step variance to all models except Stasis (default = \code{1}).
#'      \item{\code{alpha}}, strength of attraction to the optimum in OU models (default = \code{1}).
#'      \item{\code{optima.1}}, the value of the optimum in a OU model, or the first bin optimum in a multi-OU model (default = \code{0.15}).
#'      \item{\code{optima.2}}, the second bin optimum in a multi-OU model (default = \code{0.15}).
#'      \item{\code{optima.3}}, the third bin optimum in a multi-OU model (default = \code{0.15}).
#'      \item{\code{theta.1}}, the mean in a Stasis model, or the first bin mean in a multi-Stasis model (default = \code{1}).
#'      \item{\code{theta.2}}, the second bin optimum in a multi-OU model (default = \code{1}).
#'      \item{\code{theta.3}}, the third bin optimum in a multi-OU model (default = \code{1}).
#'      \item{\code{omega}}, the variance in a Stasis model (default = \code{1}).
#'      \item{\code{trend}}, the trend parameter in the Trend model (default = \code{0.5}).
#'      \item{\code{eb.rate}}, the rate of exponential rate decrease in the EB model (default = \code{-0.1}).
#' }
#'
#' @examples
#' ## Disparity through time data
#' data(BeckLee_disparity)
#'
#' ## List of models to test
#' models <- list("Trend", "BM")
#'
#' ## Testing the models on the observed disparity
#' model_test_output <- model.test(BeckLee_disparity, models, time.split = 66)
#'  
#' ## simulations using the output from model.test
#' model_test_sim_output <- model.test.sim(sim = 1000, model= model_test_output)
#'  
#' ## Plot the simulated best model
#' plot(model_test_sim_output)
#' ## Add the observed data
#' plot(BeckLee_disparity, add = TRUE, col = c("pink", "#ff000050", "#ff000050"))
#' 
#' ## Simulating a specific model with specific parameters parameters
#' model_simulation <- model.test.sim(sim = 1000, model = "BM", time.span = 120, variance = 0.1,
#'                                    sample.size = 100, parameters = list(ancestral.state = 0,
#'                                    sigma.squared = 0.1))
#' 
#' ## Summarising the results
#' plot(model_simulation, main = "A simple Brownian motion")
#' 
#' @seealso \code{\link{model.test}}, \code{\link{model.test.wrapper}}, \code{\link{summary.dispRity}} and \code{\link{plot.dispRity}}
#'
#' @references Blomberg SP, Garland T Jr, & Ives AR. 2003. Testing for phylogenetic signal in comparative data: behavioral traits are more labile. Evolution.  \bold{57}, 717-745.
#' @references Hansen TF. 1997. Stabilizing selection and the comparative analysis of adaptation. Evolution. \bold{51}, 1341-1351.
#' @references Harmon LJ, \emph{et al}. 2010. Early bursts of body size and shape evolution are rare in comparative data. \bold{64}, 2385-2396.
#' @references Hunt G. 2006. Fitting and comparing models of phyletic evolution: random walks and beyond. Paleobiology. \bold{32}, 578-601. DOI: 10.1666/05070.1.
#' @references Hunt G, Hopkins MJ & Lidgard S. 2015. Simple versus complex models of trait evolution and stasis as a response to environmental change. Proceedings of the National Academy of Sciences. \bold{112}, 4885-4890. DOI: 10.1073/pnas.1403662111
#' @references Felsenstein J. 1973. Maximum-likelihood estimation of evolutionary trees from continuous characters. American Journal of Human Genetics. \bold{25}, 471-492.
#' @references Felsenstein J. 1985. Phylogenies and the comparative method. The American Naturalist. \bold{51}, 1-15.
#' @references Murrell DJ. 2018. A global envelope test to detect non-random bursts of trait evolution. Methods in Ecology and Evolution. DOI: 10.1111/2041-210X.13006

#' 
#' Citation for the envelope code:
# @misc{david_murrell_2018_1197535,
#  author       = {David Murrell},
#  title        = {{djmurrell/DTT-Envelope-code: Rank envelope test 
#                   for disparity through time}},
#  month        = mar,
#  year         = 2018,
#  doi          = {10.5281/zenodo.1197535},
#  url          = {https://doi.org/10.5281/zenodo.1197535}
#}
#' 
#' 
#' @importFrom mnormt rmnorm
#' @author Mark N Puttick and Thomas Guillerme
#' @export

# source("sanitizing.R")
# source("model.test_fun.R")
## Defaults
# sim = 1
# model = "BM"
# time.split = NULL
# time.span = 100
# variance = 1
# sample.size = 100
# parameters = list()
# fixed.optima = FALSE
# model.rank = 1
# alternative = "two-sided"

model.test.sim <- function(sim = 1, model, model.rank = 1, alternative = "two-sided", time.split = NULL, time.span = 100, variance = 1, sample.size = 100, parameters = list(), fixed.optima = FALSE) {
    
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
    class <- check.class(model, c("character", "dispRity"), msg = " must be either a model name (character) or a dispRity object from model.test().")
    if(class == "character") {
        ## Model is provided
        model_inherit <- FALSE
        check.method(model, c("BM", "OU", "Trend", "Stasis", "EB", "multi.OU"), msg = "model")

    } else {
        if(class(model)[[2]] != "model.test") {
            stop(paste0(as.expression(match_call$model), " must be a dispRity object output from model.test.\nTry running model.test(", as.expression(match_call$model), ") first."), call. = FALSE)
        }
        ## Model is inherited from the dispRity object
        model_inherit <- TRUE
    }

    ## Setting up the parameters names (used latter in sanitzing)
    param_names <- c("ancestral.state", "sigma.squared", "alpha", "optima.1", "theta.1", "omega", "trend", "eb.rate","optima.2", "optima.3", "theta.2", "theta.3")

    ## Check the optional arguments
    if(model_inherit) {
        ## Check if any argument is provided (but rank and sim).
        check.arg.inherit <- function(arg, default, inherit) {
            if(is.null(default)) {
                check <- !is.null(arg)
            } else {
                check <- arg != default
            }
            if(length(check) != 0 && check) {
                warning(paste0(strsplit(as.character(expression(match_call$time.split)), split = "\\$")[[1]][2], " argument ignored (inherited from ", inherit, ")."), call. = FALSE)
            }
        }
        ## Warn that arguments are ignored
        check.arg.inherit(match_call$time.split, NULL, match_call$model)
        check.arg.inherit(match_call$time.span, 100, match_call$model)
        check.arg.inherit(match_call$variance, 1, match_call$model)
        check.arg.inherit(match_call$sample.size, 100, match_call$model)
        check.arg.inherit(match_call$parameters, list(), match_call$model)
        check.arg.inherit(match_call$fixed.optima, FALSE, match_call$model)

        ## model.rank
        silent <- check.class(model.rank, c("numeric", "integer"))
        check.length(model.rank, 1, " must be the value of ranked model to simulate.")

        ##TODO: allow multiple model ranks
        
        if(model.rank > nrow(model$aic.models)) {
            stop("model.rank must be the value of ranked model to simulate.", call. = FALSE)
        }

        ## Alternative
        check.method(alternative, c("two-sided", "greater", "lesser"), msg = "alternative")
        ## Translate the h1 for GET
        if(alternative == "two-sided") {
            alternative <- "two.sided"
        }
        if(alternative == "lesser") {
            alternative <- "less"
        }


        ## Get parameters from previous models
        test.p <- TRUE
        empirical.model <- model
            
        model.rank <- order(model[[1]][,1])[model.rank]
        model.details <- model[[2]][model.rank][[1]]
        model.name <- rownames(model[[1]])[model.rank]
        parameters <- model[[2]][[model.rank]]$par
            
        time.span <- model$model.data$subsets
        variance <- model$model.data$variance
        sample.size <- model$model.data$sample_size
        data.model.test <- list(variance, sample.size, time.span)
        names(data.model.test) <- c("variance", "sample_size", "subsets")
        
        if(any(names(model.details) == "split.time")) {
            time.split <- model.details$split.time
        } else {
            time.split <- NULL
        }
        
        fixed.optima <- model$fixed.optima
        names(parameters) <- sapply(names(parameters), function(x) gsub(" ", ".", x))
        parameters <- lapply(parameters, function(x) x)
        model <- model.name
        model <- strsplit(model, ":")[[1]]
        
        # MP: necessary for the combination of 'multi.OU' and 'fixed.optima=TRUE' otherwise simulations take wrong optimum
        if(fixed.optima && model == "multi.OU") parameters$optima.1 <- parameters$ancestral.state

    } else {

        ## time.span
        check.class(time.span, c("numeric", "integer"))
        if(length(time.span) == 1) {
            time.span <- c(1:(time.span))
        }

        ## Setting the sample_length
        sample_length <- length(time.span)

        ## variance
        check.class(variance, c("numeric", "integer"))
        if (length(variance) != sample_length) {
            variance <- rep(variance, sample_length)
        }

        ## sample.size
        check.class(sample.size, c("numeric", "integer"))
        if (length(sample.size) != sample_length) {
            sample.size <- rep(sample.size, sample_length)
        }

        ## Setting the model parameters
        data.model.test <- list(variance, sample.size, time.span)
        names(data.model.test) <- c("variance", "sample_size", "subsets")

        ## time.split
        if(!is.null(time.split)) {
            silent <- check.class(time.split, c("numeric", "integer"))
            time.split <- sort(sapply(time.split, function(u) which.min(abs(u - rev(data.model.test[[3]])))))
        }

        ## parameters
        check.class(parameters, "list")
        if(!is.null(names(parameters))) {
            check.method(names(parameters), param_names, "parameters names") 
        }

        ## fixed.optima
        check.class(fixed.optima, "logical")

        ## Don't test p
        test.p <- FALSE
    }

    ## Filling default parameters
    if(is.null(parameters$ancestral.state)) {
        parameters$ancestral.state <- 1e-2
    }
    if(is.null(parameters$sigma.squared)) {
        parameters$sigma.squared <- 1
    }
    if(is.null(parameters$alpha)) {
        parameters$alpha <- 1
    }
    if(is.null(parameters$optima.1)) {
        parameters$optima.1 <- 0.15
    }
    if(is.null(parameters$theta.1)) {
        parameters$theta.1 <- 1
    }
    if(is.null(parameters$omega)) {
        parameters$omega <- 1
    }
    if(is.null(parameters$trend)) {
        parameters$trend <- 0.5
    }
    if(is.null(parameters$eb.rate)) {
        parameters$eb.rate <- -0.1
    }
    if(is.null(parameters$optima.2)) {
        parameters$optima.2 <- 0.15
    }
    if(is.null(parameters$optima.3)) {
        parameters$optima.3 <- 0.15
    }
    if(is.null(parameters$theta.2)) {
        parameters$theta.2 <- 1
    }
    if(is.null(parameters$theta.3)) {
        parameters$theta.3 <- 1
    }

    ## Reorder the parameters
    p <- unlist(parameters)[match(param_names, names(parameters))]
    
    
    # convert time.split to the closest integer in the time list object - same as is done for model.test
    if(is.numeric(time.split)) time.split <- sort(sapply(time.split, function(u) which.min(abs(u - rev(time.span)))))


    total.n <- length(time.span)
    sample.time <- 1:total.n
    split.here.vcv <- c(1, time.split)
    split.here.2.vcv <- c(time.split - 1, total.n)
    ou.mean <- NULL

    any.model <- which(model == "multi.OU")

    if(any(any.model, na.rm = TRUE)) {
        split.here.vcv <- split.here.2.vcv <- NULL
        ou.mean <- c(1, time.split, length(time.span))
        split.here.vcv <- c(1, split.here.vcv)
        split.here.2.vcv <- c(split.here.2.vcv, length(time.span))
    }

    total_VCV <- matrix(0, nrow = total.n, ncol = total.n)
    total_mean <- c()
    optima.level.ou <- optima.level.stasis <-1
    model.anc <- model.alpha <- NULL
    time.int <- 1
        
    for(rec.model in 1:length(model)) {

        time.x <- time.int #TG: time.int is always fixed to 1

        # if(time.x == 1) {
                
            data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])
            output.vcv <- est.VCV(p, data.model.test.int, model.type = model[time.x])
            output.mean <- est.mean(p, data.model.test.int, model.type = model[time.x], optima.level.ou = optima.level.ou, optima.level.stasis = optima.level.stasis, fixed.optima = fixed.optima, est.anc = TRUE, split.time = ou.mean)

            if(model[1] == "BM") {
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model[1] == "OU") {
                optima.level.ou <- optima.level.ou + 1
                est.anc <- FALSE
                model.anc <- utils::tail(output.mean, 1)
                time.int <- time.x + 1
            }
            
            if(model[1] == "Stasis") {
                optima.level.stasis <- optima.level.stasis + 1
                model.anc <- p[5]
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model[1] == "Trend" || model[1] == "EB") {
                model.anc <- utils::tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
                
        # } else {
        
        #     data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])    
        #     time.out <- data.model.test.int[[3]]
        #     time.out.diff <- diff(time.out[1:2])
        #     time.out.2 <- time.out - (min(time.out) - time.out.diff)
        #     data.model.test.int[[3]] <- time.out.2
        #     output.vcv <- est.VCV(p, data.model.test.int, model.type=model[time.x])
        #     output.mean <- est.mean(p, data.model.test.in=data.model.test.int, model.type=model[time.x], optima.level.ou= optima.level.ou, optima.level.stasis= optima.level.stasis, fixed.optima=fixed.optima, est.anc=est.anc, model.anc=model.anc, split.time=NULL)
                    
        #     if(model[time.x] == "BM") {
        #         est.anc <- FALSE
        #         model.anc <- p[1]
        #         time.int <- time.x + 1
        #     }
            
        #     if(model[time.x] == "OU") {
        #         optima.level.ou <- optima.level.ou + 1
        #         est.anc <- FALSE
        #         model.anc <- p[1]
        #         time.int <- time.x + 1
        #     }
            
        #     if(model[time.x] == "Stasis") {
        #         optima.level.stasis <- optima.level.stasis + 1
        #         model.anc <- p[5]
        #         est.anc <- FALSE
        #         time.int <- time.x + 1
        #     }
            
        #     if(model[time.x] == "Trend") {
        #         model.anc <- utils::tail(output.mean, 1)
        #         est.anc <- FALSE
        #         time.int <- time.x + 1
        #     }
            
        #     if(model[time.x] == "EB") {
        #         model.anc <- utils::tail(output.mean, 1)
        #         est.anc <- FALSE
        #         time.int <- time.x + 1
        #     }
            
        # }

        total_VCV[split.here.vcv[time.x] : (split.here.2.vcv[time.x]), split.here.vcv[time.x] : (split.here.2.vcv[time.x]) ] <- output.vcv
        total_mean <- c(total_mean, output.mean)
    
    }
    
    output.values <- t(mnormt::rmnorm(n = sim, mean = total_mean, varcov = total_VCV))
    if(dim(output.values)[1] == 1) {
        output.values <- t(output.values)
    }

    # run.one.simulation <- function(X, output.values, data.model.test) {
    #     output.full <- list(output.values[,X], data.model.test[[1]], data.model.test[[2]], data.model.test[[3]])
    #     names(output.full) <- c("central_tendency", "variance", "sample_size", "subsets")
    #     class(output.full) <- c("dispRity", "model.sim")
    #     return(output.full)
    # }

    ## Transform the rmnorm into a list
    output.simulation <- apply(output.values, 2, function(X) {return(list("central_tendency" = X))})
    # output.simulation <- lapply(1:sim, run.one.simulation, output.values, data.model.test)
    
    output <- list()
    output$simulation.data <- list("sim" = output.simulation, "fix" = list("variance" = data.model.test[[1]], "sample_size" = data.model.test[[2]], "subsets" = data.model.test[[3]]))
    # output$simulation.data <- output.simulation
    
    if(test.p) {
        x <- list()
        x$sim <- sapply(output.simulation, function(x) x$central_tendency) #DEBUG: a 120*7 matrix
        x$central_tendency <- as.numeric(empirical.model[[4]]$central_tendency) #DEBUG: a 120 vector
        x$subsets <- as.numeric(empirical.model[[4]]$subsets) #DEBUG: a 120 vector
        output$p.value <- rank_env_dtt(x, alternative)
    }

    ## Add the model call
    output$call <- match_call
    output$nsim <- sim

    ## Add the inheritence from the previous object
    if(model_inherit) {
        model_results <- summary(empirical.model)[model.rank,]
        model_results <- model_results[-c(which(is.na(model_results)), 2, 3)]
        output$model <- matrix(model_results, nrow = 1, dimnames = list(model.name, names(model_results)))
    } else {
        output$model <- match_call$model
    }

    class(output) <- c("dispRity", "model.sim")
    return(output)
}