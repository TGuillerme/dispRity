#' @title Model Test
#'
#' @description Fit models of disparity change through time
#'
#' @param data A \code{dispRity} object used to test models of evolution through time.
#' @param models A \code{list} of models for changes in disparity-through-time (see the currently implemented models in \code{\link{dipRity.models}} and the \code{details} below).
#' @param pool.variance Either \code{NULL} (default) to use a Bartlett Test (\code{\link[stats]{bartlett.test}}) of equal variance. If there is no significant difference variances, then variance in samples will be pooled and the same variance will be used for all samples. Else a \code{logical} value to force the variance to be pooled or not.
#' @param time.shifts A \code{list} of ages for eventual changes in mode of evolution to be passed to each model (default is \code{NULL} for no changes).
# 
#TG: Need to reimplement this bit!
# If no age is supplied for models then all possible time shifts are fit in the model, and the highest likelihood model is returned. Note this only applies to heterogenous models (See \bold{Details}) 
# 
#' @param return.raw.models \code{logical}, whether to return the full parameter values from each tested model (\code{TRUE}) or just the comparisons between the models (\code{FALSE} - default). Note that the later object can be plot using \code{\link{plot.dispRity}}.
#' @param fixed.optima \code{logical}, whether to use an estimated optimum value in the \code{\link{OU}} or \code{\link{Stasis}} models (\code{FALSE}; default), or to set the optimum to the ancestral value (\code{TRUE}).
#' @param control.list A fine-tune control \code{list} of inputs to be passed to \code{\link[stats]{optim}}.
#' @param verbose A \code{logical} value indicating whether to be verbose or not (default is \code{TRUE}).
#' @param ... Any additional argument to be passed to the models.
#' 
#' @details The models are fit using maximum likelihood optimisation using the function \code{\link[stats]{optim}} Fine-tuning of the search algorithms can be applied using the \code{control.list} argument. Models can be fit using a homogenous model with the same process applied to the entire sequence or models with time splits that represent a change in parameters or a shift in mode. For time split models if a time value is provided, then the shift is tested at that value only. If no time shift is supplied then all shift times that allow for there to be at least 10 samples in each time bin are tested. If the sample is fewer than 30 samples long then no time splits are searched for (unless a time split is supplied by the user). The following homogenous models can be fit to the data.
#'
#' @examples
#' ## Loading data
#' data(disparity)
#' 
#' ## Testing which model fits the disparity curve
#' model.test(disparity, models = list(BM, OU, EB, Stasis, Trend))
#' 
#' @seealso \code{\link{model.test.sim}}.
#'
#' @references
#' To Add: Hunt 2006, Hunt 2008, Harmon 2010, 
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export



# DEBUG
# source("sanitizing.R")
# source("model.test_fun.R")
# source("dispRity.models.R")
# source("dispRity.models_fun.R")
# data(BeckLee_mat99) ; data(BeckLee_ages) ; data(BeckLee_tree)
# data_bootstrapped <- boot.matrix(time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous", rev(seq(from = 0, to = 120, by = 5)), model = "gradual"))
# data <- dispRity(data_bootstrapped, c(sum, variances))

# models <- list(BM, OU, EB, Stasis, Trend)#, c(BM,OU), c(OU,OU,OU))

# get.call <- function(data, models, ...) {
#     match_call <- match.call()
#     return(match_call)
# }
# match_call <- get.call(1, models = list(BM, OU, EB, Stasis, Trend))#, c(BM,OU), c(OU,OU,OU)))

# time.shifts <- NULL #list(NULL, NULL, 65, c(30,90))
# pool.variance <- NULL
# return.model.full <- FALSE
# plot.disparity <- TRUE
# fixed.optima <- FALSE
# control.list <- list(fnscale = -1)
# verbose <- TRUE


# model.test <- function(data, named.model = c("BM", "OU", "Stasis", "EB", "Trend", "multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "BM.to.Stasis", "BM.to.OU", "OU.to.BM", "OU.to.Trend", "OU.to.EB", "Stasis.to.BM", "Stasis.to.EB", "Stasis.to.Trend", "Trend.to.OU", "Trend.to.Stasis"), custom.model = NULL, pool.variance = NULL, time.shifts = NULL, return.model.full = FALSE, plot.disparity = TRUE, fixed.optima = FALSE, control.list = list(fnscale = -1)) {

model.test <- function(data, models, pool.variance = NULL, time.shifts = NULL, return.raw.models = FALSE, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE, ...) {
    
    match_call <- match.call()

    ## Data 
    check.class(data, "dispRity")
    ## convert dispRity to model.test object
    model_test_input <- select.model.list(data)

    ## Model
    #~~~~~~
    #TG: For modularity and architecture (speeding time) reasons, I've merged the named.model and custome.model arguments into models. Users can now write their own models (or modify yours) and run the thing by passing a list of models for example: models = list(BM, OU, c(OU,OU,OU), c(BM, OU)) will run a BM, and OU, a multi OU (with two shifts) and BM.to.OU. This way, it would allow users to run for example c(OU, BM, OU, BM, myModel) for example for a model with 4 shifts equivalent to BM.to.OU.to.BM.to.OU.to.myModel (where myModel is, say, a model straight from the user's imagination!).
    #~~~~~~
    ## Checking the number of models
    if(class(models) == "function") {
        one_model <- TRUE
        warning("Only one model is tested, no comparison will be available.")
    } else {
        check.class(models, "list")
        if(length(models) > 1) {
            one_model <- FALSE
        } else {
            one_model <- TRUE
            warning("Only one model is tested, no comparison will be available.")
        }
        ## Length of each model (i.e. number of shifts per model)
        model_lengths <- as.vector(unlist(lapply(models, length)))
    }

    ## Checking if all models are functions
    if(any(unlist(lapply(unlist(models), class)) != "function")) {
        stop("The models provided must be functions.")
    }

    ## Transforming all models into lists (i.e. simple models)
    to_list <- which(unlist(lapply(models, class)) != "list")
    if(length(to_list) > 0) {
        for(listing in 1:length(to_list)) {
            models[to_list[listing]] <- list(models[to_list[listing]])
        }
    }

    ## Checking time.shifts (if at least one model has one shift)
    any_shift_model <- any(model_lengths > 1)

    if(any_shift_model) {
        if(class(time.shifts) == "list") {
            check.length(time.shifts, length(models), msg = "must be a unique numeric value or a list of the same length as the models argument.", errorif = FALSE)
            ## All arguments must be numeric or null
            if(!all(unlist(lapply(time.shifts, class)) %in% c("NULL", "numeric"))) {
                stop("time.shifts list must contain only NULL or \"numeric\" values.")
            }
        } else {
            ## Error if some models need more than one shift
            if(any(model_lengths - 1 > length(time.shifts))) {
                stop("The number of time.shifts doesn't match the number of model shifts.")
            }
            ## time.shift is a unique numeric value, this won't work if some models have more than one shift!
            check.class(time.shifts, "numeric", msg = "must be a unique numeric value or a list of the same length as the models argument.")

            ## Transform the time.shifts into a list (for consistency)
            time.shifts <- lapply(as.list(model_lengths-1), function(x, time.shifts) if(x == 0) {return(NULL)} else {return(time.shifts[1:x])}, time.shifts)
        }
        ## Check if the time shifts exist in the subsamples!
        non_matching_subsamples <- is.na(match(unique(unlist(time.shifts)), model_test_input$subsamples))
        if(any(non_matching_subsamples)) {
            ## Stop if no matching (detects singular or plural)
            stop(paste0(ifelse(length(which(non_matching_subsamples)) == 1, "Subsample ", "Subsamples "), paste(unique(unlist(time.shifts))[non_matching_subsamples]), collapse = ", "), " don't match with any available subsample name in the data.")
        }
    }
    

    ## Getting the true model names (regardless of user input)
    true_model_names <- get.models.names(match_call, time.shifts)

    ## Naming the models (if necessary)
    if(is.null(names(models))) {
        names(models) <- true_model_names
    }

    ## Adding the model name component
    for(model in 1:length(models)) {
        models[[model]]$name <- true_model_names[model]
    }

    ## Checking verbose
    check.class(verbose, "logical")

    ## Checking pool.variance
    if(!is.null(pool.variance)) {
        check.class(pool.variance, "logical")
    } else {
        ## use Bartlett's test of variance to decide whether to pool variance or not (not used if pool variance is specified as TRUE or FALSE before-hand)
        p_test <- bartlett.variance(model_test_input)
        if(p_test < 0.05) {
            pool.variance <- FALSE
            if(verbose) cat("Evidence of unequal variance, Bartlett's test of equal variances (p = ", signif(p_test, 3), ").\nVariance is not pooled.\n", sep = "")
        } else {
            pool.variance <- TRUE
            if(verbose) cat("Evidence of equal variance, Bartlett's test of equal variances (p = ", signif(p_test, 3), ").\nVariance is pooled.\n", sep = "")
        }
    }
    ## Pool the variance if necessary
    if(pool.variance) {
        model_test_input <- pooled.variance(model_test_input, TRUE) #TG: why is the variance rescaled here and not in the bartlett.variance function? If ideally we'd like to recycle this pooled.variance function from the bartlett.variance function if p_test < 0.05 (rather than recalculate it).
    }

    ## Checking return.raw.models
    check.class(return.raw.models, "logical")

    ## Checking fixed.optima
    check.class(fixed.optima, "logical")

    ## Checking control.list
    check.class(control.list, "list")

    #~~~~~~~
    #TG: I've changed the bit where you test the number of changes by simply using the provided time.shifts list. Basically a model defined as c(OU, OU) is a multi OU with a shift passed through the time.shifts argument. It can also be something applied to a c(OU, BM) for a shift from OU to BM (at the same time.shifts). If the model contains more shifts (e.g. c(OU, OU, OU) - two time shifts here), the time.shifts argument should have two numeric arguments. The following bit now only checks the amount of data available between each shift. I've set the minimum to 3 but tell me if we should up it to 10!
    #~~~~~~~

    ## Checking the number of subsamples per regimes (between shifts)    
    if(any_shift_model) {

        ## Set up the minimum of subsamples
        minimum_subsamples <- 3

        ## Check the number of elements per list
        check_shifts <- lapply(lapply(time.shifts, check.shift.length, model_test_input$subsamples), unlist)

        ## Check which shifts are wrong
        wrong_shifts <- unlist(lapply(check_shifts, function(x, min) if(!is.null(x)) {return(ifelse(any(x < min), TRUE, FALSE))} else {return(FALSE)}, minimum_subsamples))

        ## Send a warning message if any is below the minimum (detects plurals)
        if(any(wrong_shifts)) {
            warning(paste0(ifelse(length(which(wrong_shifts)) == 1, "Model ", "Models "), paste(names(models)[wrong_shifts], collapse = ", "), ifelse(length(which(wrong_shifts)) == 1, " has ", " have "), "not enough subsamples for at least one shift."))
        }

        # if(length(all_times) > 31) {
        #     ten_times <- 9 : (length(all_times) - 11)
        #     run_time_split <- TRUE
        #     cat("Time split models will be tested on", length(ten_times), "shift times")
        # } else {
        #     warning("fewer than 30 samples - time split models not run")
        #     run_time_split <- FALSE
        # }
    
        ## Adding the time.shifts components
        for(model in 1:length(models)) {
            models[[model]]$shift <- time.shifts[model]
        }
    }

    ## run models
    model_out_list <- lapply(models, lapply.model.test, data.model.test = model_test_input, pool.variance = pool.variance, control.list = control.list, fixed.optima = fixed.optima, verbose = verbose, ...)
    # model_out_list <- lapply(models, lapply.model.test, data.model.test = model_test_input, pool.variance = pool.variance, control.list = control.list, fixed.optima = fixed.optima, verbose = verbose) ; warning("DEBUG model.test")


    
    # ## For multi-mode and shift models if the user supplies a shift time a priori only that shift time is test. If shift time is not supplied, the fit of a shift is tested at all time points and returns the model with the highest likelihood
    
    # if(any(try.models == "multiOU")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "multiOU model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.ou <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             #flush.console()
    #             model.test.ou(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x], n.optima=2, fixed.optima=fixed.optima)
    #         })
            
    #         cat(". Finished.")
    #         high.mod.ou <- which(model.test.all.ou[1, ] == max(model.test.all.ou[1, ]))
    #         out.best.model.ou <- model.test.all.ou[ , high.mod.ou]
    #         model_list$multi.ou <- out.best.model.ou
    #     }
        
    #     ## if time.shifts supplied, test that time only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "multiOU model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$multi.ou <- model.test.ou(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts, n.optima=n.split, fixed.optima=fixed.optima)
    #     }    
    #     cat("\n", "AICc =", model_list$multi.ou["AICc"])    
    # }
    
    # if(any(try.models == "multiStasis")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "multiStasis model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.stasis <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.stasis(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x], n.optima=2)
    #         })
            
    #         cat(". Finished.")
    #         high.mod.stasis <- which(model.test.all.stasis[1, ] == max(model.test.all.stasis[1, ]))
    #         out.best.model.stasis <- model.test.all.stasis[ , high.mod.stasis]
    #         model_list$multi.stasis <- out.best.model.stasis
    #     }
        
    #     ## if time.shifts supplied, test that time only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "multiStasis model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$multi.stasis <- model.test.stasis(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts, n.optima=n.split)
    #     }
    #     cat("\n", "AICc =", model_list$multi.stasis["AICc"])    
    # }

    # if(any(try.models == "BM.to.Trend")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "BM.to.Trend model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.bm.to.trend <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.bm.to.trend(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
            
    #         cat(". Finished.")
    #         high.mod.bm.to.trend <- which(model.test.all.bm.to.trend[1, ] == max(model.test.all.bm.to.trend[1, ]))
    #         out.best.model.bm.to.trend <- model.test.all.bm.to.trend[ , high.mod.bm.to.trend]
    #         model_list$bm.to.trend <- out.best.model.bm.to.trend
    #     }
        
    #     ## if time.shifts supplied, test that time only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "BM.to.Trend model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$bm.to.trend <- model.test.bm.to.trend(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$bm.to.trend["AICc"])    
    # }
    
    # if(any(try.models == "BM.to.EB")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "BM.to.EB model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.bm.to.eb <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.bm.to.eb(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
            
    #         cat(". Finished.")
    #         high.mod.bm.to.eb <- which(model.test.all.bm.to.eb[1, ] == max(model.test.all.bm.to.eb[1, ]))
    #         out.best.model.bm.to.eb <- model.test.all.bm.to.eb[ , high.mod.bm.to.eb]
    #         model_list$bm.to.eb <- out.best.model.bm.to.eb
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "BM.to.EB model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$bm.to.eb <- model.test.bm.to.eb(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$bm.to.eb["AICc"])
    # }
    
    # if(any(try.models == "BM.to.Stasis")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "BM.to.Stasis model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.bm.to.stasis <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.bm.to.stasis(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
            
    #         cat(". Finished.")
    #         high.mod.bm.to.stasis <- which(model.test.all.bm.to.stasis[1, ] == max(model.test.all.bm.to.stasis[1, ]))
    #         out.best.model.bm.to.stasis <- model.test.all.bm.to.stasis[ , high.mod.bm.to.stasis]
    #         model_list$bm.to.stasis <- out.best.model.bm.to.stasis
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "BM.to.Stasis model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$bm.to.stasis <- model.test.bm.to.stasis(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$bm.to.stasis["AICc"])
    # }
    
    # if(any(try.models == "BM.to.OU")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "BM.to.OU model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.bm.to.ou <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.bm.to.ou(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x], fixed.optima = fixed.optima)
    #         })
            
    #         cat(". Finished.")
    #         high.mod.bm.to.ou <- which(model.test.all.bm.to.ou[1, ] == max(model.test.all.bm.to.ou[1, ]))
    #         out.best.model.bm.to.ou <- model.test.all.bm.to.ou[ , high.mod.bm.to.ou]
    #         model_list$bm.to.ou <- out.best.model.bm.to.ou
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "BM.to.OU model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$bm.to.ou <- model.test.bm.to.ou(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts, fixed.optima = fixed.optima)
    #     }
    #     cat("\n", "AICc =", model_list$bm.to.ou["AICc"])
    # }
    
    # if(any(try.models == "OU.to.BM")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "OU.to.BM model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.ou.to.bm <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.ou.to.bm(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x], fixed.optima = fixed.optima)
    #         })
            
    #         cat(". Finished.")
    #         high.mod.ou.to.bm <- which(model.test.all.ou.to.bm[1, ] == max(model.test.all.ou.to.bm[1, ]))
    #         out.best.model.ou.to.bm <- model.test.all.ou.to.bm[ , high.mod.ou.to.bm]
    #         model_list$ou.to.bm <- out.best.model.ou.to.bm
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "OU.to.BM model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$ou.to.bm <- model.test.ou.to.bm(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts, fixed.optima = fixed.optima)
    #     }
    # }
    
    # if(any(try.models == "OU.to.Trend")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "OU.to.Trend model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.ou.to.trend <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.ou.to.trend(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x], fixed.optima = fixed.optima)
    #         })
            
    #         cat(". Finished.")
    #         high.mod.ou.to.trend <- which(model.test.all.ou.to.trend[1, ] == max(model.test.all.ou.to.trend[1, ]))
    #         out.best.model.ou.to.trend <- model.test.all.ou.to.trend[ , high.mod.ou.to.trend]
    #         model_list$ou.to.trend <- out.best.model.ou.to.trend
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "OU.to.Trend model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$ou.to.trend <- model.test.ou.to.trend(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts, fixed.optima = fixed.optima)
    #     }
    #     cat("\n", "AICc =", model_list$ou.to.trend["AICc"])
    # }
    
    # if(any(try.models == "OU.to.EB")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "OU.to.EB model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "")
    #         model.test.all.ou.to.eb <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.ou.to.eb(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x], fixed.optima = fixed.optima)
    #         })
            
    #         cat(". Finished.")
    #         high.mod.ou.to.eb <- which(model.test.all.ou.to.eb[1, ] == max(model.test.all.ou.to.eb[1, ]))
    #         out.best.model.ou.to.eb <- model.test.all.ou.to.eb[ , high.mod.ou.to.eb]
    #         model_list$ou.to.eb <- out.best.model.ou.to.eb
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "OU.to.EB model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$ou.to.eb <- model.test.ou.to.eb(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts, fixed.optima = fixed.optima)
    #     }
    #     cat("\n", "AICc =", model_list$ou.to.eb["AICc"])    
    # }
    
    # if(any(try.models == "Stasis.to.BM")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "Stasis.to.EB model. Running ", length(ten_times), " time split models:")
    #         cat("\n", "\n", "")
    #         model.test.all.stasis.to.bm <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.stasis.to.bm(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
            
    #         cat(". Finished.")
    #         high.mod.stasis.to.bm <- which(model.test.all.stasis.to.bm[1, ] == max(model.test.all.stasis.to.bm[1, ]))
    #         out.best.model.stasis.to.bm <- model.test.all.stasis.to.bm[ , high.mod.stasis.to.bm]
    #         model_list$stasis.to.bm <- out.best.model.stasis.to.bm
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "Stasis.to.BM model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$stasis.to.bm <- model.test.stasis.to.bm(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$stasis.to.bm["AICc"])
    # }

    # if(any(try.models == "Stasis.to.EB")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "Stasis.to.EB model. Running ", length(ten_times), "models:")
    #         cat("\n", "")
    #         model.test.all.stasis.to.eb <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.stasis.to.eb(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
            
    #         cat(". Finished.")
    #         high.mod.stasis.to.eb <- which(model.test.all.stasis.to.eb[1, ] == max(model.test.all.stasis.to.eb[1, ]))
    #         out.best.model.stasis.to.eb <- model.test.all.stasis.to.eb[ , high.mod.stasis.to.eb]
    #         model_list$stasis.to.eb <- out.best.model.stasis.to.eb
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "Stasis.to.EB model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$stasis.to.eb <- model.test.stasis.to.eb(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$stasis.to.eb["AICc"])    
    # }
    
    # if(any(try.models == "Stasis.to.Trend")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "Stasis.to.Trend model. Running ", length(ten_times), "models:")
    #         cat("\n", "")
    #         model.test.all.stasis.to.trend <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.stasis.to.trend(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
            
    #         cat(". Finished.")
    #         high.mod.stasis.to.trend <- which(model.test.all.stasis.to.trend[1, ] == max(model.test.all.stasis.to.trend[1, ]))
    #         out.best.model.stasis.to.trend <- model.test.all.stasis.to.trend[ , high.mod.stasis.to.trend]
    #         model_list$stasis.to.trend <- out.best.model.stasis.to.trend
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "Stasis.to.Trend model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$stasis.to.trend <- model.test.stasis.to.trend(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$stasis.to.trend["AICc"])    
    # }
    
    # if(any(try.models == "Trend.to.OU")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "Trend.to.OU model. Running ", length(ten_times), "models:")
    #         cat("\n", "")
    #         model.test.all.trend.to.ou <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.trend.to.ou(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
    #         cat(". Finished.")
    #         high.mod.trend.to.ou <- which(model.test.all.trend.to.ou[1, ] == max(model.test.all.trend.to.ou[1, ]))
    #         out.best.model.trend.to.ou <- model.test.all.trend.to.ou[ , high.mod.trend.to.ou]
    #         model_list$trend.to.ou <- out.best.model.trend.to.ou
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "Trend.to.OU model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$trend.to.ou <- model.test.trend.to.ou(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$trend.to.ou["AICc"])
    # }
    
    # if(any(try.models == "Trend.to.Stasis")) {
        
    #     ## if no time.shifts supplied, test all times (with both modes having at least 10 samples)
    #     if(is.null(time.shifts) && run_time_split) {
    #         cat("\n", "Trend.to.Stasis model. Running ", length(ten_times), "models:")
    #         cat("\n", "")
    #         model.test.all.trend.to.stasis <- sapply(ten_times, function(x) {
    #             cat("\r", "model ", x - 8, " of ", length(ten_times))
    #             model.test.trend.to.stasis(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=all.times[x])
    #         })
    #         cat(". Finished.")
    #         high.mod.trend.to.stasis <- which(model.test.all.trend.to.stasis[1, ] == max(model.test.all.trend.to.stasis[1, ]))
    #         out.best.model.trend.to.stasis <- model.test.all.trend.to.stasis[ , high.mod.trend.to.stasis]
    #         model_list$trend.to.stasis <- out.best.model.trend.to.stasis
    #     }
        
    #     ## if time.shifts supplied, test those times only    
    #     if(!is.null(time.shifts)) {
    #         cat("\n", "Trend.to.Stasis model. Time split at", time.shifts)
    #         n.split <- length(time.shifts)  + 1
    #         model_list$trend.to.stasis <- model.test.trend.to.stasis(data.model.test = model_test_input, pool.variance = pool.variance, cl = control.list, time.shifts=time.shifts)
    #     }
    #     cat("\n", "AICc =", model_list$trend.to.stasis["AICc"])
    # }
    







    ## run custom models if user-supplied #TG: this bit will be the architecture for the combined models!

    # if(is.null(time.shifts) && !is.null(custom.model)) {
    #     warning("custom model with no time.shifts - custom model not run")
    # }
    
    # if(!is.null(custom.model) && !is.null(time.shifts)) {
    #     cat("\n", "running custom models")
    #     model.one <- custom.model[,1]
    #     model.two <- custom.model[,2]
    #     n.models <- length(custom.model[,1])
    #     custom.model.out <- lapply(1:n.models, function(x) model.test.shift.mode(data.model.test = model_test_input, time.shifts=time.shifts, mode.one=model.one[x], mode.two=model.two[x], pool.variance = pool.variance, cl=control.list))
    #     custom.model.names <- apply(custom.model, 1, function(x) paste0(x, collapse="_"))
    #     model_list <- c(model_list, custom.model.out)
    #     names(model_list)[-c(1:length(test.model))] <- custom.model.names
    #     cat(" .Done.")
    # }




    ## Sorting the results


    if(!return.raw.models) {
        ## Get all the corrected AIC
        aic_out <- sapply(model_out_list, function(x) x["AICc"])
        ## Calculate the Delta and weighted AIC
        delta_aicc <- aic_out - min(aic_out)
        weight_aicc <- exp(-0.5 * delta_aicc) / sum(exp(-0.5 * delta_aicc))

        ## Ordering the results
        order_aicc <- order(weight_aicc, decreasing = TRUE)

        ## Output matrix
        output_table <- cbind(aic_out, delta_aicc, weight_aicc,
                              sapply(model_out_list, function(x) x["log_likelihood"]),
                              sapply(model_out_list, function(x) x["n_parameters"]),
                              sapply(model_out_list, function(x) x["sample_size"]))

        ## Adding the likelihood and sample columns
        colnames(output_table)[4:6] <- c("log(lik)", "param", "sample")
        rownames(output_table) <- true_model_names

        ## Output object #TG: this needs to be thought through (to know what we need for plots, etc...)
        output <- list()
        # output$call <- data$call
        # output$data <- model_test_input
        output$results <- output_table
        # output$results <- model_out_list

        ## Naming the output class #TG: To deal with later
        #class(output) <- c("dispRity", "model.test")

        return(output)
    } else {
        return(model_out_list)
    }

    
    # if(plot.disparity) { #TG: This bit should be dealt properly in plot.dispRity
    
    #     par(mfrow=c(1, 2), mar=c(4,4,2,2), oma=c(10, 4, 4, 4))
    #     xAxis <- max(model_test_input[[4]]) - model_test_input[[4]]
    #     plot(xAxis, model_test_input[[1]], type="l", xlim=c(max(xAxis), 0), xlab="Time", ylab="central tendency", las=1)
    #     varUp <- model_test_input[[1]] + model_test_input[[2]]
    #     varDown <- model_test_input[[1]] - model_test_input[[2]]
    #     polygon(x=c(xAxis, rev(xAxis)), c(varUp, rev(varDown)), col="grey", border=F)
    #     lines(xAxis, model_test_input[[1]], col="grey50")
    #     abline(v=time.shifts, lty=2, lwd=2)
    #     plotcor <- barplot(weight.aicc[order.aicc], las=1, ylim=c(0, 1), col="grey30", border=F, ylab="Akaike weights", names=F)
    #     mtext(names(aic.out)[order.aicc], 1, las=2, at=plotcor[,1], line=1)
    # }

    # return.out <- list()
    # return.out$aicc.models <- cbind(aic.out, delta.aicc, weight.aicc)
    
    # if(return.model.full) {
    #     return.out$full.details <- c(model_list)
    # }
    
    # class(return.out) <- "dispRity.model.test"
    # invisible(return.out)
}    