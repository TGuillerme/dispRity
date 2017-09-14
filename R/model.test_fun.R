## Internal functions for model.test

## Selecting model data list
select.model.list <- function(data, observed = TRUE, cent.tend = median, rarefaction) {

    if(observed) {
        ## If observed is required
        central_tendency <- unlist(extract.dispRity(data, observed = TRUE))

        ## If disparity is a single value
        if(unique(unlist(lapply(data$disparity, lapply, lapply, length))) != 1) {
            ## Calculate the variance from the disparity data
            variance <- unlist(lapply(extract.dispRity(data, observed = FALSE), lapply, var))
        } else {
            ## Extract directly the variance from the data
            variance <- sapply(data[[3]], function(x) var(data[[1]][x[[1]]]))
        }

    } else {

        ## Getting the disparity
        if(!missing(rarefaction)) {
            disparity_tmp <- extract.dispRity(data, observed = FALSE, rarefaction = rarefaction)    
        } else {
            disparity_tmp <- extract.dispRity(data, observed = FALSE)
        }
        ## Calculating the central tendency
        central_tendency <- unlist(lapply(disparity_tmp, lapply, cent.tend))
        ## Calculating the variance
        variance <- unlist(lapply(disparity_tmp, lapply, var))
    }

    ## Getting the length of the samples
    summary_table <- summary(data)
    if(!missing(rarefaction)) {
        sample_length <- rep(rarefaction, length(central_tendency))
    } else {
        sample_length <- summary_table$n[which(!is.na(summary_table[,3]))]
    }

    ## Samples
    if(data$call$subsamples[1] == "continuous") {
        subsamples <- sort(as.numeric(names(data$subsamples)))
    } else {
        subsamples <- seq(1:length(data$subsamples))
    }
    
    subsamples <- max(subsamples) - subsamples

    ## Returns the data
    return(list("central_tendency" = central_tendency,
                "variance" = variance,
                "sample_size" = as.numeric(sample_length),
                "subsamples" = rev(subsamples)))
}

## Get the match_call model names
get.models.names <- function(match_call, time.shifts) {

    ## Lapply wrapper
    lapply.combine.name <- function(one_model_name) {
        if(length(grep(",", one_model_name)) > 0) {
            ## Combine model names
            one_model_name <- strsplit(strsplit(strsplit(one_model_name, split = "c\\(")[[1]][2], "\\)")[[1]][1], split = ",")[[1]]
            ## Remove spaces
            one_model_name <- gsub(" ", "", one_model_name)
            ## Combine multi model names
            if(length(unique(one_model_name)) == 1) {
                one_model_name <- paste0("multi", unique(one_model_name))
            } else {
                one_model_name <- paste(one_model_name, collapse = ":")
            }
        }
        return(one_model_name)
    }

    ## Get the model names (raw)
    model_names <- as.character(match_call$models)
    if(model_names[[1]] == "c" || model_names[[1]] == "list") {
        model_names <- model_names[-1]
    }

    ## Transform the model names
    model_names <- unlist(lapply(as.list(model_names), lapply.combine.name))

    ## Add the eventual time.shifts
    if(!is.null(time.shifts)) {
        if(class(time.shifts) == "list") {
            ## Match the time.shifts list and the models
            model_names <- gsub("NULL", "", paste0(model_names, time.shifts))
            ## Remove multi model names
            multis <- grep("c\\(", model_names)
            if(length(multis) > 0) {
                model_names[multis] <- gsub(" ", "", gsub(",", ":", gsub("\\)", "", gsub("c\\(", "", model_names[multis]))))
            }

        } else {
            ## Match the time.shifts to the model names with ":"
            time_models <- grep(":", model_names)
            model_names[time_models] <- paste0(model_names[time_models], time.shifts[1])
        }
    }

    return(model_names)
}

## Checking the number of subsamples between each shifts
check.shift.length <- function(one_time_shift, subsamples) {

    if(is.null(one_time_shift)) {
        ## If no shift, return null
        return(NULL)
    } else {
        lengths <- list()

        ## Loop through the shifts
        for(shift in 1:length(one_time_shift)) {
            ## Select elements before
            before <- length(which(subsamples <= one_time_shift[shift]))
            ## Select elements after
            after <- length(which(subsamples > one_time_shift[shift]))
            ## Save the results
            lengths[[shift]] <- c(before, after)
        }
        return(lengths)
    }
}

## pooled variance of all data.model.test in time series (modified from PaleoTS::pool.var)
pooled.variance <- function(data.model.test, rescale.variance=FALSE)  {
    sample.size.vector <- data.model.test$sample_size - 1
    var.vector <- data.model.test$variance
    pooled_variance <- sum(sample.size.vector * var.vector) / sum(sample.size.vector)
    if(rescale.variance) {
        data.model.test_out <- data.model.test
        data.model.test_out$variance <- rep(pooled_variance, length(data.model.test$central_tendency))
        return(data.model.test_out)
    } else {
        return(pooled_variance)
    }
}

## test for homogeneity of variance between samples using Bartlett's test
bartlett.variance <- function(model.test_input) {
    variance.pooled <- pooled.variance(model.test_input)
    total.n <- sum(model.test_input$sample_size)
    total.group.n <- length(model.test_input$variance)
    numerator <- (total.n - total.group.n) * log(variance.pooled) - (sum((model.test_input$sample_size - 1) * log(model.test_input$variance)))
    denominator <- 1 + (1 / (3 * (total.group.n -1))) * ((sum(1 / (model.test_input$sample_size - 1))) - (1 / (total.n - total.group.n)))
    test.statistic <- numerator / denominator
    return(pchisq(test.statistic, df = total.group.n - 1, lower.tail = FALSE))
}

## Lapply wrapper for testing a model.
lapply.model.test <- function(one_model, data.model.test, pool.variance, control.list, fixed.optima, verbose, ...) {
    ## Verbose
    if(verbose) cat("Running model ", one_model$name, "...", sep = "")

    ## Run the model

    ## Select only the functions
    model_funs <- as.vector(which(unlist(lapply(one_model, class)) == "function"))

    if(length(model_funs) == 1) {
        ## Simple model
        n.optima <- 1
        model_out <- one_model[[1]](data.model.test, pool.variance, control.list, fixed.optima, n.optima, ...)
        # model_out <- one_model[[1]](data.model.test, pool.variance, control.list, fixed.optima, n.optima) ; warning("DEBUG lapply.model.test")
    } else {
        ## Complex model
        model_out <- model.test.shift.mode()
    }

    ## Verbose
    if(verbose) cat("Done.\nAICc = ", model_out["AICc"], "\n", sep = "")

    return(model_out)
}



# # plot models

# plot.disparity.time <- function(input_disparity, plot.variance=FALSE, plot.coords=FALSE) {
    
#     model.test_input <- select.model.list(input_disparity)
#     xAxis <- max(model.test_input[[4]]) - model.test_input[[4]]
#     plot(xAxis, model.test_input[[1]], type="l", xlim=c(max(xAxis), 0), xlab="Time", ylab="disparity measure", las=1)
#     if(plot.variance) {
#         varUp <- model.test_input[[1]] + model.test_input[[2]]
#         varDown <- model.test_input[[1]] - model.test_input[[2]]
#         polygon(x=c(xAxis, rev(xAxis)), c(varUp, rev(varDown)), col="grey", border=F)
#     }    
#     lines(xAxis, model.test_input[[1]], col="grey50")
#     if(plot.coords) return(cbind(xAxis, model.test_input[[1]]))
# }







# # ou to trend
# ou.to.trend <- function (p, data.model.test, time.min, fixed.optima=FALSE)  {
    
#     time.min <- which.min(abs(data.model.test$subsamples - time.min))
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     alpha <- p[3]
    
#     sample.size <- length(data.model.test$central_tendency)
#     sample.time <- data.model.test$subsamples      
    
#     if (fixed.optima == FALSE) {
#         optima <- p[4]  
#         trend.param <- p[5]
#            mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)
#         } else {
#         optima <- anc.state
#         trend.param <- p[4]
#         mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)
#     }
 
#     VCV <- outer(sample.time, sample.time, function(x, y) abs(x - y))
#     VCV <- exp(-alpha * VCV)
#     VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * sample.time))
#     VCV <- VCV * outer(VCVd, VCVd, pmin)
#     diag(VCV) <- VCVd + data.model.test$variance / data.model.test$sample_size
#     mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)

#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0   

#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
          
#     VCV2 <- sigma.squared * outer(time.out, time.out, pmin)
#     diag(VCV2) <- diag(VCV2) + data.model.test$variance[-c(1 : (time.min - 1))] / data.model.test$sample_size[-c(1 : (time.min - 1))]
#     mean.trend <- mean.ou[-c(1 : (time.min - 1))] + trend.param * time.out.2
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2   
#     mean.full <- c(mean.ou[1:(time.min - 1)], mean.trend)    
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.ou.to.trend <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1), fixed.optima = FALSE) {
    
#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=T)
#         }
   
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1
       
#        input_parameters <- input_parameters_trend <- trend.parameters(data.model.test)
#     half.life <- (data.model.test$subsamples[length(data.model.test$subsamples)] - data.model.test$subsamples[1]) / 4

#       if (fixed.optima == FALSE) {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters[1] / 10, log(2) / half.life, data.model.test$central_tendency[length(data.model.test$central_tendency)])
#         input_parameters <- c(input_parameters, input_parameters_trend[2])
#         lower.model <- c(NA, 1e-10, 1e-08, NA, -100)
#     } else {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters[1] / 10, log(2) / half.life)
#         input_parameters <- c(input_parameters, input_parameters_trend[2])
#            lower.model <- c(NA, 1e-10, 1e-08, NA)
#     }

#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#         }

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn =  ou.to.trend, control = cl, method = "L-BFGS-B", lower = lower.model, data.model.test = data.model.test, time.min = time.split, fixed.optima = fixed.optima)
    
#     ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     alpha <- optimised_model$par[3]
#     if (fixed.optima == FALSE) {
#         optima <- optimised_model$par[4]
#         trend <- optimised_model$par[5]
#     } else {
#         optima <- ancestral.state
#         trend <- optimised_model$par[4]
#     }
    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, alpha, optima, trend, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "alpha", "optima", "trend", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }

# # ou to eb

# ou.to.eb <- function (p, data.model.test, time.min, fixed.optima=FALSE)  {
    
#     time.min <- which.min(abs(data.model.test$subsamples - time.min))
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     alpha <- p[3]
    
#     sample.size <- length(data.model.test$central_tendency)
#     sample.time <- data.model.test$subsamples 
    
#     if (fixed.optima == FALSE) {
#         optima <- p[4]  
#         r.rate <- p[5]
#            mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)
#         } else {
#         optima <- anc.state
#         r.rate <- p[4]
#         mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)
#     }    

#     VCV <- outer(sample.time, sample.time, function(x, y) abs(x - y))
#     VCV <- exp(-alpha * VCV)
#     VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * sample.time))
#     VCV <- VCV * outer(VCVd, VCVd, pmin)
#     diag(VCV) <- VCVd + data.model.test$variance / data.model.test$sample_size
#     mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)
    
#     VCV[c(time.min: dim(VCV)[1]) , ] <- 0
#     VCV[ , c(time.min: dim(VCV)[1])] <- 0   

#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
        
#     sample.size <- length(time.out)
#     VCV2 <- outer(sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), FUN=pmin) 
#     diag(VCV2) <- diag(VCV2) + data.model.test$variance[-c(1 : (time.min - 1))] / data.model.test$sample_size[-c(1 : (time.min - 1))]
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     mean.full <- mean.ou       
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.ou.to.eb <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1), fixed.optima = FALSE) {
    
#     if (pool.variance){
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }

#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1
        
#     input_parameters <- bm.parameters(data.model.test)
#        input.time.split <- max(data.model.test[[4]]) - time.split
#     half.life <- (data.model.test$subsamples[length(data.model.test$subsamples)] - data.model.test$subsamples[1]) / 4    
        
#     if (fixed.optima == FALSE) {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters / 10, log(2) / half.life, data.model.test$central_tendency[length(data.model.test$central_tendency)])
#         input_parameters <- c(input_parameters, eb.parameters(data.model.test)[2])
#         lower.model <- c(NA, 1e-8, 1e-08, NA, -20)
#             upper.model <- c(NA, NA, NA, NA, -1e-6)
#     } else {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters / 10, log(2) / half.life)
#         input_parameters <- c(input_parameters, eb.parameters(data.model.test)[2])
#         lower.model <- c(NA, 1e-8, 1e-08, -20)
#         upper.model <- c(NA, NA, NA, -1e-6)
#     }
  
#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#         }
        
#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters/10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
        
#     optimised_model <- optim(input_parameters, fn =  ou.to.eb, control = cl, method = "L-BFGS-B", lower = lower.model, upper=upper.model, data.model.test=data.model.test, time.min = time.split, fixed.optima = fixed.optima)

#     ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     alpha <- optimised_model$par[3]

#     if (fixed.optima == FALSE) {
#         optima <- optimised_model$par[4]
#         eb.rate <- optimised_model$par[5]
#     } else {
#         optima <- ancestral.state
#         eb.rate <- optimised_model$par[4]
#     }

#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, alpha, optima, eb.rate, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "alpha", "optima", "eb.rate", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
    
# # bm to eb

# bm.to.eb <- function (p, data.model.test, time.min)  {
    
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     r.rate <- p[3]
#     sample.size <- length(data.model.test$central_tendency)
#     VCV <- sigma.squared * outer(data.model.test$subsamples, data.model.test$subsamples, FUN = pmin)
#     mean.full <- rep(anc.state, sample.size)
                
#     VCV[c(time.min: dim(VCV)[1]), ] <- 0
#     VCV[ ,c(time.min: dim(VCV)[1])] <- 0
                        
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     VCV2 <- outer(sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), FUN=pmin) 
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.bm.to.eb <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {
    
#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }
        
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1

#     input_parameters <- data.model.test$central_tendency[1]
#     input_parameters[2:3] <- eb.parameters(data.model.test)
        
#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#     }
        
#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters/10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08

#     optimised_model <- optim(input_parameters, fn =  bm.to.eb, control = cl, method = "L-BFGS-B", lower = c(NA, 0, -20), upper=c(NA, NA, -1e-6), data.model.test=data.model.test, time.min = time.split)
        
#     ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     eb.rate <- optimised_model$par[3]
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, eb.rate, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "eb.rate", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
    
# # bm to trend

# bm.to.trend <- function (p, data.model.test, time.min)  {

#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     trend.param <- p[3]
#     sample.size <- length(data.model.test$central_tendency)
#     VCV <- sigma.squared * outer(data.model.test$subsamples, data.model.test$subsamples, FUN = pmin)
#     mean.bm <- rep(anc.state, sample.size)
                
#     VCV[c(time.min: dim(VCV)[1]), ] <- 0
#     VCV[ ,c(time.min: dim(VCV)[1])] <- 0
        
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     mean.two <- rep(anc.state, sample.size) 
#     VCV2 <- sigma.squared * outer(time.out, time.out, FUN = pmin)
#     mean.trend <- mean.two + trend.param * time.out.2
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     mean.full <- c(mean.bm[1:(time.min - 1)], mean.trend)
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#      return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.bm.to.trend <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {
    
#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }
        
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1        
        
#     input_parameters <- data.model.test$central_tendency[1]
#     input_parameters[2:3] <- trend.parameters(data.model.test)

#     if (input_parameters[2] <= 0) {
#             input_parameters[2] <- 1e-04
#             }
        
#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters/10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
           
#     optimised_model <- optim(input_parameters, fn = bm.to.trend, control = cl, method = "L-BFGS-B", lower = c(NA, 0, -100), upper=c(NA,NA, 100), data.model.test=data.model.test, time.min=time.split)

#       ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     trend <- optimised_model$par[3]
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, trend, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "trend", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
        
# # stasis to trend  

# stasis.to.trend <- function (p, data.model.test, time.min)  {
    
#     sigma.squared <- p[1]
#     trend.param <- p[2]
#     omega <- p[3]
#     theta <- p[4]
    
#     sample.size <- length(data.model.test$central_tendency)
#     VCV <- diag(omega, nrow = sample.size)
#     mean.stasis <- rep(theta, sample.size)    
                                    
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0   
        
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     VCV2 <- sigma.squared * outer(time.out.2, time.out.2, pmin)
    
#     mean.two <- as.numeric(rep(theta, sample.size) + trend.param * time.out.2)
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2   
#     mean.full <- c(mean.stasis[1:(time.min - 1)], mean.two)    
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size        
#     return(dmnorm(data.model.test$central_tendency, mean = mean.full, varcov = VCV, log = TRUE))  
# }
    
# model.test.stasis.to.trend <- function (data.model.test, time.split, pool.variance = T, cl = list(fnscale = -1)) {

#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }

#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1

#     input_parameters <- trend.parameters(data.model.test)
#     input_parameters[3:4] <- stasis.parameters(data.model.test)
     
#     if (input_parameters[3] <= 0 || is.na(input_parameters[3]))    {
#         input_parameters[3] <- 1e-06
#         }
       
#     if (input_parameters[1] <= 0) {
#         input_parameters[1] <- 1e-06
#         } 

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-01
       
#     optimised_model <- optim(input_parameters, fn = stasis.to.trend, control = cl, method = "L-BFGS-B", lower = c(1e-08, NA, 1e-08, NA), data.model.test=data.model.test, time.min=time.split-1)
    
#     omega <- optimised_model$par[3]
#     theta <- optimised_model$par[4]
#     sigma.squared <- optimised_model$par[1]
#     trend <- optimised_model$par[2]    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, omega, theta, sigma.squared, trend, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "omega", "theta", "sigma.squared", "trend", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
    
# # stasis to eb
    
# stasis.to.eb <- function (p, data.model.test, time.min)  {
    
#     sigma.squared <- p[1]
#     r.rate <- p[2]
#     omega <- p[3]
#     theta <- p[4]
#     sample.size <- length(data.model.test$central_tendency)
#     VCV <- diag(omega, nrow = sample.size)
#     mean.stasis <- rep(theta, sample.size)  
        
#     VCV[c(time.min: dim(VCV)[1]), ] <- 0
#     VCV[ ,c(time.min: dim(VCV)[1])] <- 0
    
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     VCV2 <- outer(sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), FUN=pmin) 
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#     mean.full <- mean.stasis     
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.stasis.to.eb <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {

#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#     }

#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1

#     input_parameters <- eb.parameters(data.model.test)
#     input_parameters[3:4] <- stasis.parameters(data.model.test)

#     if (input_parameters[1] <= 0) {
#         input_parameters[1] <- 1e-04
#         }
            
#     if (input_parameters[4] <= 0 || is.na(input_parameters[4]))    {
#         input_parameters[4] <- 1e-04
#         }

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters/10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn =  stasis.to.eb, control = cl, method = "L-BFGS-B", lower = c(1e-10, -20, 1e-10, NA), upper=c(NA, -1e-6, NA, NA), data.model.test=data.model.test, time.min=time.split)
       
#     omega <- optimised_model$par[3]
#     theta <- optimised_model$par[4]
#     sigma.squared <- optimised_model$par[1]
#     eb.rate <- optimised_model$par[2]    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, omega, theta, sigma.squared, eb.rate, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "omega", "theta", "sigma.squared", "eb.rate", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
    
# # trend to ou

# trend.to.ou <- function (p, data.model.test, time.min)  {
    
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     alpha <- p[3]
#     optima <- p[4]
#     trend.param <- p[5]

#     sample.size <- length(data.model.test$central_tendency)
#     sample.time <- data.model.test$subsamples
    
#     VCV <- sigma.squared * outer(sample.time, sample.time, pmin)
#     mean.trend <- rep(anc.state, sample.size) + trend.param * sample.time
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
    
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0
    
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     anc.state.2 <- mean.trend[(time.min - 1)]

#     VCV2 <- outer(time.out.2, time.out.2, function(x, y) abs(x - y))
#     VCV2 <- exp(-alpha * VCV2)
#     VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha *  time.out.2))
#     VCV2 <- VCV2 * outer(VCVd, VCVd, pmin)
#     mean.ou <- optima * (1 - exp(-alpha * time.out.2)) + anc.state.2 * exp(-alpha * time.out.2)
#     diag(VCV2) <- VCVd + data.model.test$variance[-c(1 : (time.min - 1))] / data.model.test$sample_size[-c(1 : (time.min - 1))]
#      VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
     
#     mean.full <- c(mean.trend[1:(time.min - 1)], mean.ou)    
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.trend.to.ou <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {
    
#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=T)
#         }
   
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1   
   
#        input_parameters <- input_parameters_trend <- trend.parameters(data.model.test)
#     half.life <- (data.model.test$subsamples[length(data.model.test$subsamples)] - data.model.test$subsamples[1]) / 4
#        input_parameters <- c(data.model.test$central_tendency[1], input_parameters[1] / 10, log(2) / half.life, data.model.test$central_tendency[length(data.model.test$central_tendency)])
#     input_parameters <- c(input_parameters, input_parameters_trend[2])

#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#         }

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn = trend.to.ou, control = cl, method = "L-BFGS-B", lower = c(NA, 1e-10, 1e-08, NA, -100), data.model.test = data.model.test, time.min = time.split)
    
#     ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     alpha <- optimised_model$par[3]
#     optima <- optimised_model$par[4]
#     trend <- optimised_model$par[5]
    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, alpha, optima, trend, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "alpha", "optima", "trend", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }

# # bm to ou

# bm.to.ou <- function (p, data.model.test, time.min, fixed.optima=FALSE)  {
    
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     alpha <- p[3]
    
#     if (fixed.optima == FALSE) {
#         optima <- p[4]  
#         } else {
#         optima <- anc.state
#     }
    
#     sample.size <- length(data.model.test$central_tendency)
#     sample.time <- data.model.test$subsamples
    
#     VCV <- sigma.squared * outer(sample.time, sample.time, FUN = pmin)
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#     mean.bm <- rep(anc.state, sample.size)
    
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0
    
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     anc.state.2 <- mean.bm[(time.min - 1)]

#     VCV2 <- outer(time.out.2, time.out.2, function(x, y) abs(x - y))
#     VCV2 <- exp(-alpha * VCV2)
#     VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha *  time.out.2))
#     VCV2 <- VCV2 * outer(VCVd, VCVd, pmin)
#     mean.ou <- optima * (1 - exp(-alpha * time.out.2)) + anc.state.2 * exp(-alpha * time.out.2)
#      diag(VCV2) <- VCVd + data.model.test$variance[-c(1 : (time.min - 1))] / data.model.test$sample_size[-c(1 : (time.min - 1))]
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2

#     mean.full <- c(mean.bm[1:(time.min - 1)], mean.ou)    
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
    
# model.test.bm.to.ou <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1), fixed.optima = FALSE) {
    
#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=T)
#         }
        
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1   
   
#        input_parameters <- data.model.test$central_tendency[1]
#        input_parameters[2] <- bm.parameters(data.model.test)
#     half.life <- (data.model.test$subsamples[length(data.model.test$subsamples)] - data.model.test$subsamples[1]) / 4

#      if (fixed.optima == FALSE) {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters[2] / 10, log(2) / half.life, data.model.test$central_tendency[length(data.model.test$central_tendency)])
#         lower.model <- c(NA, 1e-10, 1e-08, NA)
#     } else {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters[2] / 10, log(2) / half.life)
#            lower.model <- c(NA, 1e-10, 1e-08)
#     }

#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#         }

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn = bm.to.ou, control = cl, method = "L-BFGS-B", lower = lower.model, data.model.test = data.model.test, time.min = time.split, fixed.optima = fixed.optima)
    
#     ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     alpha <- optimised_model$par[3]
#     if (fixed.optima == FALSE) {
#         optima <- optimised_model$par[4]
#     } else {
#         optima <- ancestral.state
#     }
    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, alpha, optima, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "alpha", "optima", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }

# # bm to ou

# ou.to.bm <- function (p, data.model.test, time.min, fixed.optima=FALSE)  {
    
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     alpha <- p[3]
    
#     if (fixed.optima == FALSE) {
#         optima <- p[4]  
#         } else {
#         optima <- anc.state
#     }
    
#     sample.size <- length(data.model.test$central_tendency)
#     sample.time <- data.model.test$subsamples
    
#     VCV <- outer(sample.time, sample.time, function(x, y) abs(x - y))
#     VCV <- exp(-alpha * VCV)
#     VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha *  sample.time))
#     VCV <- VCV * outer(VCVd, VCVd, pmin)
#     diag(VCV) <- VCVd + data.model.test$variance/ data.model.test$sample_size
#     mean.ou <- optima * (1 - exp(-alpha * sample.time)) + anc.state * exp(-alpha * sample.time)
 
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0    
    
#     time.out <- data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     sample.size <- length(time.out)
#     anc.state.2 <- mean.ou[(time.min - 1)]
    
#     VCV2 <- sigma.squared * outer(time.out.2, time.out.2, pmin)
#     diag(VCV2) <- diag(VCV2) + data.model.test$variance[-c(1 : (time.min - 1))] / data.model.test$sample_size[-c(1 : (time.min - 1))]
#     mean.bm <- rep(anc.state.2, sample.size)
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2

#     mean.full <- c(mean.ou[1:(time.min - 1)], mean.bm)    
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }
        
# model.test.ou.to.bm <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1), fixed.optima = FALSE) {
    
#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=T)
#         }
        
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1
   
#        input_parameters <- data.model.test$central_tendency[1]
#        input_parameters[2] <- bm.parameters(data.model.test)
#     half.life <- (data.model.test$subsamples[length(data.model.test$subsamples)] - data.model.test$subsamples[1]) / 4

#      if (fixed.optima == FALSE) {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters[2] / 10, log(2) / half.life, data.model.test$central_tendency[length(data.model.test$central_tendency)])
#         lower.model <- c(NA, 1e-10, 1e-08, NA)
#     } else {
#         input_parameters <- c(data.model.test$central_tendency[1], input_parameters[2] / 10, log(2) / half.life)
#            lower.model <- c(NA, 1e-10, 1e-08)
#     }

#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#         }

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn = ou.to.bm, control = cl, method = "L-BFGS-B", lower = lower.model, data.model.test = data.model.test, time.min = time.split, fixed.optima = fixed.optima)
    
#     ancestral.state <- optimised_model$par[1]
#     sigma.squared <- optimised_model$par[2]
#     alpha <- optimised_model$par[3]
#     if (fixed.optima == FALSE) {
#         optima <- optimised_model$par[4]
#     } else {
#         optima <- ancestral.state
#     }
    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, alpha, optima, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "alpha", "optima", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }

# # trend to stasis

# trend.to.stasis <- function (p, data.model.test, time.min)  {
    
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     trend.param <- p[3]
#     omega <- p[4]
#     theta <- p[5]    
    
#     sample.size <- length(data.model.test$central_tendency)       
#     sample.time <- data.model.test$subsamples  
    
#     VCV <- sigma.squared * outer(sample.time, sample.time, pmin)
#     mean.trend <- rep(anc.state, sample.size) + trend.param * sample.time
    
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0
    
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     sample.size <- length(time.out)
#     VCV2 <- diag(omega, sample.size, sample.size)
#     mean.stasis <- rep(theta, sample.size)    
#     mean.full <- c(mean.trend[1:(time.min - 1)], mean.stasis)
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#     dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE)
# }
    
# model.test.trend.to.stasis <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {

#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }

#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1

#     input_parameters <- data.model.test[[1]][1]
#     input_parameters[2:3] <- trend.parameters(data.model.test)
#     input_parameters[4:5] <- stasis.parameters(data.model.test)
     
#     if (input_parameters[4] <= 0 || is.na(input_parameters[4]))    {
#         input_parameters[4] <- 1e-04
#         }
       
#     if (input_parameters[2] <= 0) {
#         input_parameters[2] <- 1e-04
#         } 

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-07
       
#     optimised_model <- optim(input_parameters, fn = trend.to.stasis, control = cl, method = "L-BFGS-B", lower = c(NA, 1e-8, -100, 1e-8, NA),data.model.test=data.model.test, time.min=time.split)

#     ancestral.state <- optimised_model$par[1]     
#     omega <- optimised_model$par[4]
#     theta <- optimised_model$par[5]
#     sigma.squared <- optimised_model$par[2]
#     trend <- optimised_model$par[3]    
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, omega, theta, ancestral.state, sigma.squared, trend, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "omega", "theta", "ancestral.state", "sigma.squared", "trend", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
    
# # bm to stasis

# bm.to.stasis <- function (p, data.model.test, time.min)  {
    
#     anc.state <- p[1]
#     sigma.squared <- p[2]
#     omega <- p[3]
#     theta <- p[4]    
    
#     sample.size <- length(data.model.test$central_tendency)       
#     sample.time <- data.model.test$subsamples  

#     VCV <- sigma.squared * outer(sample.time, sample.time, pmin)
#     mean.bm <- rep(anc.state, sample.size)
    
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0
    
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     sample.size <- length(time.out)
#     VCV2 <- diag(omega, nrow=sample.size, ncol=sample.size)
#     mean.stasis <- rep(theta, sample.size)    
#     mean.full <- c(mean.bm[1:(time.min - 1)], mean.stasis)
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))  
# }
    
# model.test.bm.to.stasis <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {

#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }

#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1        

#     input_parameters <- data.model.test[[1]][1]
#     input_parameters[2] <- bm.parameters(data.model.test)
#     input_parameters[3:4] <- stasis.parameters(data.model.test)
    
#     if (input_parameters[4] <= 0 || is.na(input_parameters[4]))    {
#         input_parameters[4] <- 1e-04
#         }
       
#     if (input_parameters[1] <= 0) {
#         input_parameters[1] <- 1e-04
#         } 

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn = bm.to.stasis, control = cl, method = "L-BFGS-B", lower = c(NA, 1e-8, 1e-8, NA), data.model.test=data.model.test, time.min=time.split)
     
#     ancestral.state <- optimised_model$par[1]      
#     omega <- optimised_model$par[3]
#     theta <- optimised_model$par[4]
#     sigma.squared <- optimised_model$par[2]
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, ancestral.state, sigma.squared, omega, theta, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "ancestral.state", "sigma.squared", "omega", "theta", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }
    
# # stasis to bm

# stasis.to.bm <- function (p, data.model.test, time.min)  {
        
#     sigma.squared <- p[1]
#     omega <- p[2]
#     theta <- p[3]    
#     sample.size <- length(data.model.test$central_tendency)       
#     sample.time <- data.model.test$subsamples  
    
#     VCV <- diag(omega, sample.size, sample.size)
#     mean.stasis <- rep(theta, sample.size) 
            
#     VCV[c((time.min): dim(VCV)[1]) , ] <- 0
#     VCV[ , c((time.min): dim(VCV)[1])] <- 0
    
#     time.out <-  data.model.test$subsamples[-c(1 : (time.min - 1))]
#     time.out.diff <- diff(time.out[1:2])
#     time.out.2 <- time.out - (min(time.out) - time.out.diff)
#     VCV2 <- sigma.squared * outer(time.out.2, time.out.2, FUN=pmin) 
    
#     mean.full <- mean.stasis
#     VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
#     diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
#     return(dmnorm(t(data.model.test$central_tendency), mean = mean.full, varcov = VCV, log = TRUE))
# }

# model.test.stasis.to.bm <- function (data.model.test, time.split, pool.variance = FALSE, cl = list(fnscale = -1)) {

#     if (pool.variance)  {
#         data.model.test <- pooled.variance(data.model.test, rescale.variance=TRUE)
#         }
    
#     input_parameters <- bm.parameters(data.model.test)
#     input_parameters[2:3] <- stasis.parameters(data.model.test)
    
#     input.time.split <- time.split
#     rev.time <- max(data.model.test[[4]]) - data.model.test[[4]]
#     time.split <- which.min(abs(rev.time - time.split)) + 1
    
#     if (input_parameters[2] <= 0 || is.na(input_parameters[2]))    {
#         input_parameters[2] <- 1e-04
#         }
       
#     if (input_parameters[1] <= 0) {
#         input_parameters[1] <- 1e-04
#         } 

#     if (is.null(cl$ndeps)) {
#         cl$ndeps <- abs(input_parameters / 10000)
#         }
#     cl$ndeps[cl$ndeps == 0] <- 1e-08
       
#     optimised_model <- optim(input_parameters, fn = stasis.to.bm, control = cl, method = "L-BFGS-B", lower = c(1e-8, 1e-8, NA), data.model.test=data.model.test, time.min=time.split)
     
#     omega <- optimised_model$par[2]
#     theta <- optimised_model$par[3]
#     sigma.squared <- optimised_model$par[1]
#     log.likelihood <- optimised_model$value
#     sample_k <- length(optimised_model$par)
#       sample_size <- length(data.model.test[[1]])
#     aic <- (-2 * log.likelihood) + (2 * sample_k)
#     aicc <- (-2 * log.likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
    
#     parameter.return <- c(log.likelihood, omega, theta, sigma.squared, sample_size, sample_k, aic, aicc, input.time.split)
#     names(parameter.return)    <-    c("log.likelihood", "omega", "theta", "sigma.squared", "sample.size", "n.parameters", "AIC", "AICc", "time.split")
#     return(parameter.return)
# }

# # shift mode  - not sure about these models. The effectively test the mode of evolution as completely independent with no shared parameters between shifts
   
# model.test.shift.mode <- function(data.model.test, time.split, mode.one, mode.two, pool.variance = FALSE, cl=list(fnscale = -1), fixed.optima = FALSE) {

#     input.time.split <- max(data.model.test[[4]]) - time.split
#     start.seq <- which(abs(data.model.test[[4]] - time.split) == min(abs(data.model.test[[4]] - time.split)))[1]
#     start.seq.2 <- start.seq  + 1
#     data.model.test.one <- lapply(data.model.test, function(x) x[1:start.seq])
#     data.model.test.two <- lapply(data.model.test, function(x) x[-c(1:start.seq)])
#     data.model.test.two[[4]] <- data.model.test.two[[4]] - data.model.test.two[[4]][1]
    
#     if(mode.one == "OU") mode.one.out <- model.test.ou(data.model.test.one, pool.variance=pool.variance, cl=cl, fixed.optima=fixed.optima)
#     if(mode.one == "BM") mode.one.out <- model.test.bm(data.model.test.one, pool.variance=pool.variance, cl=cl)
#     if(mode.one == "EB") mode.one.out <- model.test.eb(data.model.test.one, pool.variance=pool.variance, cl=cl)
#     if(mode.one == "Trend")  mode.one.out <- model.test.trend(data.model.test.one, pool.variance=pool.variance, cl=cl)
#     if(mode.one == "Stasis")  mode.one.out <- model.test.stasis(data.model.test.one, pool.variance=pool.variance, cl=cl)
        
#     if(mode.two == "OU") mode.two.out <- model.test.ou(data.model.test.two, pool.variance=pool.variance, cl=cl, fixed.optima=fixed.optima)
#     if(mode.two == "BM") mode.two.out <- model.test.bm(data.model.test.two, pool.variance=pool.variance, cl=cl)
#     if(mode.two == "EB") mode.two.out <- model.test.eb(data.model.test.two, pool.variance=pool.variance, cl=cl)
#     if(mode.two == "Trend")  mode.two.out <- model.test.trend(data.model.test.two, pool.variance=pool.variance, cl=cl)
#     if(mode.two == "Stasis")  mode.two.out <- model.test.stasis(data.model.test.two, pool.variance=pool.variance, cl=cl)

#     n.parameters.all <- sum(mode.one.out["n.parameters"], mode.two.out["n.parameters"])
#     sample.size <- length(data.model.test[[1]])
#     aic.model <- (-2 * sum(mode.one.out["log.likelihood"], mode.two.out["log.likelihood"])) + (2 * n.parameters.all)
#     aicc.model <- (-2 * sum(mode.one.out["log.likelihood"], mode.two.out["log.likelihood"])) + (2 * n.parameters.all) * (sample.size / ( sample.size - n.parameters.all - 1))
         
#     mode.one.par <- mode.one.out[1 : (which(names(mode.one.out) == "sample.size") - 1)]
#     mode.two.par <- mode.two.out[1 : (which(names(mode.two.out) == "sample.size") - 1)]
    
#        log.likelihood <- sum(mode.one.par[1], mode.two.par[1])
#     mode.one.par <- mode.one.par[-1]
#     mode.two.par <- mode.two.par[-1]
#     names(mode.one.par) <- paste0(names(mode.one.par), "_mode.one")
#     names(mode.two.par) <- paste0(names(mode.two.par), "_mode.two")
#     parameter.return <- c(log.likelihood, mode.one.par, mode.two.par, aic.model, aicc.model, input.time.split)
#     names(parameter.return)[1] <- "log.likelihood"
#     end.attach <- length(parameter.return)
#     names(parameter.return)[(end.attach - 2):end.attach] <- c("AIC", "AICc", "time.split")
#     return(parameter.return)
# }




