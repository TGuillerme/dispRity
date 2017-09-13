## Parameters selections
select.parameters <- function(data.model.test, model.name) {

    ## Getting the sample size
    sample_size <- length(data.model.test$central_tendency)

    ## Selecting the time steps
    time_step <- (data.model.test$subsamples[sample_size] - data.model.test$subsamples[1]) / (sample_size - 1)

    ## Pooled variance
    var_pooled <- pooled.variance(data.model.test) #TG: this is probably redundant with other steps elsewhere! Pooled variance should be only calculated once

    ## Stasis model
    if(model.name == "Stasis") {
        ## Theta and omega (mean and var)
        theta <- mean(data.model.test$central_tendency[2:sample_size])
        omega <- var(data.model.test$central_tendency[2:sample_size]) - var_pooled / median(data.model.test$sample_size)
    
        ## Return Stasis parameters
        return(c("omega" = omega, "theta" = theta))
    }

    ## Epsilon
    epsilon <- 2 * var_pooled / round(median(data.model.test$sample_size)) 
    #~~~~~~
    #TG: In the case of a BM model, is it meant to be the round of the median sample_size (which is one value in this context - round(median(sample_size)) or the median number of elements per sample (median sample size - round(median(data.model.test$sample_size)))? I assume it's the later but please double check.
    #~~~~~

    ## Differences
    data_model_test_difference <- diff(data.model.test$central_tendency)

    if(model.name == "BM" || model.name == "OU") {
        ## Return BM parameter
        return((1/time_step) * ((1/(sample_size-1)) * sum(data_model_test_difference ^ 2) - epsilon))
    }

    ## Mean difference
    mean_difference <- mean(data_model_test_difference)

    if(model.name == "EB") {
        ## Get the sigma squared per steps
        sigma_squared_step <- (1 / time_step) * ((1 / sample_size) * sum(mean_difference ^ 2) - epsilon)
        ## Get the alpha
        alpha <- log(1e-5) / max(data.model.test$subsamples) * (1/2)

        ## Return EB parameters
        return(c("sigma_squared" = sigma_squared_step, "alpha" = alpha))
    }

    if(model.name == "Trend") {
        ## Get the trend parameter
        trend_param <- mean_difference / time_step 
        ## Get the sigma squared per steps
        sigma_squared_step <- (1 / time_step) * ((1 / (sample_size -1)) * sum(data_model_test_difference ^ 2) - mean_difference^2 - epsilon)
        #~~~~~~~
        #TG: this sigma square step counting is different than for EB. Anything abnormal there?
        #~~~~~~~

        ## Return Trend parameters
        return(c("sigma_squared" = sigma_squared_step, "trend" = trend_param))
    }

    ## No implemented method!
    return(NULL)
}

## Getting the parameters for each model
get.input.parameters <- function(data.model.test, model.name, fixed.optima = NULL, n.optima = NULL) {
    ## Selecting parameters
    input_parameters <- data.model.test$central_tendency[1]
    input_parameters <- c(input_parameters, select.parameters(data.model.test, model.name = model.name))

    ## OU parameter selection
    if(model.name == "OU") {
        ## Calculate the half life
        half_life <- (data.model.test$subsamples[length(data.model.test$subsamples)] - data.model.test$subsamples[1]) / 4
        ## Rescaling of the BM parameter
        input_parameters[2] <- input_parameters[2]/10 #TG: why?
        ## Adding the half life
        input_parameters[3] <- log(2) / half_life
        ## Selecting the optima
        n.optima <- ifelse(fixed.optima, n.optima - 1, n.optima)
        ## Adding the optima
        input_parameters <- c(input_parameters, rep(data.model.test$central_tendency[length(data.model.test$central_tendency)], n.optima))
    }

    ## Adding a 0 tolerance
    # zeros <- which(input_parameters[-1] <= 0)
    # if(length(zeros) > 0) {
    #     for(zero in 1:length(zeros)) {
    #         input_parameters[-1][zeros[zero]] <- 1e-04
    #     }
    # }
    input_parameters[2] <- ifelse(input_parameters[2] <= 0, 1e-04, input_parameters[2])

    return(input_parameters)
}

## Updating the control list
update.control.list <- function(control.list, input_parameters) {
    if(is.null(control.list$ndeps)) {
        control.list$ndeps <- abs(input_parameters/10000) #TG: where does the 10000 come from?
    }
    ## Adding a 0 tolerance
    control.list$ndeps[control.list$ndeps == 0] <- 1e-08

    return(control.list)
}

## Extracting the results for a model
extract.argument <- function(optimised_model, data.model.test, model.name, fixed.optima = NULL) {
    ## Ancestral states
    ancestral_state <- as.vector(optimised_model$par[1])
    ## Sigma squared
    sigma_squared <- as.vector(optimised_model$par[2])
    ## Log likelihood
    log_likelihood <- optimised_model$value
    ## Number of parameters
    n_parameters <- length(optimised_model$par)
    ## Sample Size
    sample_size <- length(data.model.test[[1]])
    ## Aics
    aics <- get.aic.aicc(log_likelihood, n_parameters, sample_size)

    ## OU output
    if(model.name == "OU") {
        ## Alpha
        alpha <- optimised_model$par[3]
        ## Optimum
        if(fixed.optima == FALSE) {
            optima <- optimised_model$par[-c(1:3)]
        } else {
            optima <- c(ancestral_state, optimised_model$par[-c(1:3)])
        }

        ## Selecting the output parameters
        parameter_return <- c(log_likelihood, ancestral_state, sigma_squared, alpha, optima, sample_size, n_parameters, aics[1], aics[2])
        ## Selecting the optimum display
        if(n.optima == 1) {
            optima_text <- "optimum"
        } else {
            optima_text <- paste0("optima:", 1:n.optima)
        }
        ## Naming the output parameters
        names(parameter_return) <- c("log_likelihood", "ancestral_state", "sigma_squared", "alpha", optima_text,  "sample_size", "n_parameters", "AIC", "AICc")

        return(parameter_return)
    }

    ## BM output
    if(model.name == "BM") {
        return(c("log_likelihood" = log_likelihood, "ancestral_state" = ancestral_state, "sigma_squared" = sigma_squared, "sample_size" = sample_size, "n_parameters" = n_parameters, "AIC" = aics[1], "AICc" = aics[2]))
    }

    ## EB output
    if(model.name == "EB") {
        ## Getting the early burst rate
        eb_rate <- optimised_model$par[3]
        ## Output
        return(c("log_likelihood" = log_likelihood, "ancestral_state" = ancestral_state, "sigma_squared" = sigma_squared, "eb_rate" = eb_rate, "sample_size" = sample_size, "n_parameters" = n_parameters, "AIC" = aics[1], "AICc" = aics[2]))
    }

    ## Else, not implemented
    return(NULL)
}


## Getting aic values
get.aic.aicc <- function(log_likelihood, sample_k, sample_size) {
    return(c((-2 * log_likelihood) + (2 * sample_k),
             (-2 * log_likelihood) + (2 * sample_k) * (sample_size / ( sample_size - sample_k - 1))
            ))
}

## ML optimisations
#TG: there must be an elegant way to "factorise" these functions as I did for the select.parameters one. This greatly simplifies the unit testing and is more bug robust (i.e. everything is likely to break rather than just an undetected false positive!)

## BM ML optimisation
optim.bm.ml <- function (parameters, data.model.test) {
    anc_state <- parameters[1]
    sigma_squared <- parameters[2]
    sample_size <- length(data.model.test$central_tendency)
    VCV <- sigma_squared * outer(data.model.test$subsamples, data.model.test$subsamples, FUN = pmin)
    diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
    mean.bm <- rep(anc_state, sample_size)
    return(mnormt::dmnorm(data.model.test$central_tendency, mean = mean.bm, varcov = VCV, log = TRUE))
}

## Stasis ML optimisation
optim.stasis.ml <- function (parameters, data.model.test, time.split = NULL, n.optima = 1) {
    time.split <- c(0, time.split, length(data.model.test$sample_size))
    theta <- parameters[-1]
    omega <- parameters[1]
    VCV <- diag(omega + data.model.test$variance / data.model.test$sample_size)
    mean.stasis <- c()
    for(x in 1:n.optima) mean.stasis <- c(mean.stasis, rep(theta[x], length(time.split[x] : time.split[x + 1]) - 1)) 
    return(mnormt::dmnorm(data.model.test$central_tendency, mean = mean.stasis, varcov = VCV, log = TRUE))
}

## EB ML optimisation
optim.eb.ml <- function (parameters, data.model.test)  {
    anc_state <- parameters[1]
    sigma_squared <- parameters[2]
    r.rate <- parameters[3]   
    sample_size <- length(data.model.test$central_tendency)
    time.out <- data.model.test$subsamples
    mean.eb <- rep(anc_state, sample_size)
    VCV <- outer(sigma_squared * ((exp(r.rate * time.out) - 1) / r.rate), sigma_squared * ((exp(r.rate * time.out) - 1) / r.rate), FUN=pmin)    
    diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
    return(mnormt::dmnorm(data.model.test$central_tendency, mean = mean.eb, varcov = VCV, log = TRUE))
}

## Trend ML optimisation
optim.trend.ml <- function (parameters, data.model.test)  {
    anc_state <- parameters[1]
    sigma_squared <- parameters[2]
    trend.param <- parameters[3]
    sample_size <- length(data.model.test$central_tendency)
    VCV <- sigma_squared * outer(data.model.test$subsamples, data.model.test$subsamples, FUN = pmin)
    diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
    mean.trend <- rep(anc_state, sample_size) + trend.param * data.model.test$subsamples
    return(mnormt::dmnorm(data.model.test$central_tendency, mean = mean.trend, varcov = VCV, log = TRUE))
}

## OU ML optimisation
optim.ou.ml <- function (parameters, data.model.test, time.split = NULL, n.optima = 1, fixed.optima = FALSE, optima.fixed.param = NULL)  {
    
    time.split <- c(0, time.split, length(data.model.test$subsamples))
    n.theta <- n.optima
    anc_state <- parameters[1]
    sigma_squared <- parameters[2]
    alpha <- parameters[3]

    VCV <- outer(data.model.test$subsamples, data.model.test$subsamples, function(x, y) abs(x - y))
    VCV <- exp(-alpha * VCV)
    VCVd <- (sigma_squared / (2 * alpha)) * (1 - exp(-2 * alpha * data.model.test$subsamples))
    VCV_two <- outer(VCVd, VCVd, pmin)
    VCV <- VCV * VCV_two
    diag(VCV) <- VCVd + data.model.test$variance / data.model.test$sample_size

    ou.mean.fun <- function (anc_state, optima, alpha, time) {
        return(optima * (1 - exp(-alpha * time)) + anc_state * exp(-alpha * time))
    }

    if (fixed.optima == FALSE) {
        optima <- parameters[-c(1:3)]    
        ou.mean.splits <- sapply(1:n.theta, function(x) ou.mean.fun(anc_state, optima[x], alpha, data.model.test$subsamples))
        mean.ou <- c()

        for(x in 1:n.optima) {
            mean.ou <- c(mean.ou, ou.mean.splits[(time.split[x] + 1) : time.split[x + 1] , x])
        }
    } else {
        optima <- anc_state
        if(n.optima > 1) {
            optima <- c(optima, parameters[-c(1:3)])
        }
        ou.mean.splits <- sapply(1:n.theta, function(x) ou.mean.fun(anc_state, optima[x], alpha, data.model.test$subsamples))
        mean.ou <- c()

        for(x in 1:n.optima) {
            mean.ou <- c(mean.ou, ou.mean.splits[(time.split[x] + 1) : time.split[x + 1] , x])  
        }
    }
    return(mnormt::dmnorm(t(data.model.test$central_tendency), mean = mean.ou, varcov = VCV, log = TRUE))
}
