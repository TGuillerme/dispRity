###################
# Input fun
###################
select.model.list <- function(data, observed = TRUE, cent.tend = median, rarefaction, matrix = 1) {

    if(observed) {
        ## If observed is required
        central_tendency <- unlist(extract.dispRity(data, observed = TRUE))

        ## If disparity is a single value
        if(unique(unlist(lapply(data$disparity, lapply, lapply, length))) != 1) {
            ## Calculate the variance from the disparity data
            variance <- unlist(lapply(extract.dispRity(data, observed = FALSE), lapply, var, na.rm = TRUE))
        } else {
            ## Extract directly the variance from the data
            variance <- sapply(data[[4]], function(x) var(data$matrix[[matrix]][x[[1]]], na.rm = TRUE))
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
        variance <- unlist(lapply(disparity_tmp, lapply, var, na.rm = TRUE))
    }

    ## Getting the length of the samples
    summary_table <- summary(data)
    if(!missing(rarefaction)) {
        sample_length <- rep(rarefaction, length(central_tendency))
    } else {
        # sample_length <- summary_table$n[which(!is.na(summary_table[,3]))]
        sample_length <- summary_table$n
    }

    ## Samples
    if(data$call$subsets[1] == "continuous") {
        subsets <- sort(as.numeric(names(data$subsets)))
    } else {
        subsets <- seq(1:length(data$subsets))
    }
    
    subsets_out <- rev(max(subsets) - subsets)

    ## Remove NAs
    if(any(all_nas <- (is.na(central_tendency) | is.na(variance)))) {
        ## Fixing orthograph for warning message
        plural_s <- ifelse(length(which(all_nas)) > 1, "s", "")
        plural_is <- ifelse(length(which(all_nas)) > 1, "are", "is")
        warning(paste0("The following subset", plural_s, " contains NA and ", plural_is, " removed: ", paste0(rev(subsets)[all_nas], collapse = ", ")), ".")
        ## Update everything
        central_tendency <- central_tendency[!all_nas]
        variance <- variance[!all_nas]
        sample_length <- sample_length[!all_nas]
        subsets_out <- subsets_out[!all_nas]
    }

    ## Returns the data
    return(list("central_tendency" = central_tendency,
                "variance" = variance,
                "sample_size" = sample_length,
                "subsets" = subsets_out))
}


get.parameters <- function(model.output.pars, models, time.split, fixed.optima=NULL) {
 
    optima.level <- stasis.level <- 1
     
    n.models <- length(models)
    first.model <- models[1]
    
    switch(first.model,
        BM = {
            parameters.out <- model.output.pars[c(1,2)]
        },
        OU = {
            parameters.out <- model.output.pars[c(1:4)]
            if(fixed.optima) {
                parameters.out <- parameters.out[-4]
            }
            optima.level <- optima.level + 1
        },
        Trend = {
            parameters.out <- model.output.pars[c(1:2, 7)]
        },
        EB = {
            parameters.out <- model.output.pars[c(1:2, 8)]
        },
        Stasis = {
            parameters.out <- model.output.pars[c(5,6)]
            stasis.level <- stasis.level + 1
        },
        multi.OU = {
            if(length(time.split) == 1) {
                    parameters.out <- model.output.pars[c(1:4, 9)]
                    if(fixed.optima) parameters.out <- parameters.out[-4]
            }
            if(length(time.split) == 2) {
                    parameters.out <- model.output.pars[c(1:4, 9:10)]
                    if(fixed.optima) parameters.out <- parameters.out[-4]
            }
            if(length(time.split) == 3) {
                    parameters.out <- model.output.pars[c(1:4, 9:11)]
                    if(fixed.optima) parameters.out <- parameters.out[-4]
            }
        }
    )

    if(n.models > 1) {
        
        for(y in 2:n.models) {
            second.model <- models[y]
        
            switch(second.model,
                BM = {
                    parameters.out <- c(parameters.out, model.output.pars[2])
                },
                OU = {
                    opt <- c(4, 9:10)[optima.level]
                    parameters.out <- c(parameters.out, model.output.pars[c(2:3, opt)])
                    optima.level <- optima.level + 1
                },
                Trend = {
                    parameters.out <- c(parameters.out, model.output.pars[c(2, 7)])
                },
                EB = {
                    parameters.out <- c(parameters.out, model.output.pars[c(2, 8)])
                },
                Stasis = {
                    stasis.opt <- c(5, 11:12)[stasis.level]
                    parameters.out <- c(parameters.out, model.output.pars[c(6, stasis.opt)])
                    stasis.level <- stasis.level + 1
                }
            )
        }

        too.many <- duplicated(names(parameters.out))
        if(any(too.many)) parameters.out <- parameters.out[-which(too.many)]
    }

    return(parameters.out)
}


bm.parameters <- function (data.model.test) {
    
    sample.size <- length(data.model.test$central_tendency) - 1
    round.median.sample.size <- round(median(sample.size, na.rm = TRUE))
    t.step <- (data.model.test$subsets[sample.size + 1] - data.model.test$subsets[1]) / sample.size
    epsilon <- 2 * pooled.variance(data.model.test) / round.median.sample.size
       data.model.test.difference <- diff(data.model.test$central_tendency)
    return((1/t.step) * ((1/sample.size) * sum(data.model.test.difference ^ 2) - epsilon))
}

stasis.parameters <- function (data.model.test) {
    
    sample.size <- length(data.model.test$central_tendency)
    var.pooled <- pooled.variance(data.model.test)
    theta <- mean(data.model.test$central_tendency[2:sample.size], na.rm = TRUE)
    omega <- var(data.model.test$central_tendency[2:sample.size], na.rm = TRUE) - var.pooled / median(data.model.test$sample_size, na.rm = TRUE)
    return(c(omega, theta))
}

eb.parameters <- function (data.model.test) {
    
    sample.size <- length(data.model.test$central_tendency) - 1
    t.step <- (data.model.test$subsets[sample.size + 1] - data.model.test$subsets[1]) / sample.size
    epsilon <- 2 * pooled.variance(data.model.test) / round(median(data.model.test$sample_size, na.rm = TRUE))
    data.model.test.difference <- diff(data.model.test$central_tendency)
    mean.difference <- mean(data.model.test.difference, na.rm = TRUE)
    sigma.squared.step <- (1 / t.step) * ((1 / sample.size) * sum(mean.difference ^ 2) - epsilon)
    a <- log(1e-5) / max(data.model.test$subsets) * (1/2)
    return(c(sigma.squared.step, a))
}

trend.parameters <- function (data.model.test)  {
    
    sample.size <- length(data.model.test$central_tendency) - 1
    t.step <- (data.model.test$subsets[sample.size + 1] - data.model.test$subsets[1]) / sample.size
    epsilon <- 2 * pooled.variance(data.model.test) / round(median(data.model.test$sample_size, na.rm = TRUE))
    data.model.test.difference <- diff(data.model.test$central_tendency)
    mean.difference <- mean(data.model.test.difference, na.rm = TRUE)
    trend.param <- mean.difference / t.step 
    sigma.squared.step <- (1 / t.step) * ((1 / sample.size) * sum(data.model.test.difference ^ 2) - mean.difference^2 - epsilon)
    return(c(sigma.squared.step, trend.param))
}

###################
# Estimation fun
###################

est.mean <- function(p, data.model.test.in, model.type, optima.level.ou, optima.level.stasis, fixed.optima, est.anc=TRUE, model.anc, split.time) {
    
    if(model.type == "BM" || model.type == "EB")  {
    
        if(est.anc) {
            anc.state <- p[1]
        } else {
            anc.state <- model.anc
        }
        sample.size <- length(data.model.test.in[[1]])    
        return(rep(anc.state, sample.size))
    }
    
    if(model.type == "OU")  {
        
        if(est.anc) {
            anc.state <- p[1]
        } else {
            anc.state <- model.anc
        }        
        alpha <- p[3]
                
        if (fixed.optima == FALSE) {
           if(optima.level.ou == 1) optima <- p[4]
           if(optima.level.ou == 2) optima <- p[9]
           if(optima.level.ou == 3) optima <- p[10]    
        } else {
            optima <- anc.state
        }
        
        mean.ou <- optima * (1 - exp(-alpha * data.model.test.in$subsets)) + anc.state * exp(-alpha * data.model.test.in$subsets)
        return(mean.ou)
    }
    
    if(model.type == "multi.OU")  {
        
        if(est.anc) {
            anc.state <- p[1]
        } else {
            anc.state <- model.anc
        }    
        
        alpha <- p[3]
        optima <- p[c(4, 9, 10)]

       all.splits <- length(split.time)
       start.split <- split.time[-all.splits] 
       start.split[-1]  <- start.split[-1] + 1
       end.split <- split.time[-1]
       
       take.away <- start.split[1] - 1
       start.split <- start.split - take.away
       end.split <- end.split - take.away
        
        n.optima <- length(split.time) - 1
          
        ou.mean.fun <- function (anc.state, optima, alpha, time) optima * (1 - exp(-alpha * time)) + anc.state * exp(-alpha * time)
        ou.mean.splits <- sapply(1: n.optima, function(x) ou.mean.fun(anc.state, optima[x], alpha, data.model.test.in$subsets))
                
        mean.ou <- c()
        for(x in 1:n.optima) mean.ou <- c(mean.ou, ou.mean.splits[start.split[x] : end.split[x] , x])  
        return(mean.ou)
    }    
    
    if(model.type == "Stasis")  {
        
        if(optima.level.stasis == 1) theta <- p[5]
        if(optima.level.stasis  == 2) theta <- p[11]
        if(optima.level.stasis  == 3) theta <- p[12]
        sample.size <- length(data.model.test.in[[1]])    
        return(rep(theta,  sample.size))    
    }
    
    if(model.type == "Trend")  {
        
        if(est.anc) {
            anc.state <- p[1]
        } else {
            anc.state <- model.anc
        }        
            trend.param <- p[7]
            sample.size <- length(data.model.test.in[[1]])
            mean.trend <- anc.state + trend.param * data.model.test.in$subsets
        }
}

est.VCV <- function(p, data.model.test, model.type, est.anc=TRUE, model.anc) {
    
    if(model.type == "BM" | model.type == "Trend")  {
    
        sigma.squared <- p[2]
        VCV <- sigma.squared * outer(data.model.test$subsets, data.model.test$subsets, FUN = pmin)
        diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
        return(VCV)
    }
    
    if(model.type == "OU" || model.type == "multi.OU" )  {
        
        alpha <- p[3]
        sigma.squared <- p[2]
        VCV <- outer(data.model.test$subsets, data.model.test$subsets, function(x, y) abs(x - y))
        VCV <- exp(-alpha * VCV)
        VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * data.model.test$subsets))
        VCV_two <- outer(VCVd, VCVd, pmin)
        VCV <- VCV * VCV_two
        diag(VCV) <- VCVd + data.model.test$variance / data.model.test$sample_size
        return(VCV)
    }    

    if(model.type == "Stasis")  {
        
        omega <- p[6]
        VCV <- diag(omega + data.model.test$variance / data.model.test$sample_size)
        return(VCV)    
    }
    
    if(model.type == "EB")  {
        
        sigma.squared <- p[2]
        r.rate <- p[8]
        time.out.eb <- data.model.test$subsets
        # VCV <- outer(sigma.squared * ((exp(r.rate * time.out.eb) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.eb) - 1) / r.rate), FUN=pmin)    
        
        exponential.change <- sigma.squared * ((exp(r.rate * time.out.eb) - 1) / r.rate)
        exponential.matrix <- 1 - (exponential.change / max(exponential.change))
        exponential.matrix[which(exponential.matrix < 1e-7)] <- 1e-8
        VCV <- outer(exponential.matrix, exponential.matrix, FUN=pmin)  
        diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
        return(VCV)
    }
}


###################
# Likelihood fun
###################
model.test.lik <- function(model.test_input, model.type.in, time.split, control.list, fixed.optima) {

    half.life <- (model.test_input$subsets[length(model.test_input$subsets)] - model.test_input$subsets[1]) / 4
    sts.params <- stasis.parameters(model.test_input)
    p <- c()
    p[1] <- model.test_input$central_tendency[1]
    p[2] <- bm.parameters(model.test_input)
    p[3] <- log(2) / half.life
    p[c(4, 9, 10)] <- model.test_input$central_tendency[length(model.test_input$central_tendency)]
    p[c(5, 11, 12)] <- sts.params[2]
    p[6] <- sts.params[1]
    p[7] <- trend.parameters(model.test_input)[2]
    p[8] <- eb.parameters(model.test_input)[2]

    if(is.null(control.list$ndeps)) {
        control.list$ndeps <- abs(p / 1000000)
        control.list$ndeps[control.list$ndeps == 0] <- 1e-08
    }
    
    lower.bounds <- c(NA, 1e-8, 1e-8, NA, NA, 1e-8, -100, -100, NA, NA, NA, NA)
    upper.bounds <- c(NA, 100, 100, NA, NA, 20, 100, -1e-8, NA, NA, NA, NA)
    
    model.output <- stats::optim(par = p, fn = opt.mode,  method = "L", control = control.list, model.type.in = model.type.in, time.split = time.split, data.model.test = model.test_input, lower = lower.bounds, upper = upper.bounds, fixed.optima = fixed.optima)
    
    model.type.in
    
    model.output.pars <- model.output[[1]]
    names(model.output.pars) <- c("ancestral state", "sigma squared", "alpha", "optima.1", "theta.1", "omega", "trend", "eb", "optima.2", "optima.3", "theta.2", "theta.3")
    model.output$par <- get.parameters(model.output.pars, model.type.in, time.split = time.split, fixed.optima = fixed.optima)
    return(model.output)
}

opt.mode <- function(p, model.type.in, time.split, data.model.test, fixed.optima)  {
     
     
    if(!is.null(time.split)) time.split <- sort(sapply(time.split, function(u) which.min(abs(u - rev(data.model.test[[4]])))))
     
    total.n <- length(data.model.test$subsets)
    sample.time <- 1:total.n
    split.here.vcv <-c(1, time.split)
    split.here.2.vcv <-c(time.split - 1, total.n)        
            
    any.model <- which(model.type.in == "multi.OU")

    if(any(any.model, na.rm=T)) {
            split.here.vcv <- split.here.2.vcv <- NULL
            ou.mean <- c(1, time.split, length(data.model.test$subsets))
            split.here.vcv <- c(1, split.here.vcv)
            split.here.2.vcv <- c(split.here.2.vcv, length(data.model.test$subsets))
        }

    total_VCV <- matrix(0, nrow=total.n, ncol=total.n)

    total_mean <-  c()
    optima.level.ou <- optima.level.stasis <-1
    model.anc <- model.alpha <- NULL
    time.int <- 1
    
    for(rec.model in 1:length(model.type.in)) {

        time.x <- time.int

        if(time.x == 1) {        
            data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])
            output.vcv <- est.VCV(p, data.model.test.int, model.type=model.type.in[time.x])
                
            output.mean <- est.mean(p, data.model.test.in = data.model.test.int, model.type=model.type.in[time.x], optima.level.ou=optima.level.ou, optima.level.stasis= optima.level.stasis, fixed.optima=fixed.optima, est.anc=TRUE, split.time=ou.mean)

            if(model.type.in[1] == "BM") {
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model.type.in[1] == "OU") {
                optima.level.ou <- optima.level.ou + 1
                est.anc <- FALSE
                model.anc <- utils::tail(output.mean, 1)
                time.int <- time.x + 1
            }
            
            if(model.type.in[1] == "Stasis") {
                optima.level.stasis <- optima.level.stasis + 1
                model.anc <- p[5]
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model.type.in[1] == "Trend" || model.type.in[1] == "EB") {
                model.anc <- utils::tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
                
    } else {
        
            data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])    
            
            time.out <- data.model.test.int[[4]]
            time.out.diff <- diff(time.out[1:2])
            time.out.2 <- time.out - (min(time.out) - time.out.diff)
            data.model.test.int$subsets <- time.out.2
                
            output.vcv <- est.VCV(p, data.model.test.int, model.type=model.type.in[time.x])
    
            output.mean <- est.mean(p, data.model.test.in=data.model.test.int, model.type=model.type.in[time.x], optima.level.ou= optima.level.ou, optima.level.stasis= optima.level.stasis, fixed.optima=fixed.optima, est.anc=est.anc, model.anc=model.anc, split.time=NULL)
                    
            if(model.type.in[time.x] == "BM") {
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model.type.in[time.x] == "OU") {
                optima.level.ou <- optima.level.ou + 1
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model.type.in[time.x] == "Stasis") {
                optima.level.stasis <- optima.level.stasis + 1
                model.anc <- p[5]
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model.type.in[time.x] == "Trend") {
                model.anc <- utils::tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model.type.in[time.x] == "EB") {
                model.anc <- utils::tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
    }

    total_VCV[split.here.vcv[time.x] : (split.here.2.vcv[time.x]), split.here.vcv[time.x] : (split.here.2.vcv[time.x]) ] <- output.vcv
    total_mean <-  c(total_mean, output.mean)
    }
    mnormt::dmnorm(t(data.model.test$central_tendency), mean = total_mean, varcov = total_VCV, log = TRUE)
}


###################
# Miscelaneous fun
###################

pooled.variance <- function(data.model.test, rescale.variance=FALSE)  {
    sample.size.vector <- data.model.test$sample_size - 1
    var.vector <- data.model.test$variance
    pooled_variance <- sum(sample.size.vector * var.vector) / sum(sample.size.vector)
    if (rescale.variance) {
        data.model.test_out <- data.model.test
        data.model.test_out$variance <- rep(pooled_variance, length(data.model.test$central_tendency))
        return(data.model.test_out)
    } else {
        return(pooled_variance)
    }
}

### Function from Murrell D.J. 2018. 'Global envelope test to detect non-random bursts of trait evolution'. Methods Ecol. Evol. doi:10.1111/2041-210X.13006
####################################################
#
#   Generic function to compute rank envelope test
#
#   two tailed test: test="two.sided"
#   one sided tests: test="less" OR test="greater"
#
####################################################

rank_env_dtt <- function(x, alternative) {
    spp_num <- length(x$subsets)      
    sims <- as.matrix(x$sim)
    s1 <- sims[-c(1),]
    r <- as.vector(x$subsets[-c(1)])
    obs <- as.vector(x$central_tendency)[-c(1)]
    c1 <- list("r" = r, "obs" = obs, "sim_m" = s1)
    c2 <- spptest::create_curve_set(c1)
    res <- spptest::rank_envelope(c2, alternative = alternative)
    return(res) 
}

