##' @name model.test.sim
#'
#' @title Simulate Model Test
#'
#' @description Simulate models of disparity change through time
#'
#' @param sim The number of seperate simulations 
#' @param model Either (i) the named model of evolution to simulate for changes in disparity-through-time using a homogenous or hetergenous model or (ii) the an object of class \code{dispRity.sim} from the \code{model.test} function. If the \code{model.test} (ii) is supplied, all remaining arguments apart from \code{sim} are ignored as the model specified by the input model is used
#' @param time.split The age of the change in mode. The age is measured as the time before the most recent sample, and multiple ages can be supplied in a vector. Note this only applies to heterogenous models
#' @param time.span The length of the sequence of class numeric. If one number is supplied this is treated as the length of the sequence and the time span is treated as sequence from 0 to time.span in unit increments. If the a vector of length > 1 is supplied, this is treated as the the age of each sample in the sequence.
#' @param variance The variance of each sample of class numeric. If one number is supplied this is the variance for all samples in the sequence. If a vector of equal length to the time.span vector is supplied, this is used for the variance of each sample in the sequence
#' @param sample.size The sample.size of each sample of class numeric. If one number is supplied this is the sample.size for all samples in the sequence. If a vector of equal length to the time.span vector is supplied, this is used for the sample.size of each sample in the sequence
#' @param parameters \code{list}  The model parameters used for simulations: ancestral.state (default = 0.01), sigma.squared (default = 1), alpha (default = 1), optima.1, optima.2, optima.3 (default = 0.15), theta.1, theta.2, theta.3 (default = 1), omega (default = 1), trend (default = 0.5), eb.rate (default = -0.1). 
#' @param fixed.optima A \code{logical} value, whether to use an estimated optimum value in OU models (\code{FALSE} - default), or whether to set the OU optimum to the ancestral value (\code{TRUE}).
#' @param model.rank If a \code{dispRity.sim} is supplied, which model is used for simulation. The rank refers to the order of models as specified by AICc, so if rank=1 (default) the best-fitting model is used for simulation
#' @return A list of class \code{dispRity.sim}. Each list element contains the simulated central_tendency, as well as the variance, sample_size, and subsets useds to simulate the data.
#'
#' @examples
#' ## To Add
#' @seealso \code{\link{model.test}}.
#'
#' @references
#' To Add: Hunt 2006, Hunt 2008, Harmon 2010, 
#' 
#' @require mnormt
#' @author Mark N Puttick and Thomas Guillerme
#' @export

# # sim=1
# time.split=66
# time.span=120
# sample.size=100
# variance=1e-1
# model=tests
# model.rank <- NULL

model.test.sim <- function(sim=1, model, time.split=NULL, time.span=100, variance=1, sample.size=100, parameters=list(), fixed.optima=FALSE, model.rank=1) {
    
    if(length(class(model)) == 2 && class(model)[2] == "model.test") {
            
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
            
    } else {
        
        if (length(time.span) == 1) {
            time.span <- c(1:(time.span))
            }
    
        sample_length <- length(time.span)
        
        if (length(variance) != sample_length) {
            variance <- rep(variance, sample_length)
            }
        
        if (length(sample.size) != sample_length) {
            sample.size <- rep(sample.size, sample_length)
            }
    
        data.model.test <- list(variance, sample.size, time.span)
        names(data.model.test) <- c("variance", "sample_size", "subsets")
    
        if(!is.null(time.split)) time.split <-  sort(sapply(time.split, function(u) which.min(abs(u - rev(data.model.test[[3]])))))
        
    }
             
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
        
    p <- unlist(parameters)  
    ord.p <- match(c("ancestral.state", "sigma.squared", "alpha", "optima.1", "theta.1", "omega", "trend", "eb.rate <- -0.1","optima.2", "optima.3", "theta.2", "theta.3"), names(p))
    p <- p[ord.p]

    total.n <- length(time.span)
    sample.time <- 1:total.n
    split.here.vcv <- c(1, time.split)
    split.here.2.vcv <- c(time.split - 1, total.n)        
    ou.mean <- NULL
    
    any.model <- which(model == "multi.OU")
    if(any(any.model, na.rm=T)) {
        split.here.vcv <- split.here.2.vcv <- NULL
        ou.mean <- c(1, time.split, max(time.span))
        split.here.vcv <- c(1, split.here.vcv)
        split.here.2.vcv <- c(split.here.2.vcv, max(time.span))
    }

    total_VCV <- matrix(0, nrow=total.n, ncol=total.n)
    total_mean <-  c()
    optima.level.ou <- optima.level.stasis <-1
    model.anc <- model.alpha <- NULL
    time.int <- 1
        
    for(rec.model in 1:length(model)) {

        time.x <- time.int

        if(time.x == 1) {        
                
            data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])
            output.vcv <- est.VCV(p, data.model.test.int, model.type=model[time.x])
            output.mean <- est.mean(p, data.model.test.int, model.type=model[time.x], optima.level.ou=optima.level.ou, optima.level.stasis= optima.level.stasis, fixed.optima=fixed.optima, est.anc=T, split.time=ou.mean)

            if(model[1] == "BM") {
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model[1] == "OU") {
                optima.level.ou <- optima.level.ou + 1
                est.anc <- FALSE
                model.anc <- tail(output.mean, 1)
                time.int <- time.x + 1
            }
            
            if(model[1] == "Stasis") {
                optima.level.stasis <- optima.level.stasis + 1
                model.anc <- p[5]
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model[1] == "Trend" || model[1] == "EB") {
                model.anc <- tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
                
    } else {
        
            data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])    
            time.out <- data.model.test.int[[3]]
            time.out.diff <- diff(time.out[1:2])
            time.out.2 <- time.out - (min(time.out) - time.out.diff)
            data.model.test.int[[3]] <- time.out.2
            output.vcv <- est.VCV(p, data.model.test.int, model.type=model[time.x])
            output.mean <- est.mean(p, data.model.test.in=data.model.test.int, model.type=model[time.x], optima.level.ou= optima.level.ou, optima.level.stasis= optima.level.stasis, fixed.optima=fixed.optima, est.anc=est.anc, model.anc=model.anc, split.time=NULL)
                    
            if(model[time.x] == "BM") {
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model[time.x] == "OU") {
                optima.level.ou <- optima.level.ou + 1
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model[time.x] == "Stasis") {
                optima.level.stasis <- optima.level.stasis + 1
                model.anc <- p[5]
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model[time.x] == "Trend") {
                model.anc <- tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model[time.x] == "EB") {
                model.anc <- tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
    }

    total_VCV[split.here.vcv[time.x] : (split.here.2.vcv[time.x]), split.here.vcv[time.x] : (split.here.2.vcv[time.x]) ] <- output.vcv
    total_mean <-  c(total_mean, output.mean)
    }
    
    output.values <- t(rmnorm(n = sim, mean =  total_mean, varcov = total_VCV))
    if(dim(output.values)[1] == 1) output.values <- t(output.values)
    output.simulation <- lapply(1:sim, function(x) {
            output.full <- list(output.values[,x], data.model.test[[1]], data.model.test[[2]], data.model.test[[3]])
        names(output.full) <- c("central_tendency", "variance", "sample_size", "subsets")
        class(output.full) <- "dispRity.sim"
        output.full
        }
    )
    
    output <- c()
    output$simulation.data <-  output.simulation
    
    if(test.p) {
            x <- c()
        x$sim <- sapply(output.simulation, function(x) x$central_tendency)
        x$central_tendency <- as.numeric(empirical.model[[4]]$central_tendency) 
        x$subsets <- as.numeric(empirical.model[[4]]$subsets)
        p.values <- rank_env_dtt(x, Plot = FALSE)
            output$p.value <- p.values
        
    }

    class(output) <- "dispRity.sim"
    return(output)
        
}
