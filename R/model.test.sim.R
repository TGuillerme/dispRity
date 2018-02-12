#' @name model.test.sim
#'
#' @title Simulate Model Test
#'
#' @description Fit simulate models of disparity change through time
#'
#' @param number.sim The number of seperate simulations 
#' @param model The named model of evolution to simulate for changes in disparity-through-time using a homogenous or hetergenous model 
#' @param return.full Option to return just the disaprity measure (FALSE, default) or to return the full dispRity.model.test object. If return.full=TRUE and and number.sim > 1 the return is a list of lists
#' @param time.split The age of the change in mode. The age is measured as the time before the most recent sample, and multiple ages can be supplied in a vector. Note this only applies to heterogenous models
#' @param time.span The length of the sequence of class numeric. If one number is supplied this is treated as the length of the sequence and the time span is treated as sequence from 0 to time.span in unit increments. If the a vector of length > 1 is supplied, this is treated as the the age of each sample in the sequence.
#' @param variance The variance of each sample of class numeric. If one number is supplied this is the variance for all samples in the sequence. If a vector of equal length to the time.span vector is supplied, this is used for the variance of each sample in the sequence
#' @param sample.size The sample.size of each sample of class numeric. If one number is supplied this is the sample.size for all samples in the sequence. If a vector of equal length to the time.span vector is supplied, this is used for the sample.size of each sample in the sequence
#' @param time.split A vector of the age or ages at which the mode or optima changes
#' @param ancestral.state The ancestral state at the start of the sequence (i.e., time = 0)
#' @param sigma.squared The step-variance of the Brownian motion-type processes
#' @param optima.1 The optima applied to the first phase of the OU processes
#' @param optima.2 The optima applied to the second phase of the OU processes
#' @param optima.3 The optima applied to the third phase of the OU processes
#' @param theta.1 The theta applied to the first phase of the Stasis processes
#' @param theta.2 The theta applied to the second phase of the Stasis processes
#' @param theta.3 The theta applied to the third phase of the Stasis processes
#' @param alpha The attraction parameter of the OU processes
#' @param omega The attraction parameter of the Stasis process
#' @param eb.rate The exponential decrease parameter of the early burst process
#' @param trend Trend parameters
#' @param fixed.optima Logical to use an estimated optimum value in OU models (fixed.optima = FALSE), or whether to set the OU optimum to the ancestral value (fixed.optima = TRUE)
#'
#' @examples
#' ## To Add
#' @seealso \code{\link{model.test}}.
#'
#' @references
#' To Add: Hunt 2006, Hunt 2008, Harmon 2010, 
#' 
#' @author Mark N Puttick and Thomas Guillerme
#' @export


# number.sim=1
# model
# time.split=NULL
# time.span=100
# sample.size=100
# variance=1e-1
# sigma.squared=1
# ancestral.state=0.15
# alpha=1
# optima.1=0.15
# theta.1=1
# omega=1
# eb.rate=-0.1
# trend=.5
# optima.2=30
# optima.3=0.15
# theta.2=1
# theta.3=1
# model.type.in=c("Trend")

model.test.sim <- function(number.sim=1,model.type.in, time.split=NULL, return.full=FALSE, time.span=100, sample.size=100, variance=1e-1, sigma.squared=1, ancestral.state=0.15, alpha=1, optima.1=0.15, theta.1=1, omega=1, eb.rate=-0.1, trend=0.1, optima.2=0.15, optima.3=0.15, theta.2=1, theta.3=1, fixed.optima=F) {
    
    model.test <- length(model.type.in) > 1
    model.test.2 <- any(model.type.in == "multi.OU")
    shift.model.test <- any(model.test, model.test.2)
    if (is.null(time.split) && shift.model.test) stop("please supply a shift time for multi-mode model")
    
    p <- c()
    p[1] <- ancestral.state
    p[2] <- sigma.squared
    p[3] <- alpha
    p[4] <- optima.1
    p[5] <- theta.1
    p[6] <- omega
    p[7] <- trend
    p[8] <- eb.rate    
    p[9] <- optima.2
    p[10] <- theta.2
    p[11] <- theta.3
    
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

      total.n <- length(time.span)
    sample.time <- 1:total.n
    split.here.vcv <- c(1, time.split)
    split.here.2.vcv <- c(time.split - 1, total.n)        
    ou.mean <- NULL
    
    any.model <- which(model.type.in == "multi.OU")
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
        
    for(rec.model in 1:length(model.type.in)) {

        time.x <- time.int

        if(time.x == 1) {        
                
            data.model.test.int <- lapply(data.model.test, function(k) k[sort(sample.time[split.here.vcv[time.x] : (split.here.2.vcv[time.x])] )])
            output.vcv <- est.VCV(p, data.model.test.int, model.type=model.type.in[time.x])
            output.mean <- est.mean(p, data.model.test.int, model.type=model.type.in[time.x], optima.level.ou=optima.level.ou, optima.level.stasis= optima.level.stasis, fixed.optima=fixed.optima, est.anc=T, split.time=ou.mean)

            if(model.type.in[1] == "BM") {
                est.anc <- FALSE
                model.anc <- p[1]
                time.int <- time.x + 1
            }
            
            if(model.type.in[1] == "OU") {
                optima.level.ou <- optima.level.ou + 1
                est.anc <- FALSE
                model.anc <- tail(output.mean, 1)
                time.int <- time.x + 1
            }
            
            if(model.type.in[1] == "Stasis") {
                optima.level.stasis <- optima.level.stasis + 1
                model.anc <- p[5]
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model.type.in[1] == "Trend" || model.type.in[1] == "EB") {
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
                model.anc <- tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
            if(model.type.in[time.x] == "EB") {
                model.anc <- tail(output.mean, 1)
                est.anc <- FALSE
                time.int <- time.x + 1
            }
            
    }

    total_VCV[split.here.vcv[time.x] : (split.here.2.vcv[time.x]), split.here.vcv[time.x] : (split.here.2.vcv[time.x]) ] <- output.vcv
    total_mean <-  c(total_mean, output.mean)
    }
    output.values <- t(rmnorm(n = number.sim, mean =  total_mean, varcov = total_VCV))
    
    if(return.full) {
        
        output <- lapply(1:number.sim, function(x) {
            output.full <- list(output.values[,x], data.model.test.int[[1]], data.model.test.int[[2]], data.model.test.int[[3]])
            names(output.full) <- c("disparity_measure", "variance", "sample_size", "subsets")
            output.full
            }
        )
    
    } else {
        
        output <- output.values
    }
    return(output)
}