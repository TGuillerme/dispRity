#' @name model.test.sim
#'
#' @title Simulate Model Test
#'
#' @description Fit simulate models of disparity change through time
#'
#' @param number.sim The number of seperate simulations 
#' @param named.model The named model of evolution to simulate for changes in disparity-through-time using a homogenous or hetergenous model 
#' @param return.full Option to return just the disaprity measure (FALSE, default) or to return the full dispRity.model.test object. If return.full=TRUE and and number.sim > 1 the return is a list of lists
#' @param time.split The age of the change in mode. The age is measured as the time before the most recent sample, and multiple ages can be supplied in a vector. Note this only applies to heterogenous models
#' @param time.span The length of the sequence of class numeric. If one number is supplied this is treated as the length of the sequence and the time span is treated as sequence from 0 to time.span in unit increments. If the a vector of length > 1 is supplied, this is treated as the the age of each sample in the sequence.
#' @param variance The variance of each sample of class numeric. If one number is supplied this is the variance for all samples in the sequence. If a vector of equal length to the time.span vector is supplied, this is used for the variance of each sample in the sequence
#' @param sample.size The sample.size of each sample of class numeric. If one number is supplied this is the sample.size for all samples in the sequence. If a vector of equal length to the time.span vector is supplied, this is used for the sample.size of each sample in the sequence
#' @param time.split A vector of the age or ages at which the mode or optima changes
#' @param ancestral.state The ancestral state at the start of the sequence (i.e., time = 0)
#' @param sigma.squared The step-variance of the Brownian motion-type processes
#' @param optima The optima applied to the Stasis and OU processes
#' @param alpha The attraction parameter of the OU processes
#' @param omega The attraction parameter of the Stasis process
#' @param eb.rate The exponential decrease parameter of the early burst process
#' @param trend Trend parameters
#' @param plot.disparity Logical argument to plot the measure of disparity through time and a barplot of the relative support for each model (default = FALSE)
#' @param fixed.optima Logical to use an estimated optimum value in OU models (fixed.optima = FALSE), or whether to set the OU optimum to the ancestral value (fixed.optima = TRUE)
#' @param control.list Fine-tune control inputs for the optim function
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


	model.test.sim <- function(number.sim=1,named.model = c("BM", "OU", "Stasis", "EB", "Trend", "multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "BM.to.Stasis", "BM.to.OU", "OU.to.BM", "OU.to.Trend", "OU.to.EB", "Stasis.to.BM", "Stasis.to.EB", "Stasis.to.Trend", "Trend.to.OU", "Trend.to.Stasis"), time.split=NULL, time.span=100, sample.size=100, variance=1e-1, sigma.squared=1, ancestral.state=0.15, alpha=1, optima=0.15, theta=1, omega=1, eb.rate=-0.1, trend=0.1, return.full=FALSE) {
	
		if (length(time.span) == 1) {
			time.span <- seq(0:(time.span - 1))
			}
		sample_length <- length(time.span)
		
		if (length(variance) != sample_length) {
			variance <- rep(variance, sample_length)
			}
		
		if (length(sample.size) != sample_length) {
			sample.size <- rep(sample.size, sample_length)
			}
	
		time.split.in <- time.split - 1
		time.split	<- c(0, time.split - 1, max(time.span))  
		n.theta <- n.optima <- length(time.split)
	
		if (named.model == "BM") {
			VCV <- sigma.squared * outer(time.span, time.span, FUN = pmin)
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.bm <- rep(ancestral.state, sample_length)
			
			variance <- unname(variance)
			disparity_measure <- t(rmnorm(number.sim, mean=mean.bm, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
				dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
				names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
				dataFileIn
				})
			}
		}
				
		if (named.model == "OU" || named.model == "multiOU") {	
			ou.mean.fun <- function (ancestral.state, optima, alpha, time) optima * (1 - exp(-alpha * time)) + ancestral.state * exp(-alpha * time)
			if (named.model == "OU") {
				mean.ou <- ou.mean.fun(ancestral.state, optima, alpha, time.span)
			} else {
				mean.ou <- c()	
				for(x in 1:n.optima) mean.ou <- c(mean.ou, ou.mean.splits[(time.split[x] + 1) : time.split[x + 1] , x])  
			}
			
			VCV <- outer(time.span, time.span, function(x, y) abs(x - y))
			VCV <- exp(-alpha * VCV)
			VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * time.span))
			VCV_two <- outer(VCVd, VCVd, pmin)
			VCV <- VCV * VCV_two
			diag(VCV) <- VCVd + variance / sample.size
			variance <- unname(variance)
			disparity_measure <- t(rmnorm(number.sim, mean=mean.ou, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
				dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
				names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
				dataFileIn
				})
			}
		}

		if(named.model == "Stasis" || named.model == "multiStasis") {	
			if (named.model == "Stasis") {
				mean.stasis <- rep(theta, sample_length)
			} else {
				mean.stasis <- c()
	   			for(x in 1:n.optima) mean.stasis <- c(mean.stasis, rep(theta[x], length(time.split[x] : time.split[x + 1]) - 1)) 
			}
			
			VCV <- diag(omega + variance / sample.size)
			variance <- unname(variance)
			disparity_measure <- t(rmnorm(number.sim, mean=mean.stasis, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
				dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
				names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
				dataFileIn
				})
			}
		}

	if(named.model == "EB") {	
	  		VCV <- outer(sigma.squared * ((exp(eb.rate * time.span) - 1) / eb.rate), sigma.squared * ((exp(eb.rate * time.span) - 1) / eb.rate), pmin)
	  		diag(VCV) <- diag(VCV) + variance / sample.size
			mean.eb <- rep(ancestral.state, sample_length)	
			variance <- unname(variance)
			disparity_measure <- t(rmnorm(number.sim, mean=mean.eb, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
				dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
				names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
				dataFileIn
				})
			}
		}
	
		if(named.model == "Trend") {		
			VCV <- sigma.squared * outer(time.span, time.span, FUN = pmin)
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.trend <- rep(ancestral.state, sample_length) + trend * time.span
			disparity_measure <- t(rmnorm(number.sim, mean=mean.trend, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
		
	   	
		if(named.model == "BM.to.Trend") {		
			time.min <- time.split.in[1]
			VCV <- sigma.squared * outer(time.span, time.span, FUN = pmin)
			mean.bm <- rep(ancestral.state, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
	   	 	time.out.diff <- diff(time.out[1:2])
	    		time.out.2 <- time.out - (min(time.out) - time.out.diff) 	
	    		mean.two <- rep(ancestral.state, length(time.out)) + trend * time.out.2
			VCV2 <- sigma.squared * outer(time.out.2, time.out.2, FUN = pmin)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.bm[1:(time.min - 1)], mean.two)   							
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
	if(named.model == "BM.to.EB") {		   	
			time.min <- time.split.in[1]
			split.one <- time.min - 1
			VCV <- sigma.squared * outer(time.span, time.span, FUN = pmin)
			mean.bm <- rep(ancestral.state, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
	    	time.out.diff <- diff(time.out[1:2])
	   		time.out.2 <- time.out - (min(time.out) - time.out.diff)
	   		mean.two <- rep(ancestral.state, length(time.out)) 
			VCV2 <- outer(sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), pmin)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.bm[1:(time.min - 1)], mean.two)   						
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
	if(named.model == "BM.to.Stasis") {		
			time.min <- time.split.in[1]
			VCV <- sigma.squared * outer(time.span, time.span, FUN = pmin)
			mean.bm <- rep(ancestral.state, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
	   	 	time.out.diff <- diff(time.out[1:2])
	   	 	time.out.2 <- time.out - (min(time.out) - time.out.diff)
	    		sample_length_2 <- length(time.out)
	    		mean.two <- rep(theta, sample_length_2)  
	    		VCV2 <- diag(omega, nrow=sample_length_2, ncol=	sample_length_2)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.bm[1:(time.min - 1)], mean.two)   							
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
		
		if(named.model == "BM.to.OU") {		
			time.min <- time.split.in[1]
			VCV <- sigma.squared * outer(time.span, time.span, FUN = pmin)
			diag(VCV) <- diag(VCV) + variance / sample_size
			mean.bm <- rep(ancestral.state, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
	   	 	time.out.diff <- diff(time.out[1:2])
	   	 	time.out.2 <- time.out - (min(time.out) - time.out.diff)
	    		sample_length_2 <- length(time.out)
	    		anc.state.2 <- mean.bm[(time.min - 1)]	
			VCV2 <- outer(time.out.2, time.out.2, function(x, y) abs(x - y))
	    		VCV2 <- exp(-alpha * VCV2)
			VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha *  time.out.2))
	    		VCV2 <- VCV2 * outer(VCVd, VCVd, pmin)
			mean.ou <- optima * (1 - exp(-alpha * time.out.2)) + anc.state.2 * exp(-alpha * time.out.2)
	 		diag(VCV2) <- VCVd + variance[-c(1 : (time.min - 1))] / sample_size[-c(1 : (time.min - 1))]
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
			mean.full <- c(mean.bm[1:(time.min - 1)], mean.ou)    
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
		if(named.model == "OU.to.BM") {	
			time.min <- time.split.in[1]
			split.one <- time.min - 1	
			VCV <- outer(time.span, time.span, function(x, y) abs(x - y))
			VCV <- exp(-alpha * VCV)
			VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * time.span))
			VCV_two <- outer(VCVd, VCVd, pmin)
			VCV <- VCV * VCV_two
			diag(VCV) <- VCVd + variance / sample.size
			ou.mean.fun <- function (ancestral.state, optima, alpha, time) optima * (1 - exp(-alpha * time)) + ancestral.state * exp(-alpha * time)
			mean.ou <- ou.mean.fun(ancestral.state, optima, alpha, time.span)    
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	
			time.out <- time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    anc.state.2 <- mean.ou[(time.min - 1)]
		    mean.two <- rep(anc.state.2, length(time.out))
			VCV2 <- sigma.squared * outer(time.out, time.out, FUN = pmin)
			diag(VCV2) <- diag(VCV2) + variance[-c(1 : (time.min - 1))] / sample.size[-c(1 : (time.min - 1))]
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			mean.full <- c(mean.ou[1:(time.min - 1)], mean.two)
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
		if(named.model == "OU.to.Trend") {	
			time.min <- time.split.in[1]
			VCV <- outer(time.span, time.span, function(x, y) abs(x - y))
			VCV <- exp(-alpha * VCV)
			VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * time.span))
			VCV_two <- outer(VCVd, VCVd, pmin)
			VCV <- VCV * VCV_two
			diag(VCV) <- VCVd + variance / sample.size
			ou.mean.fun <- function (ancestral.state, optima, alpha, time) optima * (1 - exp(-alpha * time)) + ancestral.state * exp(-alpha * time)
			mean.ou <- ou.mean.fun(ancestral.state, optima, alpha, time.span)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	
			time.out <- time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    anc.state.2 <- mean.ou[(time.min - 1)]
		    mean.two <- rep(anc.state.2, length(time.out)) + trend * time.out.2
			VCV2 <- sigma.squared * outer(time.out, time.out, FUN = pmin)
			diag(VCV2) <- diag(VCV2) + variance[-c(1 : (time.min - 1))] / sample.size[-c(1 : (time.min - 1))]
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			mean.full <- c(mean.ou[1:(time.min - 1)], mean.two)
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}

		if(named.model == "OU.to.EB") {	
			time.min <- time.split.in[1]
			VCV <- outer(time.span, time.span, function(x, y) abs(x - y))
			VCV <- exp(-alpha * VCV)
			VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * time.span))
			VCV_two <- outer(VCVd, VCVd, pmin)
			VCV <- VCV * VCV_two
			diag(VCV) <- VCVd + variance / sample.size
			ou.mean.fun <- function (ancestral.state, optima, alpha, time) optima * (1 - exp(-alpha * time)) + ancestral.state * exp(-alpha * time)
			mean.ou <- ou.mean.fun(ancestral.state, optima, alpha, time.span)    
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	    	
			time.out <-  time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    mean.two <- rep(ancestral.state, length(time.out)) 
			VCV2 <- outer(sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.2) - 1) / r.rate), FUN=pmin)
			diag(VCV2) <- diag(VCV2) + variance[-c(1 : (time.min - 1))] / sample_size[-c(1 : (time.min - 1))]
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			mean.full <- c(mean.ou[1:(time.min - 1)], mean.two)   				
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
		if(named.model == "Stasis.to.BM") {	
			time.min <- time.split.in[1]
		   	diag(VCV) <- diag(time.span,length(time.span), length(time.span))
			mean.one <- rep(theta, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    mean.two <- rep(ancestral.state, length(time.out)) 
			VCV2 <- sigma.squared * outer(time.out.2, time.out.2, pmin)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.one[1:(time.min - 1)], mean.two)   				
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
		if(named.model == "Stasis.to.EB") {	
			time.min <- time.split.in[1]
		   	VCV <- diag(omega, length(time.span), length(time.span))
			mean.one <- rep(theta, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    mean.two <- rep(theta, length(time.out)) 
			VCV2 <- outer(sigma.squared * ((exp(eb.rate * time.out.2) - 1) / eb.rate), sigma.squared * ((exp(eb.rate * time.out.2) - 1) / eb.rate), pmin)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.one[1:(time.min - 1)], mean.two)   				
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	
		if(named.model == "Stasis.to.Trend") {	
			time.min <- time.split.in[1]
		  	VCV <- diag(time.span,length(time.span), length(time.span))
			mean.one <- rep(theta, sample_length)
			VCV[c((time.min): dim(VCV)[1]), ] <- 0
			VCV[ ,c((time.min): dim(VCV)[1])] <- 0	  
			time.out <-  time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    mean.two <- rep(ancestral.state, length(time.out)) + trend * time.out.2
			VCV2 <- sigma.squared * outer(time.out.2, time.out.2, pmin)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2	
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.one[1:(time.min - 1)], mean.two)   				
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
		
		if(named.model == "Trend.to.OU") {	
			time.min <- time.split.in[1]
			VCV <- sigma.squared * outer(time.span, time.span, pmin)
			mean.trend <- rep(anc.state, sample_length) + trend * time.span
			diag(VCV) <- diag(VCV) + variance / sample.size
			VCV[c((time.min): dim(VCV)[1]) , ] <- 0
		    VCV[ , c((time.min): dim(VCV)[1])] <- 0
		    time.out <-  time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
		    sample.size <- length(time.out)
		    anc.state.2 <- mean.trend[(time.min - 1)]
		    VCV2 <- outer(time.out.2, time.out.2, function(x, y) abs(x - y))
		    VCV2 <- exp(-alpha * VCV2)
			VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha *  time.out.2))
		    VCV2 <- VCV2 * outer(VCVd, VCVd, pmin)
			mean.ou <- optima * (1 - exp(-alpha * time.out.2)) + anc.state.2 * exp(-alpha * time.out.2)
		 	diag(VCV2) <- VCVd + variance[-c(1 : (time.min - 1))] / sample.size[-c(1 : (time.min - 1))]
		 	VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
			mean.full <- c(mean.trend[1:(time.min - 1)], mean.ou)    
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}

		if(named.model == "Trend.to.Stasis") {	
			time.min <- time.split.in[1]
			VCV <- sigma.squared * outer(time.span, time.span, pmin)
			mean.trend <- rep(anc.state, sample_length) + trend * time.span
			VCV[c((time.min): dim(VCV)[1]) , ] <- 0
		    VCV[ , c((time.min): dim(VCV)[1])] <- 0
		    time.out <-  time.span[-c(1 : (time.min - 1))]
		    time.out.diff <- diff(time.out[1:2])
		    time.out.2 <- time.out - (min(time.out) - time.out.diff)
			sample_size.2 <- length(time.out)
			VCV2 <- diag(omega, sample_size.2, sample_size.2)
			mean.stasis <- rep(theta, sample_size.2)    
			mean.full <- c(mean.trend[1:(time.min - 1)], mean.stasis)
			VCV[time.min:dim(VCV)[1], time.min:dim(VCV)[1]] <- VCV2
			diag(VCV) <- diag(VCV) + variance / sample.size
			mean.full <- c(mean.trend[1:(time.min - 1)], mean.ou)    
			disparity_measure <- t(rmnorm(number.sim, mean=mean.full, varcov = VCV))
			if (return.full == FALSE) {
				sim.data <- disparity_measure
			} else {
				sim.data <- lapply(1:number.sim, function(x) {
					dataFileIn <- list(disparity_measure[ ,x], variance, sample.size, time.span)
					names(dataFileIn) <- c("disparity_measure", "variance", "sample_size", "subsamples")
					dataFileIn
				})
			}
		}
	return(sim.data)
}	
	
	
	
	
	model.test.shift.mode.sim <- function(time.split, mode.one, mode.two, sigma.squared=c(1, 1), ancestral.state=0.1, trend=c(0.1, 0.1), variance=c(1e-1, 1e-1), eb.rate=c(-0.1, -0.1), alpha=c(1, 1), optima=c(0.15, 0.15), time.span=100, sample.size=c(100, 100)) {
		
		if(length(time.span) < 2) {
			time.span <- seq(0:(time.span - 1))
		}
		
		sample_length <- length(time.span)
		
		number.sim.one <- 	time.span - shiftTime
		number.sim.two <- 	time.span - number.sim.one
		time.span.one <- 0:(number.sim.one)
		time.span.two <- 0:(number.sim.two)
		number.sim.one <- length(time.span.one ) -1 
		number.sim.two <- length(time.span.two) -1
			
		if(mode.one == "OU") {
			model.one <- model.test.ou.sim(sigma.squared=sigma.squared[1],  ancestral.state=ancestral.state[1], alpha=alpha[1], optima=optima[1],  n.optima=1, time.span=number.sim.one[1], variance=variance[1], sample.size=sample.size[1])
			}
			
		if(mode.one == "BM") {
			model.one <- model.test.bm.sim(sigma.squared=sigma.squared[1], number.sim.one=number.sim.one[1], variance=variance[1], sample.size=sample.size[1])
			}
			
		if(mode.one == "EB") {
			model.one <- model.test.eb.sim(sigma.squared=sigma.squared[1], eb.rate=eb.rate[1], number.sim.one=number.sim.one[1], variance=variance[1], sample.size=sample.size[1], time.span=number.sim.one[1])
			}
			
		if(mode.one == "Trend") { 
			model.one <- model.test.trend.sim(sigma.squared=sigma.squared[1], ancestral.state=ancestral.state[1], trend=trend[1], variance=variance[1], sample.size=sample.size[1], time.span=number.sim.one[1])
			}
			
		ancestral.state2 <- tail(model.one[[1]], 1)
			
		if(mode.two == "OU") { 
			model.two <- model.test.ou.sim(sigma.squared=sigma.squared[2],  ancestral.state=ancestral.state2, alpha=alpha[2], optima=optima[2],  n.optima=1, time.span=number.sim.two, variance=variance[2], sample.size=sample.size[2])
			}
			
		if(mode.two == "EB") { 
			model.two <- model.test.eb.sim(sigma.squared=sigma.squared[2], eb.rate=eb.rate[2], number.sim.one=number.sim.two, variance=variance[2], sample.size=sample.size[2], ancestral.state=ancestral.state2)
			}
			
		if(mode.two == "Trend") { 
			model.two <- model.test.trend.sim(sigma.squared=sigma.squared[2], ancestral.state=ancestral.state2, trend=trend[2], variance=variance[2], sample.size=sample.size[2], time.span=number.sim.two)
			}
			
		dataset.one <- lapply(model.one, function(x) x)
		dataset.two <- lapply(model.two, function(x) x)
		dataset.two[[4]] <- dataset.two[[4]] - dataset.two[[4]][1]
		time.span.one <- dataset.one[[4]]
		time.span.two <- dataset.two[[4]]
		
		disparity_measure <- unlist(c(model.one[[1]], model.two[[1]]))
		variance <- unlist(c(model.one[[2]], model.two[[2]]))
		sample.size <- unlist(c(model.one[[3]], model.two[[3]]))
		time.span <- c(time.span.one, tail(time.span.one, 1) + 1 + time.span.two)
		dataFile <- list(disparity_measure, variance, sample.size, time.span)
		names(dataFile) <- c("disparity_measure", "variance", "sample_size", "subsamples")
		return(dataFile)
	}		

	
	
	
	
	
