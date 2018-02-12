est.mean <- function(p, data.model.test.in, model.type, optima.level.ou, optima.level.stasis, fixed.optima=F, est.anc=TRUE, model.anc, split.time) {
	
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
		
		mean.ou <- optima * (1 - exp(-alpha * data.model.test.in$subsamples)) + anc.state * exp(-alpha * data.model.test.in$subsamples)
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
	    ou.mean.splits <- sapply(1: n.optima, function(x) ou.mean.fun(anc.state, optima[x], alpha, data.model.test.in$subsamples))
	    	    
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
	   	 mean.trend <- anc.state + trend.param * data.model.test.in$subsamples
 		}
	   
	    
	}

