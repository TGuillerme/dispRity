est.VCV <- function(p, data.model.test, model.type, fixed.optima=F, est.anc=TRUE, model.anc) {
	
	if(model.type == "BM" | model.type == "Trend")  {
	
		sigma.squared <- p[2]
		VCV <- sigma.squared * outer(data.model.test$subsamples, data.model.test$subsamples, FUN = pmin)
		diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
		return(VCV)
	}
	
	if(model.type == "OU" || model.type == "multi.OU" )  {
		
	  	alpha <- p[3]
	    sigma.squared <- p[2]
	  	VCV <- outer(data.model.test$subsamples, data.model.test$subsamples, function(x, y) abs(x - y))
	    VCV <- exp(-alpha * VCV)
		VCVd <- (sigma.squared / (2 * alpha)) * (1 - exp(-2 * alpha * data.model.test$subsamples))
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
	    time.out.eb <- data.model.test$subsamples
	    VCV <- outer(sigma.squared * ((exp(r.rate * time.out.eb) - 1) / r.rate), sigma.squared * ((exp(r.rate * time.out.eb) - 1) / r.rate), FUN=pmin)    
	    diag(VCV) <- diag(VCV) + data.model.test$variance / data.model.test$sample_size
	    return(VCV)
	}
}