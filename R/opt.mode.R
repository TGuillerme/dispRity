opt.mode <- function(p, model.type.in, time.split, data.model.test, ou.split, fixed.optima)  {
     
     
    if(!is.null(time.split)) time.split <-  sort(sapply(time.split, function(u) which.min(abs(u - rev(data.model.test[[4]])))))
     
  	total.n <- length(data.model.test$subsamples)
	sample.time <- 1:total.n
	split.here.vcv <-c(1, time.split)
	split.here.2.vcv <-c(time.split - 1, total.n)		
			
	any.model <- which(model.type.in == "multi.OU")

	if(any(any.model, na.rm=T)) {
			split.here.vcv <- split.here.2.vcv <- NULL
			ou.mean <- c(1, time.split, length(data.model.test$subsamples))
			split.here.vcv <- c(1, split.here.vcv)
			split.here.2.vcv <- c(split.here.2.vcv, length(data.model.test$subsamples))
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
			
			time.out <- data.model.test.int[[4]]
			time.out.diff <- diff(time.out[1:2])
			time.out.2 <- time.out - (min(time.out) - time.out.diff)
			data.model.test.int$subsamples <- time.out.2
				
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
	mnormt::dmnorm(t(data.model.test$central_tendency), mean =  total_mean, varcov = total_VCV, log = TRUE)
}