	bm.parameters <- function (data.model.test) {
		
		sample.size <- length(data.model.test$central_tendency) - 1
		round.median.sample.size <- round(median(sample.size))
		t.step <- (data.model.test$subsamples[sample.size + 1] - data.model.test$subsamples[1]) / sample.size
		epsilon <- 2 * pooled.variance(data.model.test) / round.median.sample.size
	   	data.model.test.difference <- diff(data.model.test$central_tendency)
	    return((1/t.step) * ((1/sample.size) * sum(data.model.test.difference ^ 2) - epsilon))
	}
	
		stasis.parameters <- function (data.model.test) {
		
		sample.size <- length(data.model.test$central_tendency)
		var.pooled <- pooled.variance(data.model.test)
	    theta <- mean(data.model.test$central_tendency[2:sample.size])
	    omega <- var(data.model.test$central_tendency[2:sample.size]) - var.pooled / median(data.model.test$sample_size)
	    return(c(omega, theta))
	}
	
	eb.parameters <- function (data.model.test) {
		
		sample.size <- length(data.model.test$central_tendency) - 1
	    t.step <- (data.model.test$subsamples[sample.size + 1] - data.model.test$subsamples[1]) / sample.size
	    epsilon <- 2 * pooled.variance(data.model.test) / round(median(data.model.test$sample_size))
	    data.model.test.difference <- diff(data.model.test$central_tendency)
	    mean.difference <- mean(data.model.test.difference)
	    sigma.squared.step <- (1 / t.step) * ((1 / sample.size) * sum(mean.difference ^ 2) - epsilon)
	    a <- log(1e-5) / max(data.model.test$subsamples) * (1/2)
	    return(c(sigma.squared.step, a))
	}
	
	trend.parameters <- function (data.model.test)  {
		
		sample.size <- length(data.model.test$central_tendency) - 1
	    t.step <- (data.model.test$subsamples[sample.size + 1] - data.model.test$subsamples[1]) / sample.size
	    epsilon <- 2 * pooled.variance(data.model.test) / round(median(data.model.test$sample_size))
	    data.model.test.difference <- diff(data.model.test$central_tendency)
	    mean.difference <- mean(data.model.test.difference)
	    trend.param <- mean.difference / t.step 
	    sigma.squared.step <- (1 / t.step) * ((1 / sample.size) * sum(data.model.test.difference ^ 2) - mean.difference^2 - epsilon)
	    return(c(sigma.squared.step, trend.param))
	}