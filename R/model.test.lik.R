# model.test.lik <- function(model.test_input, model.type.in, time.split, control.list, fixed.optima) {

# 	half.life <- (model.test_input$subsamples[length(model.test_input$subsamples)] - model.test_input$subsamples[1]) / 4    
# 	sts.params <- stasis.parameters(model.test_input)
# 	p <- c()
# 	p[1] <- model.test_input$central_tendency[1]
# 	p[2] <- bm.parameters(model.test_input)
# 	p[3] <- log(2) / half.life
# 	p[c(4, 9, 10)] <- model.test_input$central_tendency[length(model.test_input$central_tendency)]
# 	p[c(5, 11, 12)] <- sts.params[2]
# 	p[6] <- sts.params[1]
# 	p[7] <- trend.parameters(model.test_input)[2]
# 	p[8] <- eb.parameters(model.test_input)[2]

# 	if(is.null(control.list$ndeps)) {
# 		control.list$ndeps <- abs(p / 1000000)
# 		control.list$ndeps[control.list$ndeps == 0] <- 1e-08
# 	}
	
# 	lower.bounds <- c(NA, 1e-8, 1e-8, NA, NA, 1e-8, -100, -100, NA, NA, NA, NA)
# 	upper.bounds <- c(NA, 100, 100, NA, NA, 20, 100, -1e-8, NA, NA, NA, NA)
	
# 	model.output <- optim(par=p, fn=opt.mode,  method="L", control = control.list, model.type.in=model.type.in, time.split=time.split, data.model.test = model.test_input, lower=lower.bounds, upper=upper.bounds, fixed.optima=fixed.optima)
	
# 	model.output.pars <- model.output[[1]]
# 	names(model.output.pars) <- c("ancestral state", "sigma squared", "alpha", "optima.1", "theta.1", "omega", "trend", "eb", "optima.2", "optima.3", "theta.2", "theta.3")
# 	model.output$par <- get.parameters(model.output.pars, model.type.in, time.split=time.split)
# 	return(model.output)
# }
