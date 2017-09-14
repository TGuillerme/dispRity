rm(list=ls(all=T))
library(dispRity)
source('model.test_fun.R', chdir = TRUE)
source('model.test_fun_MNP.R', chdir = TRUE)
source('model.test.sim.R', chdir = TRUE)

	set.seed(123)
	# my_disparity <- dispRity(boot.matrix(time.subsamples(my_data…), ...), …) - set-up the data for input
	
	data(BeckLee_mat99)
	data(BeckLee_ages)
	data(BeckLee_tree)
	
	continuousData <- time.subsamples(BeckLee_mat99, BeckLee_tree, method="continuous", time=seq(120, 0, length.out=120), model="gradual")
	data_bootstrapped <- boot.matrix(continuousData)
	sumVar <- dispRity(data_bootstrapped, c(sum, variances))
	
	trueMedian <- sapply(data_bootstrapped[[3]], function(x)   median(data_bootstrapped[[1]][x[[1]]] ))
	

	# my_model_testings <- model.test(input_disparity, models = c(“OU”, “BM”)) - your suggestion is called 'model.test'

	# 'model.test'. This takes a 'dispRity' object as input and test models. This function allows for fitting of 1 to multiple models. Explanation given in the documentation in model.test_fun_MNP.R. 

	# Here we test all models with a break at 66 Ma - 'Trend' is the best fitting model and you should see a plot
	
	model.test.out <- model.test(input_disparity=sumVar, named.model = c("BM", "OU", "Stasis", "EB", "Trend", "multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "OU.to.Trend", "OU.to.EB", "Stasis.to.EB", "Stasis.to.Trend"), plot.disparity=TRUE, control.list=list(fnscale = -1), time.split=66, return.model.full=TRUE, fixed.optima=T) 
	
	# evidence of unequal variance, Bartlett's test of equal variances p =  0.0216 . Variance not pooled
	# time split models will be tested on 101 shift times
	# testing BM model, AICc = -250.3157
	# testing OU model, AICc = -248.2114
	# testing Stasis model, AICc = 144.9291
	# testing Early Burst model, AICc = -248.3128
	# testing Trend model, AICc = -255.4666
	# multiOU model. Time split at 66
	# AICc = -246.0704
	# multiStasis model. Time split at 66
	# AICc = 53.90302
	# BM.to.Trend model. Time split at 66
	# AICc = -246.2496
	# BM.to.EB model. Time split at 66
	# AICc = -214.4644
	# OU.to.Trend model. Time split at 66
	# AICc = -244.0584
	# OU.to.EB model. Time split at 66
	# AICc = -213.0176
	# Stasis.to.EB model. Time split at 66
	# AICc = -141.0217
	# Stasis.to.Trend model. Time split at 66
	# AICc = -136.7475

	# Not run - treat the split time as a free parameter. The likelihood of a split at all times that allow for at least 10 samples are tested (so it takes a while)
	
	# model.test.out.all.times <- model.test(input_disparity=sumVar, named.model = c("BM", "OU", "Stasis", "EB", "Trend", "multiOU", "multiStasis", "BM.to.Trend", "BM.to.EB", "OU.to.Trend", "OU.to.EB", "Stasis.to.EB", "Stasis.to.Trend"), plot.disparity=TRUE, control.list=list(fnscale = -1), return.model.full=TRUE, fixed.optima=T) 
	
	# models can be simulated with the function model.test.sim
	 
	 # take the estimate parameters
	 simulate.parameters.trend <- model.test.out$full.details$trend

	# simulate 1000 datasets
	trend.sim <- model.test.sim(1000, "Trend", sigma.squared=simulate.parameters[3], ancestral.state=simulate.parameters[2], trend=simulate.parameters[4], time.span=model.test_input[[4]], variance=model.test_input[[2]], sample.size=model.test_input[[3]])
	
	# find and compare the 95% CI from the simulation to the true values
	lower_95 <- apply(trend.sim, 1, function(x) quantile(x, 0.025))
	upper_95 <- apply(trend.sim, 1, function(x) quantile(x, 0.975))
	
	x_y_coords <- plot.disparity.time(sumVar, plot.coords=T)
	plot(x_y, col="white", ylim=c(0, 5), xaxs="i", yaxs="i", xlim=c(max(x_y[,1]), 0))
	polygon(c(x_y[,1], rev(x_y[,1])), c(lower_95, rev(upper_95)), col="#00000050", border=F)
	lines(x_y[,1], x_y[,2], lwd=6, col="#00000050")