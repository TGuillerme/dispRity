	# select.model.list <- function(data, observed = TRUE, cent.tend = median, rarefaction) {
	
	#     if(observed) {
	#         ## If observed is required
	#         central_tendency <- unlist(extract.dispRity(data, observed = TRUE))
	
	#         ## If disparity is a single value
	#         if(unique(unlist(lapply(data$disparity, lapply, lapply, length))) != 1) {
	#             ## Calculate the variance from the disparity data
	#             variance <- unlist(lapply(extract.dispRity(data, observed = FALSE), lapply, var))
	#         } else {
	#             ## Extract directly the variance from the data
	#             variance <- sapply(data[[3]], function(x) var(data[[1]][x[[1]]]))
	#         }
	
	#     } else {
	
	#         ## Getting the disparity
	#         if(!missing(rarefaction)) {
	#             disparity_tmp <- extract.dispRity(data, observed = FALSE, rarefaction = rarefaction)    
	#         } else {
	#             disparity_tmp <- extract.dispRity(data, observed = FALSE)
	#         }
	#         ## Calculating the central tendency
	#         central_tendency <- unlist(lapply(disparity_tmp, lapply, cent.tend))
	#         ## Calculating the variance
	#         variance <- unlist(lapply(disparity_tmp, lapply, var))
	#     }
	
	#     ## Getting the length of the samples
	#     summary_table <- summary(data)
	#     if(!missing(rarefaction)) {
	#         sample_length <- rep(rarefaction, length(central_tendency))
	#     } else {
	#         sample_length <- summary_table$n[which(!is.na(summary_table[,3]))]
	#     }
	
	#     ## Samples
	#     if(data$call$subsamples[1] == "continuous") {
	#         subsamples <- sort(as.numeric(names(data$subsamples)))
	#     } else {
	#         subsamples <- seq(1:length(data$subsamples))
	#     }
		
	# 	subsamples <- max(subsamples) - subsamples
	
	# 	## Returns the data
	#     return(list("central_tendency" = central_tendency,
	#                 "variance" = variance,
	#                 "sample_size" = sample_length,
	#                 "subsamples" = rev(subsamples)))
	# }
	
	
	# pooled.variance <- function(data.model.test, rescale.variance=FALSE)  {
	# 	sample.size.vector <- data.model.test$sample_size - 1
	# 	var.vector <- data.model.test$variance
	# 	pooled_variance <- sum(sample.size.vector * var.vector) / sum(sample.size.vector)
	#     if (rescale.variance) {
	#         data.model.test_out <- data.model.test
	#         data.model.test_out$variance <- rep(pooled_variance, length(data.model.test$central_tendency))
	#         return(data.model.test_out)
	#     } else {
	#         return(pooled_variance)
	#     }
	# }
	
	# # test for homogeneity of variance between samples using Bartlett's test
	
	# bartlett.variance <- function(model.test_input) {
	# 	variance.pooled <- pooled.variance(model.test_input)
	# 	total.n <- sum(model.test_input$sample_size)
	# 	total.group.n <- length(model.test_input$variance)
	# 	numerator <- (total.n - total.group.n) * log(variance.pooled) - (sum((model.test_input$sample_size - 1) * log(model.test_input$variance)))
	# 	denominator <- 1 + (1 / (3 * (total.group.n -1))) * ((sum(1 / (model.test_input$sample_size - 1))) - (1 / (total.n - total.group.n)))
	# 	test.statistic <- numerator / denominator
	# 	pchisq(test.statistic, df = total.group.n - 1, lower.tail = FALSE)
	# }
	
	# 	plot.disparity.time <- function(input_disparity, plot.variance=FALSE, plot.coords=FALSE) {
		
	# 	model.test_input <- select.model.list(input_disparity)
	# 	xAxis <- max(model.test_input[[4]]) - model.test_input[[4]]
	# 	plot(xAxis, model.test_input[[1]], type="l", xlim=c(max(xAxis), 0), xlab="Time", ylab="disparity measure", las=1)
	# 	if(plot.variance) {
	# 		varUp <- model.test_input[[1]] + model.test_input[[2]]
	# 		varDown <- model.test_input[[1]] - model.test_input[[2]]
	# 		polygon(x=c(xAxis, rev(xAxis)), c(varUp, rev(varDown)), col="grey", border=F)
	# 	}	
	# 	lines(xAxis, model.test_input[[1]], col="grey50")
	# 	if(plot.coords) return(cbind(xAxis, model.test_input[[1]]))
	# }

	
	# 