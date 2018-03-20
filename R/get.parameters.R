
# get.parameters <- function(model.output.pars, models, time.split) {

# optima.level <- stasis.level <- 1

# n.models <- length(models)
# first.model <- models[1]

# if(first.model == "BM") {
# 	parameters.out <- model.output.pars[c(1,2)]
# 	}
# if(first.model == "OU") {
# 	parameters.out <- model.output.pars[c(1:4)]
# 	optima.level <- optima.level + 1
# 	}
# if(first.model == "Trend") {
# 	parameters.out <- model.output.pars[c(1:2, 7)]
# 	}
# if(first.model == "EB") {
# 	parameters.out <- model.output.pars[c(1:2, 8)]
# 	}
# if(first.model == "Stasis") {
# 	parameters.out <- model.output.pars[c(5,6)]
# 	stasis.level <- stasis.level + 1
# 	}
# if(first.model == "multi.OU") {
# 	if(length(time.split) == 1)	parameters.out <- model.output.pars[c(1:4, 9)]
# 	if(length(time.split) == 2)	parameters.out <- model.output.pars[c(1:4, 9:10)]
# 	if(length(time.split) == 3)	parameters.out <- model.output.pars[c(1:4, 9:11)]
# }


# if(n.models > 1) {
	
# 	for(y in 2:n.models) {
# 		second.model <- models[y]
	
# 		if(second.model == "BM") {
# 			parameters.out <- c(parameters.out, model.output.pars[2])
# 			}
# 		if(second.model == "OU") {
# 			opt <- c(4, 9:10)[optima.level]
# 			parameters.out <- c(parameters.out, model.output.pars[c(2:3, opt)])
# 			optima.level <- optima.level + 1
# 			}
		
# 		if(second.model == "Trend") {
# 			parameters.out <- c(parameters.out, model.output.pars[c(2, 7)])
# 			}
		
# 		if(second.model == "EB") {
# 			parameters.out <- c(parameters.out, model.output.pars[c(2, 8)])
# 			}
		
# 		if(second.model == "Stasis") {
# 			stasis.opt <- c(5, 11:12)[stasis.level]
# 			parameters.out <- c(parameters.out, model.output.pars[c(6, stasis.opt)])
# 			stasis.level <- stasis.level + 1
			
# 			}
# 		}
		
# 		too.many <- duplicated(names(parameters.out))
# 		if(any(too.many)) parameters.out <- parameters.out[-which(too.many)]
# 	}
	
# 	return(parameters.out)
# }

