#Getting the data function
set.pair.series <- function(series_pair, intercept=NULL) {
    #Getting the series from the list
    series_pair_out <- list.to.table(series_pair)
    #Setting the factor as binomial
    series_pair_out$factor <- c(rep(0, length(series_pair[[1]])), rep(1, length(series_pair[[2]])))
    #Add intercept (if non-null)
    if(!is.null(intercept)) {
        series_pair_out$intercept <- intercept
    }
    return(series_pair_out)
}

#Estimating intercept function
intercept.estimate <- function(intercept0, slopes) {
    if(length(slopes) > 1) {
        #First intercept
        intercept <- intercept0 + slopes[1] * 1
        for(n in 2:length(slopes)) {
            intercept <- intercept + slopes[n] *1
        }
    } else {
        intercept <- intercept0 + slopes * 1
    }

    return(intercept)
}

#Creating the model function
create.model <- function(data, family=binomial, intercept=NULL, ...) {
    if(is.null(intercept)) {
        #Estimating the intercept and the slope in the model
        model <- glm(data ~ factor, data=data, family=family, ...)
    } else {
        #Estimating only the slope in the model in the model
        model <- glm(data ~ factor - 1+offset(intercept), data=data, family=family, ...)
    }
    return(model)
}

#Saving results function
save.results <- function(model, results) {
    save_out <- match(results, names(summary(model)))
    return(summary(model)[save_out])
}