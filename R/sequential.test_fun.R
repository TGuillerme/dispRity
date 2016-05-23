#Getting the data function
set.pair.series <- function(series_pair, intercept=NULL) {
    series_pair_out <- list()
    for(element in 1:length(series_pair[[1]])) {
        tmp_list <- lapply(series_pair, `[[`, element)
        #Getting the series from the list
        series_pair_out[[element]] <- list.to.table(tmp_list)
        #Remove series column
        series_pair_out[[element]]$series <- NULL
        #Setting the factor as binomial
        series_pair_out[[element]]$factor <- c(rep(0, length(series_pair[[1]][[element]])), rep(1, length(series_pair[[2]][[element]])))
        #Add intercept (if non-null)
        if(!is.null(intercept)) {
            #If intercept is a list get the right element!
            if(class(intercept) == "list") {
                series_pair_out[[element]]$intercept <- intercept[[element]][[1]]
            } else {
                series_pair_out[[element]]$intercept <- intercept
            }
        }
    }
    return(series_pair_out)
}
#Estimating intercept function
intercept.estimate <- function(intercept0, slope) {
    if(length(slope) > 1) {
        #First intercept
        intercept <- intercept0 + slope[1] * 1
        for(n in 2:length(slope)) {
            intercept <- intercept + slope[n] * 1
        }
    } else {
        intercept <- intercept0 + slope * 1
    }

    return(intercept)
}


#Sets the intercept0 for a model
set.intercept0 <- function(first_model) {
    #If intercept is significant
    if(summary(first_model)$coefficients[1,4] < 0.05) {
        #Set intercept0
        intercept0 <- coef(first_model)[1]
    } else {
        #Else intercept0 is just 0
        intercept0 <- 0
    }
    return(intercept0)
}

#Setting the predicted intercept for the next model
set.intercept.next <- function(one_model, intercept0) {

    model_summary <- summary(one_model)$coefficients

    #Check if the model contains an intercept
    if(dim(model_summary)[1] != 1) {
        p_value <- model_summary[2,4]
        slope <- model_summary[2,1]
    } else {
        p_value <- model_summary[4]
        slope <- model_summary[1]
    }

    if(p_value > 0.05) {
        #Set slope to 0 if intercept is not significant
        slope <- 0
    }

    #Calculate the next models intercept
    intercept_next <- intercept.estimate(intercept0, slope)

    return(intercept_next)
}

#Creating the model function
create.model <- function(data, family, intercept = NULL, ...) {
    if(is.null(intercept)) {
        #Estimating the intercept and the slope in the model
        model <- glm(data ~ factor, data = data, family = family, ...)
    } else {
        #Estimating only the slope in the model in the model
        if(intercept == "in.data") {
            #Intercept is present in the data
            intercept <- unique(data$intercept)
            model <- glm(data ~ factor - 1+offset(intercept), data = data, family = family, ...)
        } else {
            #Intercept is given as a value
            model <- glm(data ~ factor - 1+offset(intercept), data = data, family = family, ...)
        }
    }
    return(model)
}