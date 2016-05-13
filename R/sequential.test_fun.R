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

    if(summary(one_model)$coefficients[4] < 0.05) {
        #Set slope to 0 if intercept is not significant
        slope <- 0
    } else {
        #Set slope to model's estimate if significant
        slope <- summary(one_model)$coefficients[1]
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

#Adding a line
add.line <- function(xs, ys, lines.args) {
    if(!is.null(lines.args)) {
        #Adding the x,y coordinates
        lines.args$x <- xs ; lines.args$y <- ys
        do.call(lines, lines.args)
    } else {
        lines(xs, ys)
    }
}

#Adding significance tokens
significance.token <- function(xs, ys, p.value, token.args) {
    if(p.value < 0.1) {
        #Selecting the token
        if(p.value < 0.1) token <- "."
        if(p.value < 0.05) token <- "*"
        if(p.value < 0.01) token <- "**"
        if(p.value < 0.001) token <- "***"
        #Default plotting
        if(is.null(token.args)) {
            text(x = sum(xs)/2, y = max(ys)+max(ys)*0.05, token)
        } else {
        #Plotting with arguments
            token.args$labels <- token
            token.args$x <- sum(xs)/2
            if(any(names(token.args) == "float")) {
                token.args$y <- max(ys)+max(ys)*token.args$float
                token.args$float <- NULL
            } else {
                token.args$y <- max(ys)+max(ys)*0.05
            }
            do.call(text, token.args)
        }
    }
}