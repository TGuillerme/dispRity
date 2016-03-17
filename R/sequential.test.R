#' @name sequential.test
#'
#' @title Sequential linear regressions
#'
#' @usage sequential.test(series, results = "coefficients", family, ...)
#'
#' @description Performs a sequential \code{\link[stats]{glm}} on the series by correcting for time autocorrelation. 
#'
#' @param series time series of which to estimate the slopes sequentially.
#' @param results which results from the \code{\link[stats]{glm}} to display (default = \code{"coefficients"}).
#' @param family the family of the \code{\link[stats]{glm}}.
#' @param ... optional arguments to be passed to the \code{\link[stats]{glm}}.
#' @param add whether to add the results of the sequential test to the current plot (default = \code{FALSE}).
#' @param lines.args a list of arguments to pass to \code{\link[graphics]{lines}} (default = \code{NULL}).
#' @param token.args a list of arguments to pass to \code{\link[graphics]{text}} for plotting tokens (see details; default = \code{NULL}).
#'
#' @details
#' This test allows to correct for time autocorrelation by estimating the intercept of the \code{\link[stats]{glm}} using a predicted intercept using the preceding \code{\link[stats]{glm}}.
#' the \code{token.args} argument intakes a list of arguments to be passed to \code{\link[graphics]{text}} for plotting the significance tokens. The plotted tokens are the standard p-value significance tokens from R:
#' \code{0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1}
#' Additionally, the \code{float} argument can be used for setting the height of the tokens compared to the slopes. For example one can use \code{sequential.test(..., token.args = list(float = 0.3, col = "blue", cex = 0.5))} for plotting blue tokens 50% smaller than normal and 30 higher than the slope.
#' 
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)),
#'      dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 100)
#' ## Calculating the sum of variances
#' sum_of_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
#' ## Extracting the row series
#' series <- extract.dispRity(sum_of_variances, observed = FALSE)
#'
#' ## The sequential test
#' sequential.test(series, family = gaussian)
#'
#' @seealso \code{\link{test.dispRity}}, \code{\link{bhatt.coeff}}, \code{\link{null.test}}.
#'
#'
#' @author Thomas Guillerme
#' @export

#Testing
#source("sanitizing.R")
#source("sequential.test_fun.R")
#source("test.dispRity_fun.R")

sequential.test <- function(series, results = "coefficients", family, ..., add = FALSE, lines.args = NULL, token.args = NULL) {

    #SANITIZING
    #results must be at least coefficients!
    if(is.na(match("coefficients", results))) {
        results <- c(results, "coefficients")
    }

    #Family
    if(missing(family)) {
        stop("glm family type argument is necessary!")
    }
    # if(family(link="identity")[[1]] == "gaussian") {
    #     warning("Model family is set to gaussian, should it not be binomial?")
    # }

    #add
    check.class(add, "logical")

    #lines.args
    if(!is.null(lines.args)) check.class(lines.args, "list")

    #token.args
    if(!is.null(token.args)) check.class(token.args, "list")


    #APPLYING THE SEQUENTIAL TEST

    #Setting the sequence
    seq_series <- unlist(apply(set.sequence(length(series)), 2, list), recursive=FALSE)

    #Applying the first test to get the intercept origin
    model1 <- create.model(set.pair.series(series[seq_series[[1]]]), family, ...)
    #model1 <- create.model(set.pair.series(series[seq_series[[1]]]), family) ; warning("DEBUG")
    
    #Set origin intercept for the following models
    intercept_predict <- NULL

    #If intercept is significant
    if(summary(model1)$coefficients[1,4] < 0.05) {
        #Set intercept0
        intercept0 <- coef(model1)[1]
        #If slope is significant
        if(summary(model1)$coefficients[2,4] < 0.05) {
            # Calculate predict intercept for next model
            intercept_predict <- intercept.estimate(intercept0, coef(model1)[2])
        } else {
            #intercept 1 is just intercept
            intercept_predict <- intercept0
        }
    } else {
    #Intercept is just 0
        intercept0 <- 0
        #If slope is significant
        if(summary(model1)$coefficients[2,4] < 0.05) {
            #Caclulate predict intercept for next model
            intercept_predict <- intercept.estimate(intercept0, coef(model1)[2])
        } else {
            #intercept 1 is just intercep
            intercept_predict <- intercept0
        }
    }

    #Loop through the other models
    models <- list(NULL)
    #Storing the first model
    models[[1]] <- model1
    for(model in 2:(length(seq_series))) {
        #Create the new model
        models[[model]] <- create.model(data = set.pair.series(series[seq_series[[model]]], intercept = intercept_predict[model-1]), family, intercept = intercept_predict[model-1], ...)
        #models[[model]] <- create.model(data = set.pair.series(series[seq_series[[model]]], intercept = intercept_predict[model-1]), family, intercept = intercept_predict[model-1]) ; warning("DEBUG")

        #Predict the new intercept
        #If slope is significant
        if(summary(models[[model]])$coefficients[1,4] < 0.05) {
            #Calculate new intercept for next model by using the current model slope and the previous model intercept
            new_intercept <- intercept.estimate(intercept0 = intercept0, c(coef(model1)[2], unlist(lapply(models[-1], coef)) ))
        } else {
            #Intercept remains the same as previously
            new_intercept <- intercept_predict[model-1]
        }
        intercept_predict <- c(intercept_predict, new_intercept)
    }

    #SAVING THE RESULTS
    #Saving the results for the first model
    model1_results <- save.results(model1, results)
    models_results <- lapply(models, save.results, results)

    #Creating the saving table template
    Matrix_template <- matrix(NA, nrow=length(seq_series), ncol=length(model1_results$coefficients[1,]))
    rownames(Matrix_template) <- unlist(lapply(convert.to.character(seq_series, series), paste, collapse=" - "))

    #Saving the first intercept
    Intercept_results <- Matrix_template
    Intercept_results[1,] <- model1_results$coefficients[1,]
    colnames(Intercept_results) <- names(model1_results$coefficients[1,])

    #Adding the predict column
    if(length(seq_series) > 1) {
        #Empty Predict column
        Intercept_results <- cbind(rep(NA, nrow(Intercept_results)), Intercept_results)
        colnames(Intercept_results)[1] <- "Predict"
        #Added predicted intercepts (ignoring the last one that's not used)
        Intercept_results[,1] <- intercept_predict
    }

    #Saving the slopes
    Slope_results <- Matrix_template
    Slope_results[1,] <- model1_results$coefficients[2,]
    colnames(Slope_results) <- names(model1_results$coefficients[2,])

    #Adding the other slopes
    if(length(seq_series) > 1) {
        #Adding the slopes
        for(model in 2:length(seq_series)) {
            Slope_results[model,] <- models_results[[model]]$coefficients
        }
    }

    #Combining the tables
    results_out <- list("Intercept" = Intercept_results, "Slope" = Slope_results)

    #Plotting
    if(add == TRUE) {
        #Getting x,y coordinates for the first model
        xs <- seq_series[[1]]
        ys <- c(intercept0, Intercept_results[1,1])
        #Plotting the line
        add.line(xs, ys, lines.args)
        #Add significance (if necessary)
        significance.token(xs, ys, Slope_results[1,4], token.args)

        #Looping through the other models
        for(series in 2:length(seq_series)) {
            #Getting x,y coordinates for the first model
            xs <- seq_series[[series]]
            ys <- c(Intercept_results[series-1,1], Intercept_results[series,1])
            #Plotting the line
            add.line(xs, ys, lines.args)
            #Add significance (if necessary)
            significance.token(xs, ys, Slope_results[series,4], token.args)
        }
    }

    #Anything else to save?
    if(any(results != "coefficients")) {
        details <- results[which(results != "coefficients")]
        results_details <- list(model1[c(match(details, names(model1)))])
        results_details <- c(results_details, lapply(models, function(X) return(X[c(match(details, names(X)))])))
        names(results_details) <- unlist(lapply(convert.to.character(seq_series, series), paste, collapse=" - "))
        #return the coefficients and the details
        return(list("Results"=results_out, "Details"=results_details))
    } else {
        #Just return the coefficients
        return(results_out)
    }

}