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
#' @param data a \code{dispRity} object
#' @param ... optional arguments to be passed to the \code{\link[stats]{glm}}.
#'
#' @details
#' This test allows to correct for time autocorrelation by estimating the intercept of the \code{\link[stats]{glm}} using a predicted intercept using the preceding \code{\link[stats]{glm}}.
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


sequential.test <- function(series, results="coefficients", family, ...) {

    #SANITIZING
    #results must be at least coefficients!
    if(is.na(match("coefficients", results))) {
        results <- c(results, "coefficients")
    }

    #Family
    if(missing(family)) {
        stop("glm family type argument is necessary!")
    }
    if(family(link="identity")[[1]] == "gaussian") {
        warning("Model family is set to gaussian, should it not be binomial?")
    }

    #APPLYING THE SEQUENTIAL TEST

    #Setting the sequence
    seq_series <- set.sequence(length(series))
    seq_series <- unlist(apply(seq_series, 2, list), recursive=FALSE)

    #Applying the first test to get the intercept origin
    model1 <- create.model(set.pair.series(series[seq_series[[1]]]), family, ...)
    #model1 <- create.model(set.pair.series(series[seq_series[[1]]]), family) ; warning("DEBUG")
    
    #Set origin intercept for the following models
    intercept_predict <- NULL
    if(summary(model1)$coefficients[1,4] < 0.05) {
        #If intercept is significant
        if(summary(model1)$coefficients[2,4] < 0.05) {
            #If slope is significant
            intercept_predict <- intercept.estimate(coef(model1)[1], coef(model1)[2])
        } else {
            #intercept 1 is just intercep
            intercept_predict <- coef(model1)[1]
        }
    } else {
        #Intercept is just 0
        #If intercept is significant
        if(summary(model1)$coefficients[2,4] < 0.05) {
            #If slope is significant
            intercept_predict <- intercept.estimate(0, coef(model1)[2])
        } else {
            #intercept 1 is just intercep
            intercept_predict <- 0
        }
    }

    #Loop through the other models
    models <- list(NULL)
    for(model in 2:length(seq_series)) {
        #Create the new model
        models[[model-1]] <- create.model(data = set.pair.series(series[seq_series[[model]]], intercept=intercept_predict[model-1]), family, intercept = intercept_predict[model-1], ...)
        #models[[model-1]] <- create.model(data = set.pair.series(series[seq_series[[model]]], intercept=intercept_predict[model-1]), family, intercept = intercept_predict[model-1]) ; warning("DEBUG")

        #Predict the new intercept
        intercept_predict <- c(intercept_predict, intercept.estimate(coef(model1)[1], c(coef(model1)[2], unlist(lapply(models, coef)) )))
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
        Intercept_results[,1] <- c(NA, intercept_predict[-1])
    }

    #Saving the slopes
    Slope_results <- Matrix_template
    Slope_results[1,] <- model1_results$coefficients[2,]
    colnames(Slope_results) <- names(model1_results$coefficients[2,])

    #Adding the other slopes
    if(length(seq_series) > 1) {
        #Adding the slopes
        for(model in 2:length(seq_series)) {
            Slope_results[model,] <- models_results[[model-1]]$coefficients
        }
    }

    #Combining the tables
    results_out <- list("Intercept"=Intercept_results, "Slope"=Slope_results)

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


    #Some ploting
    # plot(series_1_2[,1:2])
    # abline(mod)
    # curve(predict(mod1, data.frame(data=x),type="response"),add=TRUE) 
    # curve(predict(mod2, data.frame(intercept=intercept, data=x), type="response"), add=TRUE,col="red")


    #ADD A FUNCTION FOR ADDING THE RESULTS TO A PLOT!

}