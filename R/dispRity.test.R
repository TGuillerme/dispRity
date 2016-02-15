#' @name dispRity.test
#' @aliases bhatt.coeff sequential.test null.test
#'
#' @title Disparity tests
#'
#' @usage bhatt.coeff(x, y, bw = bw.nrd0, ...)
#' sequential.test(series, results = "coefficients", family, ...)
#' null.test(...)
#'
#' @description Different implemented tests for comparing series.
#'
#' @param x,y two distributions.
#' @param bw the bandwith size, either a \code{numeric} or a \code{function} (see \code{\link[stats]{bw.nrd0}}).
#' @param series time series of which to estimate the slopes sequentially.
#' @param results which results from the \code{\link[stats]{glm}} to display (default = \code{"coefficients"}).
#' @param family the family of the \code{\link[stats]{glm}}.
#' @param ... optional arguments to be passed to the functions.
#'
#' @details
#' These are inbuilt statistical tests for comparing disparity series:
#' \itemize{
#'   \item \code{bhatt.coeff}: calculates the Bhattacharyya Coefficient (probability of overlap) between two distributions.
#'   \item \code{sequential.test}: performs a sequential \code{\link[stats]{glm}} on the series by correcting for time autocorrelation. The time autocorrelation is corrected by estimating the intercept of the \code{\link[stats]{glm}} using a predicted intercept using the preceding \code{\link[stats]{glm}}.
#'   \item \code{null.test}: soon!
#' }
#'
#' @examples
#' ## Bhattacharyya Coefficient:
#' ## Two dummy distributions
#' x <- rnorm(1000, 0, 1) ; y <- rnorm(1000, 1, 2)
#' ## What is the probability of overlap of these distributions?
#' bhatt.coeff(x, y)
#'
#' ## Sequential test:
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
#' @seealso \code{\link{test.dispRity}}.
#'
#' @references
#' Bhattacharyya A. 1943. On a measure of divergence between two statistical populations defined by their probability distributions. Bull. Calcutta Math. Soc., \bold{35}, pp. 99–-109
#'
#' @author Thomas Guillerme

#Calculate the Bhattacharyya Coefficient
bhatt.coeff<-function(x, y, bw=bw.nrd0, ...) {
    #SANITIZING
    #x and y
    check.class(x, "numeric")
    check.class(y, "numeric")
    
    #bw
    if(class(bw) == "numeric") {
        check.length(bw, 1, " must be either a single numeric value or a function.")
         bw<-round(bw)
    } else {
        check.class(bw, "function", " must be either a single numeric value or a function.")
    }

    #BHATTACHARYYA COEFFICIENT
    #sum(sqrt(x relative counts in bin_i * y relative counts in bin_i))

    #Setting the right number of bins (i)
    if(class(bw) == 'function') {
        #Bin width
        band.width<-bw(c(x,y), ...)
        #Bin breaks
        bin.breaks<-seq(from=min(c(x,y)), to=max(c(x,y)+band.width), by=band.width) #adding an extra bandwith to the max to be sure to include all the data
        #Number of bins
        bin.n<-length(bin.breaks)-1
    } else {
        #Bin breaks
        bin.breaks<-hist(c(x,y), breaks=bw, plot=F)$breaks
        #Bin width
        band.width<-diff(bin.breaks)[1]
        #Number of bins
        bin.n<-bw
    }

    #Counting the number of elements per bin
    histx<-hist(x, breaks=bin.breaks, plot=FALSE)[[2]]
    histy<-hist(y, breaks=bin.breaks, plot=FALSE)[[2]]
    #Relative counts
    rel.histx<-histx/sum(histx)
    rel.histy<-histy/sum(histy)
    
    #Calculating the Bhattacharyya Coefficient (sum of the square root of the multiple of the relative counts of both distributions)
    bhatt.coeff<-sum(sqrt(rel.histx*rel.histy))
    return(bhatt.coeff)
#End
}

# Calculate the time-correlated lm test:
# 1 - apply a logistic regression to the first series
# 2 - save the slope + the intercept for series 1
# 3 - estimate the intercept for the 2nd series using slope 1 + intercept 1
# 4 - save slope for series 2
# 5 - estimate the intercept for the 3rd series using slope 2 + intercept 2
# 6 - etc...
# 
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

    #FUNCTIONS

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

#Calculate the null model deviation test
make.null <- function(...) {
    # Series can be an observed value or a bootstrapped one

    # null.rule must be the type of null model (or the null model "rule") (can invoke space.maker)

    # replicates (of the null.rule)

    # ... any optionals to be passed to test space.maker

    #Final example (should look like that)
    #test.dispRity(sum_of_ranges, test = randtest, "null", null = make.null())
}






# Add a hypervolume::hypervolume_distance test (i.e. is the distance between two groups significantly different than 0?)

# Add the hypervolume::hypervolume_inclusion_test (i.e. is one group par of the other?)
