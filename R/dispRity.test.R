#' @name dispRity.test
#' @aliases bhatt.coeff
#'
#' @title Disparity tests
#'
#' @description Different implemented tests for comparing series.
#'
#' @param x,y two distributions.
#' @param bw the bandwith size, either a \code{numeric} or a \code{function} (see \code{\link[stats]{bw.nrd0}}).
#' @param ... optional arguments to be passed to \code{bw}.
#'
#' @details
#' These are inbuilt statistical tests for comparing disparity series.
#' \itemize{
#'   \item \code{bhatt.coeff}: calculates the Bhattacharyya Coefficient (probability of overlap) between two distributions.
# '   \bold{aruments: }
# '   \itemize{
# '      \item \code{x,y}: the two distributions.
# '      \item \code{bw}: the bandwith size, either a \code{numeric} or a \code{function} (see \code{\link[stats]{bw.nrd0}}).
# '      \item \code{...}: Optional arguments to be passed to \code{bw}.
# '   }
#' }
#'
#' @examples
#' ## 
#'
#' @seealso \code{\link{test.dispRity}}.
#'
#' @references
#' Bhattacharyya A. 1943. On a measure of divergence between two statistical populations defined by their probability distributions. Bull. Calcutta Math. Soc., \bold{35}, pp. 99–-109
#'
#' @author Thomas Guillerme





# #Calculating each axis variance
# dispRity.tests <- function(X) return(X)


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

# Add the time-correlated lm test:
# 1 - apply a logistic regression to the first series
# 2 - save the slope + the intercept for series 1
# 3 - estimate the intercept for the 2nd series using slope 1 + intercept 1
# 4 - save slope for series 2
# 5 - estimate the intercept for the 3rd series using slope 2 + intercept 2
# 6 - etc...

#series <- extract.dispRity(data, observed=FALSE)

sequential.test <- function(series, results="coefficients", ...) {

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

    #Get the two first series
    series_pair1 <- set.pair.series(extracted_data[1:2])

    #Creating the first model
    model1 <- create.model(series_pair1, gaussian)
    message("Model is linear but should be logistic!")

    #Save the results
    model1_res <- save.results(model1, results)

    #Estimate the intercept for the second model
    intercept_1 <- intercept.estimate(coef(model1)[1], coef(model1)[2])


    #Get the second row of series
    series_pair2 <- set.pair.series(extracted_data[2:3], intercept=intercept_1)

    #Creating the second model
    model2 <- create.model(series_pair2, gaussian, intercept=intercept_1)
    message("Model is linear but should be logistic!")

    #Save the results
    model2_res <- save.results(model2, results)

    #Estimate the intercept for the third model
    intercept_2 <- intercept.estimate(coef(model1)[1], c(coef(model1)[2], coef(model2)[1]))


    #Get the third row of series
    series_pair3 <- set.pair.series(extracted_data[c(3,1)], intercept=intercept_2)

    #Creating the second model
    model3 <- create.model(series_pair3, gaussian, intercept=intercept_2)
    message("Model is linear but should be logistic!")

    #Save the results
    model3_res <- save.results(model3, results)

    #Estimate the intercept for the third model
    intercept_3 <- intercept.estimate(coef(model1)[1], c(coef(model1)[2], coef(model2)[1], coef(model3)[1]))

    # etc ...

    #add a securing for estimations (do not estimate if non-signif)
    #Estimating the intercept for the second series (if slope is significant)
    if(mod1_results[2,4] < 0.05) {

    } else {
        # Use the mod1 intercept (if significant)
        if(mod1_results[1,4] < 0.05) {
            intercept2 <- mod1_results[1,1]
        } else {
            intercept2 <- 0
        }
    }



    #Some ploting
    # plot(series_1_2[,1:2])
    # abline(mod)
    # curve(predict(mod1, data.frame(data=x),type="response"),add=TRUE) 
    # curve(predict(mod2, data.frame(intercept=intercept, data=x), type="response"), add=TRUE,col="red")

}






# Add a hypervolume::hypervolume_distance test (i.e. is the distance between two groups significantly different than 0?)

# Add the hypervolume::hypervolume_inclusion_test (i.e. is one group par of the other?)
