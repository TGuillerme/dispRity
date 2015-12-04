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

series <- extract.dispRity(data, observed=FALSE)

sequential.test <- function(series, ...) {

    #Get the two first series
    series_1_2 <- list.to.table(series[1:2], "binomial")

    #Creating the first model
    #mod1 <- glm(factor ~ data, data=series_1_2, family = "binomial")
    message("Model is linear but should be logistic!")
    mod1 <- lm(data ~ factor, data=series_1_2)

    #Save the results
    mod1_results <- summary(mod1)$coefficients
    #Allow to save multiple results!
    #mod1_aic <- summary(mod1)$aic

    #Estimate the intercept for the second model
    #intercept_1 <- predict(mod, data.frame(factor=1) ,type="response")
    intercept_1 <- coef(mod1)[1]+coef(mod1)[2]*1

    #Get the second and third series
    series_2_3 <- list.to.table(series[2:3], "binomial")
    #Add the intercept
    series_2_3$intercept <- as.numeric(intercept_1)

    #Create the second model
    #mod2 <- glm(factor ~ data - 1+offset(intercept), data=series_2_3, family = "binomial")
    message("Model is linear but should be logistic!")
    mod2 <- lm(data ~ factor - 1+offset(intercept), data=series_2_3)

    #Save the results
    mod2_results <- summary(mod2)$coefficients

    #Estimate the intercept for the third model
    #intercept_1 <- predict(mod, data.frame(factor=1) ,type="response")
    intercept_2 <- coef(mod1)[1]+coef(mod1)[2]*1+coef(mod2)[1]*1

    #Get the third (and first, for testing) series
    series_3_1 <- list.to.table(series[c(3,1)], "binomial")
    #Change 0 to 1 and other way around for the testing
    series_3_1$factor <- c(rep(0, 100), rep(1, 100))
    #Add the intercept
    series_3_1$intercept <- as.numeric(intercept_2)

    #Create the third model
    #mod3 <- glm(factor ~ data - 1+offset(intercept), data=series_3_1, family = "binomial")
    message("Model is linear but should be logistic!")
    mod3 <- lm(data ~ factor - 1+offset(intercept), data=series_3_1)

    #Save the results
    mod3_results <- summary(mod3)$coefficients



    #FUNCTIONS

    #Getting the data function

    #Estimating intercept function

    #Creating the model function

    #Saving results function



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
