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

# t.test.smith<-function(x, y, ...) {
#     #Disparity T-Test calculation from Anderson and Friedman 2012.
#     #Code modified from Smith et al. 2014 - Evolution

#     #SANITIZING
#     #x and y
#     check.class(x, "numeric")
#     check.class(y, "numeric")

#     #TESTING
#     #Mean difference
#     mean_difference <- abs(mean(x) - mean(y))

#     #Degree of freedom
#     degree_freedom <- length(x) + length(y) - 2

#     #T statistic
#     term_A <- ((length(x)-1) * length(x) * var(x) + (length(y)-1) * length(y) * var(y)) / degree_freedom
#     term_B <- (length(x) + length(y))/(length(x) * length(y))
#     t_statistic <- mean_difference/sqrt(term_A*term_B)

#     #p value
#     p_value <- 1-pt(t_statistic, df = degree_freedom)

#     #make test two-tailed
#     if (p_value > 0.5) {
#         p_value <- 2*(1-p_value)
#     } else {
#         if (p_value < 0.5){
#             p_value <- 2*(p_value)   
#         } else {
#             if (p_value == 0.5){
#                 p_value <- 1
#             }
#         }
#     }

#     #Output
#     table_out <- as.data.frame(matrix(c(mean_difference, p_value, degree_freedom, t_statistic), nrow=1, ncol=4))
#     names(table_out) <- c("diff", "p_value", "df", "T")
#     return(table_out)
# }


# Add a hypervolume::hypervolume_distance test (i.e. is the distance between two groups significantly different than 0?)

# Add the hypervolume::hypervolume_inclusion_test (i.e. is one group par of the other?)

# Add the time-correlated lm test:
# 1 - apply a logistic regression to the first series
# 2 - save the slope + the intercept for series 1
# 3 - estimate the intercept for the 2nd series using slope 1 + intercept 1
# 4 - save slope for series 2
# 5 - estimate the intercept for the 3rd series using slope 2 + intercept 2
# 6 - etc...