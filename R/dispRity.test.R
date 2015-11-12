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



# Anderson.test<-function(BSresults, time_pco) {
#     #Disparity T-Test calculation from Anderson and Friedman 2012.
#     #Code modified from Smith et al. 2014 - Evolution

#     #Extracting the sample sizes
#     sample_size<-unlist(lapply(time_pco, nrow))

#     #Getting the mean and the variance from the BSresults
#     variance_int<-apply(BSresults, 2, var)
#     mean_int<-apply(BSresults, 2, mean)

#     #Calculating the T_statistics functions
#     mean.difference<-function(x,y, mean_int) {mean_int[x]-mean_int[y]}
#     term.A<-function(x,y,sample_size, variance_int) { ((sample_size[x]-1)*(sample_size[x])* variance_int[x] + (sample_size[y]-1)*(sample_size[y])* variance_int[y] )/(sample_size[x]+sample_size[y]+2) }
#     term.B<-function(x,y, sample_size) { (sample_size[x] + sample_size [y])/(sample_size[x] * sample_size [y]) }

#     #Calculating the statistic, df and p-value.
#     difference<-p_values<-degrees_freedom<-t_statistic<-as.data.frame(matrix(NA, nrow=ncol(BSresults), ncol=ncol(BSresults)))
#     rownames(difference)<-rownames(p_values)<-rownames(degrees_freedom)<-rownames(t_statistic)<-names(time_pco)
#     colnames(difference)<-colnames(p_values)<-colnames(degrees_freedom)<-colnames(t_statistic)<-names(time_pco)
#     for(row in 1:ncol(BSresults)) {
#         for(col in 1:ncol(BSresults)) {
#             #Calculate difference
#             difference[row,col]<-mean.difference(row,col, mean_int)
#             #Calculate T
#             t_statistic[row,col]<-mean.difference(row,col, mean_int)/sqrt(term.A(row,col,sample_size,variance_int)*term.B(row,col,sample_size))
#             if(!is.finite(t_statistic[row,col])) {
#                 #Exist the loop if some variance or differences are not finit numbers.
#                 message("T statistic cannot be calculate. Probable reason: some values are is near Inf or -Inf.")
#                 return(list("diff"=difference, "df"=degrees_freedom, "T"=t_statistic, "p"=p_values))
#             }

#             #Calculate df
#             degrees_freedom[row,col]<-sample_size[row]+sample_size[col]-2
#             #Calculate p
#             p_values[row,col]<- 1-pt(t_statistic[row, col], df = degrees_freedom[row, col])

#             #make test two-tailed
#             if (p_values [row,col] > 0.5) {
#                 p_values [row,col] <- 2*(1-p_values[row,col])
#             } else {
#                 if (p_values [row,col] < 0.5){
#                     p_values [row,col] <- 2*(p_values[row,col])   
#                 } else {
#                     if (p_values [row,col] == 0.5){
#                         p_values [row,col] <- 1
#                     }
#                 }
#             }
#         }
#     }
#     return(list("diff"=difference, "df"=degrees_freedom, "T"=t_statistic, "p"=p_values))
# }