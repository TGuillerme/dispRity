#' @name bhatt.coeff 
#'
#' @title Bhattacharyya Coefficient
#'
#' @description Calculates the probability of overlap between two distributions.
#'
#' @param x,y two distributions.
#' @param bw the bandwidth size, either a \code{numeric} or a \code{function} (see \code{\link[stats]{bw.nrd0}}).
#' @param ... optional arguments to be passed to the \code{bw} argument.
#'
#' @examples
#' ## Two dummy distributions
#' x <- rnorm(1000, 0, 1)
#' y <- rnorm(1000, 1, 2)
#'
#' ## What is the probability of overlap of these distributions?
#' bhatt.coeff(x, y)
#'
#' @seealso \code{\link{test.dispRity}}, \code{\link{null.test}}.
# \code{\link{sequential.test}}
#'
#' @references
#' Bhattacharyya A. \bold{1943}. On a measure of divergence between two statistical populations defined by their probability distributions. Bull. Calcutta Math. Soc., 35, pp. 99-109
#' 
#' @author Thomas Guillerme
# @export

bhatt.coeff <- function(x, y, bw = bw.nrd0, ...) {
    ## Check data
    ## x and y
    check.class(x, "numeric")
    check.class(y, "numeric")
    
    ## bw (bandwidth)
    if(is(bw, "numeric")) {
        check.length(bw, 1, " must be either a single numeric value or a function.")
        bw <- round(bw)
    } else {
        check.class(bw, "function", " must be either a single numeric value or a function.")
    }

    ## BHATTACHARYYA COEFFICIENT
    ## sum(sqrt(x relative counts in bin_i * y relative counts in bin_i))

    ## Setting the right number of bins (i)
    if(is(bw, "function")) {
        ## Bin width
        band.width <- bw(c(x, y), ...)
        ## Bin breaks
        ## adding an extra bandwith to the max to be sure to include all the data
        bin.breaks <- seq(from = min(c(x, y)), to = max(c(x, y) + band.width), by = band.width) 
        ## Number of bins
        bin.n <- length(bin.breaks) - 1
    } else {
        ## Bin breaks
        bin.breaks <- hist(c(x, y), breaks = bw, plot = FALSE)$breaks
        ## Bin width
        band.width <- diff(bin.breaks)[1]
        ## Number of bins
        bin.n <- bw
    }

    ## Counting the number of elements per bin
    histx <- hist(x, breaks = bin.breaks, plot = FALSE)[[2]]
    histy <- hist(y, breaks = bin.breaks, plot = FALSE)[[2]]
    ## Relative counts
    rel.histx <- histx / sum(histx)
    rel.histy <- histy / sum(histy)
    
    ## Calculating the Bhattacharyya Coefficient (sum of the square root of the multiple of the relative counts of both distributions)
    bhatt.coeff <- sum(sqrt(rel.histx * rel.histy))
    return(bhatt.coeff)
}