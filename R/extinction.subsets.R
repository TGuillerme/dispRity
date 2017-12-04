#' @title Getting the time subsets from at and after an extinction event
#'
#' @description Getting the reference (pre-extinction) and the comparison (post-extinction) time subsets
#'
#' @param data a \code{dispRity} object.
#' @param extinction \code{numerical}, the time at the extinction event.
#' @param lag \code{numerical}, the lag effect (i.e. how many subsets after the extinction to consider - default = \code{1}).
#' @param names \code{logical}, whether to display the bins names (\code{TRUE}) or not (\code{FALSE} - default).
#' @param as.list \code{logical}, whether to output the results as a list for \code{\link{test.dispRity}} (\code{TRUE}) or not (\code{FALSE} - default).
#' 
#' @examples
#' ## Loading some disparity data
#' data(disparity)
#' 
#' ## Time subsets for the K-Pg extinction (66 Mya)
#' extinction.subsets(disparity, 66, names = TRUE)
#' 
#' ## Extinction with a lag effect of 3 slices
#' extinction_time <- extinction.subsets(disparity, 66, lag = 3, as.list = TRUE)
#' 
#' ## Testing the extinction effect with a lag
#' test.dispRity(disparity, wilcox.test, comparisons = extinction_time,
#'               correction = "bonferroni")
#' 
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

extinction.subsets <- function(data, extinction, lag = 1, names = FALSE, as.list = FALSE) {

    ## Bins or slices
    is_bins <- ifelse(data$call$subsets[1] == "discrete", TRUE, FALSE)

    if(is_bins) {
        ## Extinction bins
        extinction_bins <- grep(paste0(" - ", extinction), names(data$subsets))

        ## Check if extinction bin is in the data
        if(length(extinction_bins) == 0) {
            ## Detect the bin before the extinction time
            bin_ages <- which(detect.bin.age(data, extinction, greater = TRUE) == TRUE)
            extinction_subset<- bin_ages[length(bin_ages)]
        }
    } else{
        ## Extinction slices
        bin_ages <- which(as.numeric(names(data$subsets)) > extinction)
        extinction_subset <- bin_ages[length(bin_ages)]

    }

    ## Adding the lag effect bins
    extinction_subset <- seq(from = extinction_subset, to = extinction_subset+lag)

    ## Returning the names or numbers
    if(names) {
        extinction_subset <- names(data$subsets)[extinction_subset]
    }

    ## Returning a list or not
    if(as.list) {
        extinction_subset <- sapply(extinction_subset[-1], function(comp, ref) c(ref, comp), ref = extinction_subset[1], simplify = FALSE)
    }

    return(extinction_subset)

}