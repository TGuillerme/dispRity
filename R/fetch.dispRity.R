#' @name fetch.dispRity
#' @title Fetching elements of a \code{dispRity} object.
#' @aliases fetch.matrix fetch.elements fetch.series
#'
#' @description Fetching the matrix, elements or series of a \code{dispRity} object.
#'
#' @param dispRity A \code{dispRity} object.
#' @param series A \code{numeric} value to select a series (\code{0} is no series; default).
#' @param rarefaction A \code{numeric} value to select the rarefaction level (\code{0} is no rarefaction; default).
#' @param bootstrap A \code{numeric} value to select a specific bootstrap draw (\code{0} is no bootstrap; default).
#' 
#' @usage fetch.matrix(dispRity, series, rarefaction, bootstrap)
#' fetch.elements(dispRity, series)
#' fetch.series(dispRity)
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' 
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping and rarefying the data
#' dispRity_object <- boot.matrix(customised_series, bootstraps = 100,
#'      rarefaction = c(15, 10))
#' 
#' ## To get the original matrix
#' fetch.matrix(dispRity_object, 0,0,0)
#' # or simply fetch.matrix(dispRity_object)
#' ## To get the matrix of the second series, first rarefaction, 58th bootstrap
#' fetch.matrix(dispRity_object, 2,1,58)
#' ## To get the elements in the first series
#' fetch.elements(dispRity_object, 1)
#' ## To get the series
#' fetch.series(dispRity_object)
#' 
#' @author Thomas Guillerme

fetch.matrix <- function(dispRity, series = 1, rarefaction = 1, bootstrap = 1){
    if(bootstrap == 0) {
        return(dispRity$matrix[dispRity$series[[series]]$elements, 1:dispRity$call$dimensions])
    } else {
        return(dispRity$matrix[dispRity$series[[series]][[rarefaction+1]][,bootstrap], 1:dispRity$call$dimensions])
    }
}

fetch.elements <- function(dispRity, series = 1){
    return(dispRity$series[[series]]$elements)
}

fetch.series <- function(dispRity){
    return(names(dispRity$series))
}