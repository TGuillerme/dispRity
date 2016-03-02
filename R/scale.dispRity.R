#' @title Scaling disparity results.
#'
#' @description Divides calculated disparity by the maximum disparity.
#'
#' @param data a \code{dispRity} object.
#' @param max an optional value for scaling. If missing the maximum disparity from \code{data} is used.
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Calculate disparity as the sum of the centroids
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 7,
#'      rarefaction = c(10, 25))
#' ## Calculating the sum of variances
#' disparity <- dispRity(bootstrapped_data, metric = c(sum, centroids))
#' 
#' ## Scaling the data
#' summary(disparity)
#' summary(scale.dispRity(disparity))
#'
#' @seealso \code{\link{dispRity}} and \code{\link{test.dispRity}}.
#'
#' @author Thomas Guillerme
#' @export

scale.dispRity<-function(data, max) {

    #Sanitizing

    #data
    check.class(data, "dispRity")

    #is bootstrapped?
    if(any(names(data$disparity) == "bootstrapped")) {
        is.bootstrapped <- TRUE
    } else {
        is.bootstrapped <- FALSE
    }

    #Extracting all the data
    if(missing(max)) {
        ext_data <- unlist(extract.dispRity(data, observed = TRUE))
        if(is.bootstrapped == TRUE) {
            ext_data <- c(ext_data, unlist(extract.dispRity(data, observed = FALSE)))
        }
        max <- max(ext_data)
    }

    lapply.scale <- function(X, max) {return(X/max)}

    #Divide disparity by the max disparity
    data$disparity$observed <- lapply(data$disparity$observed, lapply, lapply, lapply.scale, max)
    if(is.bootstrapped == TRUE) {
        data$disparity$bootstrapped <- lapply(data$disparity$bootstrapped, lapply, lapply, lapply.scale, max)
    }

    return(data)
}

