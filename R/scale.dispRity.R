#' @title Scaling disparity results.
#'
#' @description Divides calculated disparity by the maximum disparity.
#'
#' @param data a \code{dispRity} object.
#' @param max either a \code{logical} value or a single \code{numeric} value for dividing the data. If \code{TRUE}, the data is divided by the maximum value of \code{data} is used.
# ' @param center either a \code{logical} value or a \code{numeric} vector of length equal to the number of elements of \code{data} (default is \code{FALSE}).
# ' @param scale either a \code{logical} value or a \code{numeric} vector of length equal to the number of elements of \code{data} (default is \code{FALSE}).
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Setting up two customised series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 7,
#'      rarefaction = c(10, 25))
#' ## Calculating the sum of centroids
#' disparity <- dispRity(bootstrapped_data, metric = c(sum, centroids))
#' 
#' ## Scaling the data
#' summary(disparity) # No scaling
#' summary(scale(disparity)) # Dividing by the maximum
#' ## Multiplying by 10 (dividing by 0.1)
#' summary(scale.dispRity(disparity, max = 0.1))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{link[base]{scale}}.
#'
#' @author Thomas Guillerme
#' @export

scale.dispRity<-function(data, max = TRUE) {#, center = FALSE, scale = FALSE) {

    #Sanitizing

    #data
    check.class(data, "dispRity")

    #is bootstrapped?
    is.bootstrapped <- ifelse(any(names(data$disparity) == "bootstrapped"), TRUE, FALSE)

    #Extracting all the data
    if(max == TRUE) {
        ext_data <- unlist(extract.dispRity(data, observed = TRUE))
        if(is.bootstrapped == TRUE) {
            ext_data <- c(ext_data, unlist(extract.dispRity(data, observed = FALSE)))
        }
        max <- max(ext_data)
    } else {
        if(class(max) == "numeric") {
            check.length(max, 1, " must be either logical or a single numeric value.")
        }
    }

    #Lapply functions
    lapply.max <- function(X, max) {return(X/max)}
    # lapply.scale <- function(X, center, scale) {return(scale(X, center, scale))}

    #Divide disparity by the max disparity
    if(max != FALSE) {
        data$disparity$observed <- lapply(data$disparity$observed, lapply, lapply, lapply.max, max)
        if(is.bootstrapped == TRUE) {
            data$disparity$bootstrapped <- lapply(data$disparity$bootstrapped, lapply, lapply, lapply.max, max)
        }
    }

    #Scaling disparity
    # if(any(c(scale, center) == TRUE)) {
    #     data$disparity$observed <- lapply(data$disparity$observed, lapply, lapply, lapply.scale, max)
    #     if(is.bootstrapped == TRUE) {
    #         data$disparity$bootstrapped <- lapply(data$disparity$bootstrapped, lapply, lapply, lapply.max, max)
    #     }        
    # } 

    return(data)
}

