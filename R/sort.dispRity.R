#' @title Sorts a \code{dispRity} object.
#'
#' @description Sorts the series of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param decreasing \code{logical}. Should the sort be increasing or decreasing? Not available for partial sorting
#' @param sort an optional \code{vector} of \code{numeric} values corresponding to the order in which to return the series..
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat99) ; data(BeckLee_tree) 
#'
#' ## Split the data
#' series <- time.series(data = BeckLee_mat99, tree = BeckLee_tree,
#'       method = "continuous", time = 5, model = "acctran")
#'
#' ## Calculating the sum of centroids
#' disparity <- dispRity(series, metric = mean)
#' 
#' ## Sorting the data
#' summary(disparity)
#' summary(sort(disparity, decreasing = TRUE))
#' summary(sort(disparity, sort = c(1,3,4,5,2)))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{\link{plot.dispRity}}, \code{\link{get.dispRity}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme
#' @export

sort.dispRity<-function(data, decreasing = FALSE, sort) {

    #Initialising series length variable
    length_series <- length(data$series)

    #Sanitizing

    #data
    check.class(data, "dispRity")
    if(length_series == 1) stop("Data contains only one series.")

    #decreasing
    check.class(decreasing, "logical")

    #sort
    if(!missing(sort)) {
        check.class(sort, "numeric")
        check.length(sort, length_series, " must be the same length as the number of series in data.")
        if(all.equal(sort(sort), seq(from = 1, to = length_series)) != TRUE) {
            stop(paste("Sort argument can only contain unique numbers between 1 and ", length_series, ".", sep = ""))
        }
    } else {
        if(decreasing == FALSE) sort <- seq(from = 1, to = length_series)
        if(decreasing == TRUE) sort <- rev(seq(from = 1, to = length_series))
    }

    #Sorting the series names
    data$series <- data$series[sort]
    #Sorting the observed disparity data
    if(!is.null(names(data$disparity$observed))) {
        data$disparity$observed <- data$disparity$observed[sort]
    } else {
        data$disparity$observed <- lapply(data$disparity$observed, lapply, recursive.sort, sort = sort)
    }

    #Sorting the bootstrapped disparity data (if needed)
    if(any(names(data$disparity) == "bootstrapped")) {
        if(!is.null(names(data$disparity$bootstrapped))) {
            data$disparity$bootstrapped <- data$disparity$bootstrapped[sort]
        } else {
            data$disparity$bootstrapped <- lapply(data$disparity$bootstrapped, lapply, recursive.sort, sort = sort)
        }
    }
    return(data)
}


