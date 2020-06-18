# #' @title Test disparity metric
# #'
# #' @description Test whether a metric captures changes trait space size, density and position.
# #'
# #' @param data A matrix or a \code{dispRity} object (see details).
# #' @param metric A vector containing one to three functions. At least of must be a dimension-level 1 or 2 function (see details).
# #' @param shifts The types of shits to test, can be \code{"random"}, \code{"size"}, \code{"density"} and \code{"position"}.
# #' @param replicates Optional, a \code{numeric} number of replicates to increase variance.
# #' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
# #' @param ... Optional arguments to be passed to the metric.
# #' @param verbose A \code{logical} value indicating whether to be verbose or not.
# #' 
# #' @details
# #' This function tests the behaviour of the input \code{metric} on the full dataset provided from \code{data}.
# #' The shifting reduces the space by removing 10% of the dataset from 100% to 10% as described in Guillerme et al. 2020.
# #' A linear model is then fitted to the dataset to calculate whether the metric shows a significant change.
# #' 
# #' @examples
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' 
# #' @references
# #' Guillerme T, Puttick MN, Marcy AE, Weisbecker V. \bold{2020} Shifting spaces: Which disparity or dissimilarity measurement best summarize occupancy in multidimensional spaces?. Ecol Evol. 2020;00:1-16. (doi:10.1002/ece3.6452)

# test.dispRity <- function(data, metric, shifts, replicates, dimensions, ..., verbose) {

#     ## Saving the call
#     match_call <- match.call()
#     dots <- list(...)

#     # warning("DEBUG") ; return(match_call)

#     ## Check data input
#     if(!is(data, "dispRity")) {
#         data <- fill.dispRity(make.dispRity(data = check.dispRity.data(data)))
#     } else {
#         ## Making sure matrix exist
#         if(is.null(data$matrix[[1]])) {
#             stop.call(match_call$data, " must contain a matrix or a list of matrices.")
#         }
#         ## Make sure dimensions exist in the call
#         if(is.null(data$call$dimensions)) {
#             data$call$dimensions <- ncol(data$matrix[[1]])
#         }
#     }

#     ## Get the metric list
#     metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data$matrix[[1]]), ...)
#     # metrics_list <- get.dispRity.metric.handle(metric, match_call, data.dim = dim(data$matrix[[1]]))


#     return()
# }