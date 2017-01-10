# #' @title Combines or cleans time series.
# #' 
# #' @usage merge.time.series(series1, series2, after = TRUE)
# #'
# #' @description Combines two time series sequentially or cleans a single time series by merging any series with less than 3 taxa to the previous or next series.
# #'
# #' @param series1 An ordinated matrix of maximal dimensions \eqn{k*(k-1)}, or a \code{dispRity} object (see details).
# #' @param series2 An optional matrix of maximal dimensions \eqn{k*(k-1)}, or a \code{dispRity} object (see details). If missing \code{data1} will be cleaned.
# #' @param after A \code{logical} value indicating whether to be merge before or after the first series (\code{default = TRUE}).
# #'
# #' @return
# #' This function outputs a \code{dispRity} object containing:
# #' \item{data}{A \code{list} of the split ordinated data (each element is a \code{matrix}).}
# #' \item{elements}{A \code{vector} containing all the rownames from the input matrix.}
# #' \item{series}{A \code{vector} containing the name of the series.}
# #' \code{dispRity} objects can be summarised using \code{print} (S3).
# #' 
# #' @details  
# #' The \code{dispRity} object given to the \code{data} argument can be: a list of matrices (typically output from the functions \code{\link{time.series}} or \code{\link{cust.series}}), a bootstrapped matrix output from \code{\link{boot.matrix}} or a list of disparity measurements calculated from this \code{dispRity} function.
# #' 
# #' @examples
# #' ## Load the disparity data
# #' data(disparity)
# #' 
# #' ## Merging the two first series
# #' merge.time.series(disparity$series[[1]], disparity$series[[2]])
# #' 
# #' @seealso \code{\link{cust.series}}, \code{\link{time.series}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
# #'
# #' @author Thomas Guillerme

# #For testing
# #source("sanitizing.R")
# #source("merge.time.series_fun.R")

# # data1 <- time_binsEQ_Beck
# # data2 <- time_binsEQ_Beck
# # after = TRUE

# merge.time.series <- function(data1, data2, after = TRUE) {
#     #----------------------
#     # SANITIZING
#     #----------------------
#     #DATA
#     check.data.to.merge(data1)
#     if(!missing(data2)) {
#         check.data.to.merge(data2)
#         merge_function_mode <- TRUE
#     } else {
#         merge_function_mode <- FALSE
#     }

#     #previous
#     check.class(after, "logical")

#     #----------------------
#     # MERGING SERIES
#     #----------------------
#     if(merge_function_mode) {
#         #data
#         if(after) {
#             new_data <- c(data1$data, data2$data)
#         } else {
#             new_data <- c(data2$data, data1$data)
#         }

#         #elements
#         new_elements <- unique(c(data1$elements, data2$elements))

#         #series type
#         if(data1$series[1] == data2$series[1]) {
#             if(data1$series[1] == "discrete" | data1$series[1] == "continuous") {
#                 series_type <- data1$series[1]
#             } else {
#                 series_type <- "composite"
#             }
#         } else {
#             series_type <- "composite"
#         }

#         #series combination
#         if(after) {
#             new_series <- c(series_type, data1$series[-1], data2$series[-1])
#         } else {
#             new_series <- c(series_type, data2$series[-1], data1$series[-1])
#         }

#         #combining the new series
#         output <- list("data" = new_data, "elements" = new_elements, "series" = new_series)
#         #output
#         class(output) <- "dispRity"
#         return(output)
#     } 
#     #----------------------
#     # CLEANING SERIES
#     #----------------------       
#     if(!merge_function_mode) {

#         # Set up an iterative counter for safety (avoid infinite loop!)
#         itterative_counter <- 1
#         max_itteration <- length(data1$data) - 1

#         while(any(unlist(lapply(data1$data, nrow)) < 3)) {
#             # Clean the series
#             data1 <- clean.series(data1, after)
#             # Update the counter
#             itterative_counter <- itterative_counter + 1
#             if(itterative_counter == max_itteration) {
#                 stop("Impossible to clean the series. It seems there is not enough data!")
#             }
#         }
#         return(data1)
#     }
# }