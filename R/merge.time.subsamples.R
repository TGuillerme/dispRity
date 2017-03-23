# #' @title Combines or cleans time subsamples.
# #' 
# #' @usage merge.time.subsamples(subsamples1, subsamples2, after = TRUE)
# #'
# #' @description Combines two time subsamples sequentially or cleans a single time subsamples by merging any subsamples with less than 3 taxa to the previous or next subsamples.
# #'
# #' @param subsamples1 An ordinated matrix of maximal dimensions \eqn{k*(k-1)}, or a \code{dispRity} object (see details).
# #' @param subsamples2 An optional matrix of maximal dimensions \eqn{k*(k-1)}, or a \code{dispRity} object (see details). If missing \code{data1} will be cleaned.
# #' @param after A \code{logical} value indicating whether to be merge before or after the first subsamples (\code{default = TRUE}).
# #'
# #' @return
# #' This function outputs a \code{dispRity} object containing:
# #' \item{data}{A \code{list} of the split ordinated data (each element is a \code{matrix}).}
# #' \item{elements}{A \code{vector} containing all the rownames from the input matrix.}
# #' \item{subsamples}{A \code{vector} containing the name of the subsamples.}
# #' \code{dispRity} objects can be summarised using \code{print} (S3).
# #' 
# #' @details  
# #' The \code{dispRity} object given to the \code{data} argument can be: a list of matrices (typically output from the functions \code{\link{time.subsamples}} or \code{\link{cust.subsamples}}), a bootstrapped matrix output from \code{\link{boot.matrix}} or a list of disparity measurements calculated from this \code{dispRity} function.
# #' 
# #' @examples
# #' ## Load the disparity data
# #' data(disparity)
# #' 
# #' ## Merging the two first subsamples
# #' merge.time.subsamples(disparity$subsamples[[1]], disparity$subsamples[[2]])
# #' 
# #' @seealso \code{\link{cust.subsamples}}, \code{\link{time.subsamples}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
# #'
# #' @author Thomas Guillerme

# #For testing
# #source("sanitizing.R")
# #source("merge.time.subsamples_fun.R")

# # data1 <- time_binsEQ_Beck
# # data2 <- time_binsEQ_Beck
# # after = TRUE

# merge.time.subsamples <- function(data1, data2, after = TRUE) {
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
#     # MERGING subsamples
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

#         #subsamples type
#         if(data1$subsamples[1] == data2$subsamples[1]) {
#             if(data1$subsamples[1] == "discrete" | data1$subsamples[1] == "continuous") {
#                 subsamples_type <- data1$subsamples[1]
#             } else {
#                 subsamples_type <- "composite"
#             }
#         } else {
#             subsamples_type <- "composite"
#         }

#         #subsamples combination
#         if(after) {
#             new_subsamples <- c(subsamples_type, data1$subsamples[-1], data2$subsamples[-1])
#         } else {
#             new_subsamples <- c(subsamples_type, data2$subsamples[-1], data1$subsamples[-1])
#         }

#         #combining the new subsamples
#         output <- list("data" = new_data, "elements" = new_elements, "subsamples" = new_subsamples)
#         #output
#         class(output) <- "dispRity"
#         return(output)
#     } 
#     #----------------------
#     # CLEANING subsamples
#     #----------------------       
#     if(!merge_function_mode) {

#         # Set up an iterative counter for safety (avoid infinite loop!)
#         itterative_counter <- 1
#         max_itteration <- length(data1$data) - 1

#         while(any(unlist(lapply(data1$data, nrow)) < 3)) {
#             # Clean the subsamples
#             data1 <- clean.subsamples(data1, after)
#             # Update the counter
#             itterative_counter <- itterative_counter + 1
#             if(itterative_counter == max_itteration) {
#                 stop("Impossible to clean the subsamples. It seems there is not enough data!")
#             }
#         }
#         return(data1)
#     }
# }