#' @title Disparity time series changes
#'
#' @description Check for changes in disparity time series
#'
#' @param data a \code{"dispRity"} object with time series (e.g. from \code{chrono.subsets}) and disparity values
#' @param method the method for measuring change (either \code{"itsa"}, \code{"citsa"}, \code{"area"}, \code{"average"} or any generic \code{h.test} function). See details.
#' @param changepoint
#' @param time.window
#' @param ... A named list of specific options for each different method. See details.
#' 
#' @details
#' The implemented disparity time series changes are:
#' \itemize{
#'      \item \code{"itsa"}  @@@...
#'      \item \code{"citsa"} @@@... 
#'      \item \code{"area"}  @@@...
#'      \item \code{"average"}  @@@...
#' }
#' 
#' Each method can take the following specific arguments
#' 
#' \itemize{
#'      \item \code{"itsa"}  @@@...
#'      \item \code{"citsa"} @@@... 
#'      \item \code{"area"}  @@@...
#'      \item \code{"average"}  @@@...
#' }
#' 
#' @returns
#' 
#' @@@
#' 
#' @examples
#' 
#' @@@
#' 
#' @seealso \code{\link{chrono.subsets}} \code{\link{dispRity}}
#' 
#' @author Caleb Scutt, Thomas Guillerme
#' @references
#' @@@
#' 


chrono.test <- function(data, method, changepoint, time.window, ...) {
    match_call <- match.call()
    ##############
    # SANITISING #
    ##############

    ## start with data
    check.class(data, "dispRity") ## do all the classic dispRity checking, is_multi etc, make sure it has a tree and a matrix.
    mat <- get.matrix(data)
    tree <- get.tree(data)



    check.time(changepoint, c("numeric", "integer", ))
    check.length(changepoint, 1, " must be a single numeric value.")
    # check.class(replicates, c("numeric"))
    # check.length(replicates, 1, " must be a single numeric value.")
    methods <- c("itsa", "citsa", "area", "h.test")
    check.method(slice.model, slice_models, "slice.model argument")



    #######################################################################################################

    changepoint <- set.changepoint(changepoint) ## set changepoint

    delta_df <- make.deltatronic(data, changepoint)

    if(!is.null(time.window)) {
        delta_df <- set.time.window(delta_df, time.window, changepoint)
    }


    






}

## sanitizing function for changepoint + time.window
