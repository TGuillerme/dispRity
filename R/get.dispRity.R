#' @title Extracting sub-samples from a dispRity object.
#'
#' @description Extracting sub-samples series and data from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param what A list of series names or series numbers to be extracted.
#' @param keep.elements \code{logical}, whether to keep the elements from the original matrix (default = \code{FALSE}).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the observed and bootstrapped matrices.}
#' \item{disparity}{A \code{list} of disparity values.}
#' \item{elements}{A \code{vector} containing all the names of the elements from the original matrix.}
#' \item{series}{A \code{vector} containing the name of the series (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstrapping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat99) ; data(BeckLee_tree) 
#'
#' ## Series sub-samples
#' series_full <- time.series(BeckLee_mat99, BeckLee_tree,
#'      method = "continuous",time = 5, model = "acctran")
#' series_full # 5 series for 99 elements
#' get.dispRity(series_full, what = 1) # 1 series for 3 elements
#'
#' ## Bootstrapped data sub-samples
#' bootstrapped_data <- boot.matrix(series_full, bootstraps = 10,
#'      rarefaction = c(3, 5))
#' bootstrapped_data # 5 series for 99 elements
#' get.dispRity(bootstrapped_data, what = "66.75552") # 1 series for 23 elements
#'
#' ## Disparity data sub-samples
#' disparity_data <- dispRity(bootstrapped_data, variances)
#' disparity_data # 5 series for 99 elements
#' get.dispRity(disparity_data, what = c(1,5)) # 2 series for 13 elements
#'
#' @seealso \code{\link{dispRity}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme

get.dispRity<-function(data, what, keep.elements=FALSE) {
    #----------------------
    # SANITIZING
    #----------------------
    #data
    check.class(data, "dispRity")

    #what
    if(missing(what)) {
        #do nothing
        return(data)
    } else {
        #witch class
        if(class(what) == "numeric" || class(what) == "integer") {
            #is any element not present?
            #first check dispRity type
            if(length(data) == 3) {
                if(any(is.na(match(what, 2:length(data$series)-1)))) stop("At least one requested series is not matching with data.")
            } else {
                if(any(is.na(match(what, 1:length(data$series))))) stop("At least one requested series is not matching with data.")
            } 
        } else {
            if(class(what) == "character") {
            #is any element not present?
            #first check dispRity type
                if(length(data) == 3) {
                    if(any(is.na(match(what, data$series[-1])))) stop("At least one requested series is not matching with data.")
                    what <- match(what, data$series[-1])
                } else {
                    if(any(is.na(match(what, data$series)))) stop("At least one requested series is not matching with data.")
                    what <- match(what, data$series)
                } 
            } else {
                stop("'what' must be a numeric or character argument.")
            }
        }
    }

    #keep.elements
    check.class(keep.elements, "logical")

    #----------------------
    # EXTRACTING THE ELEMENTS
    #----------------------

    #Case for serial dispRity objects
    if(length(data) == 3) {
        output <- list()
        #data
        output$data <- data$data[what]
        #elements
        if(keep.elements == TRUE) {
            output$elements <- data$elements
        } else {
            output$elements <- as.vector(unique(unlist(lapply(output$data, rownames))))
        }
        #series
        output$series <- data$series[c(1,(what+1))]
    }

    #Case for bootstraps dispRity objects
    if(length(data) == 4) {
        output <- list()
        #data
        output$data$observed <- data$data$observed[what]
        if(!is.null(data$data$bootstraps)) output$data$bootstraps <- data$data$bootstraps[what]
        #elements
        if(keep.elements == TRUE) {
            output$elements <- data$elements
        } else {
            output$elements <- as.vector(unique(unlist(lapply(output$data$observed, lapply, lapply, rownames))))
        }
        #series
        output$series <- data$series[what]
        #call
        output$call <- data$call
    }

    #Case for disparity dispRity objects
    if(length(data) == 5) {
        output <- list()
        #data
        output$data$observed <- data$data$observed[what]
        if(!is.null(data$data$bootstraps)) output$data$bootstraps <- data$data$bootstraps[what]
        #disparity
        output$disparity$observed <- data$disparity$observed[what]
        if(!is.null(data$disparity$bootstrapped)) output$disparity$bootstrapped <- data$disparity$bootstrapped[what]
        #elements
        if(keep.elements == TRUE) {
            output$elements <- data$elements
        } else {
            output$elements <- as.vector(unique(unlist(lapply(output$data$observed, lapply, lapply, rownames))))
        }
        #series
        output$series <- data$series[what]
        #call
        output$call <- data$call
    }

    class(output) <- "dispRity"
    return(output)
}

