# ' @title Creating a generic \code{dispRity} object.
# '
# ' @description Creating\code{dispRity} object.
# '
# ' @param data A \code{matrix}.
# ' @param call A \code{list}.
# ' @param series A \code{list}.
# ' 
# ' @examples
# ' 
# ' @author Thomas Guillerme

make.dispRity <- function(data, call, series) {
    ## Make the empty object
    dispRity_object <- list("matrix" = matrix() , "call" = list(), "series" = list())
    ## Add the origin series
    dispRity_object$series$origin <- list("elements" = NULL)

    ## Add the matrix
    if(!missing(data)) {
        check.class(data, "matrix")
        dispRity_object$matrix <- data
    }

    ## Add the call
    if(!missing(call)) {
        check.class(call, "list")
        dispRity_object$call <- call
    }

    ## Add the series
    if(!missing(series)) {
        check.class(series, "list")
        dispRity_object$series <- series
    }

    class(dispRity_object) <- "dispRity"

    return(dispRity_object)
}

fill.dispRity <- function(data) {

    ## Elements
    if(length(data$series$origin$elements) == 0) {
        data$series$origin$elements <- 1:nrow(data$matrix)
    }

    ## Dimensions
    if(length(data$call$dimensions) == 0) {
        data$call$dimensions <- ncol(data$matrix)
    }

    ## Fill empty series
    if(length(data$series) == 1) {
        data$series <- c(data$series, list(list("elements" = 1:nrow(data$matrix))))
        data$series[[2]][[2]] <- matrix(1:nrow(data$matrix))
    } else {
        for(series in 2:length(data$series)) {
            data$series[[series]] <- list("elements" = data$series[[series]]$elements, matrix(data$series[[series]]$elements))
        }
    }

    return(data)
}