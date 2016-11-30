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