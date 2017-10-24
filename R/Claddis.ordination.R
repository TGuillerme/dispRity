#' @title Imports data from Claddis
#'
#' @description Takes Claddis data and computes both the distance and the ordination matrix 
#'
#' @param data Data from Claddis::ReadMorphNexus.
#' @param distance Distance type to be computed by \code{\link[Claddis]{MorphDistMatrix}}. Can be either \code{"Gower"}, \code{"GED"}, \code{"Max"}, \code{"Comp"}
#' @param transform Whether to transform the proportional distances (for Gower and Max). Options are \code{"none"}, \code{"sqrt"}, or \code{"arcsine_sqrt"} (the default).
#' @param k The number of dimensions in the ordination. If left empty, the number of dimensions is set to number of rows - 1.
#' @param ... Any optional arguments to be passed to \code{\link[stats]{cmdscale}}.
#' 
#' @examples
#' \dontrun{
#' require(Claddis)
#' 
#' ## Creating an ordination of the distance matrix of Claddis example data
#' Claddis.ordination(Claddis::Michaux1989)
#' }
#'
#' @seealso \code{\link[Claddis]{MorphDistMatrix}}, \code{\link[Claddis]{ReadMorphNexus}}, \code{\link[Claddis]{MakeMorphMatrix}}, \code{\link[stats]{cmdscale}}, \code{\link{custom.subsamples}}, \code{\link{time.subsamples}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' 
#' @author Thomas Guillerme
#' @export

Claddis.ordination <- function(data, distance = "Gower", transform = "arcsine_sqrt", k, ...) {
    match_call <- match.call()
    ## Sanitizing

    ## Data
    error_msg <- paste(match_call$data, "does not contain a matrix.\nUse Claddis::ReadMorphNexus to generate the proper data format.")
    check.class(data, "list", msg = error_msg)
    ## Must have at least one matrix
    if(!any(names(data) %in% "matrix")) {
        stop(error_msg, call. = FALSE)
    }
    ## Matrix must be a matrix
    check.class(data$matrix, "matrix", msg = error_msg)

    ## Distance
    distances_available <- c("Gower", "GED", "Max", "Comp")
    check.method(distance, distances_available, msg = "distance argument")

    ## Transform
    transforms_available <- c("none", "sqrt", "arcsine_sqrt")
    check.method(transform, transforms_available, msg = "transform argument")    

    ## k
    max_k <- (nrow(data$matrix) -1)
    if(missing(k)) {
        k <- max_k
    } else {
        check.class(k, "numeric")
        check.length(k, 1, " must be a single numeric value.")
        if(k > max_k) {
            stop(paste("k cannot be greater than the number of rows in data - 1 (", max_k, ")", sep = ""))
        }
    }

    ## Transforming the Claddis data

    ## Compute the distance
    distance <- MorphDistMatrix.support(data, distance = distance) #TG: Change to the proper version

    ## Ordinate the matrix
    ordination <- stats::cmdscale(distance, k = k, ...)

    return(ordination)
}