#' @title Imports data from Claddis
#'
#' @description Takes Claddis data and computes both the distance and the ordination matrix 
#'
#' @param data Data from Claddis::ReadMorphNexus.
#' @param distance Distance type to be computed by \code{\link[Claddis]{MorphDistMatrix}}. Can be either \code{"Gower"}, \code{"GED"}, \code{"Max"}, \code{"Comp"}
#' @param transform Whether to transform the proportional distances (for Gower and Max). Options are \code{"none"}, \code{"sqrt"}, or \code{"arcsine_sqrt"} (the default).
#' @param k The number of dimensions in the ordination. If left empty, the number of dimensions is set to number of rows - 1.
#' @param add whether to use the Cailliez correction for negative eigen values (\code{add = TRUE}; default - see \code{\link[stats]{cmdscale}}) or not (\code{add = FALSE}).
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
#' @seealso \code{\link[Claddis]{MorphDistMatrix}}, \code{\link[Claddis]{ReadMorphNexus}}, \code{\link[Claddis]{MakeMorphMatrix}}, \code{\link[stats]{cmdscale}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' 
#' @author Thomas Guillerme
#' @export

Claddis.ordination <- function(data, distance = "Gower", transform = "arcsine_sqrt", k, add = TRUE, ...) {
    match_call <- match.call()
    ## Sanitizing

    ## Data
    error_msg <- paste0("data does not contain a matrix.\nUse Claddis::ReadMorphNexus to generate the proper data format.")
    check.class(data, "list", msg = error_msg)
    ## Must have at least one matrix
    if(!any(names(data) %in% "matrix")) {
        stop.call("", error_msg)
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
            stop.call("", paste0("k cannot be greater than the number of rows in data - 1 (data has", max_k, "rows)."))
        }
    }

    ## Transforming the Claddis data

    ## Compute the distance
    distance <- MorphDistMatrix.support(data, distance = distance)

    ## Ordinate the matrix
    ordination <- stats::cmdscale(distance, k = k, add = add, ...)

    if(class(ordination) != "matrix") {
        ordination <- ordination$points
    }

    return(ordination)
}