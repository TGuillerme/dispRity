#' @title Imports data from Claddis
#'
#' @description Takes Claddis data and computes both the distance and the ordination matrix 
#'
#' @param data Data from \code{\link[Claddis]{read_nexus_matrix}} or the path to a file to be read by \code{\link[ape]{read.nexus.data}} (see details).
#' @param distance Distance type to be computed by \code{\link[Claddis]{calculate_morphological_distances}}. Can be either \code{"gc"}, \code{"ged"}, \code{"red"}, \code{"mord"}. \code{distance} can also be set to \code{NULL} to convert a matrix in \code{\link[Claddis]{read_nexus_matrix}} list type (see details).
#' @param ... Any optional arguments to be passed to \code{\link[Claddis]{calculate_morphological_distances}}.
#' @param k The number of dimensions in the ordination. If left empty, the number of dimensions is set to number of rows - 1.
#' @param add whether to use the Cailliez correction for negative eigen values (\code{add = TRUE}; default - see \code{\link[stats]{cmdscale}}) or not (\code{add = FALSE}).
#' @param arg.cmdscale Any optional arguments to be passed to \code{\link[stats]{cmdscale}} (as a named list such as \code{list(x.ret = TRUE)}).
#' 
#' @details
#' If \code{data} is a file path, the function will use a modified version of \code{\link[ape]{read.nexus.data}} (that handles polymorphic and ambiguous characters). The file content will then be converted into a \code{\link[Claddis]{read_nexus_matrix}} type list treating all characters as unordered.
#' If the \code{distance} is set to \code{NULL}, \code{data} will be only converted into a \code{\link[Claddis]{read_nexus_matrix}} type list.
#' 
#' @examples
#' \dontrun{
#' require(Claddis)
#' 
#' ## Ordinating the distance matrix of Claddis example data
#' Claddis.ordination(Claddis::michaux_1989)
#' 
#' ## Creating simple discrete morphological matrix (with polymorphisms)
#' cat(
#' "#NEXUS
#' BEGIN DATA;
#' DIMENSIONS  NTAX=5 NCHAR=5;
#' FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
#' MATRIX
#'      t1  {01}1010
#'      t2  02120
#'      t3  1210(01)
#'      t4  01111
#'      t5  00101
#' ;
#' END;", file = "morpho_matrix.nex")
#' 
#' ## Ordinating the matrix (using a distance matrix)
#' Claddis.ordination("morpho_matrix.nex")
#' 
#' ## Only converting the nexus matrix into a Claddis format
#' Claddis_data <- Claddis.ordination("morpho_matrix.nex", distance = NULL)
#' 
#' file.remove("morpho_matrix.nex")
#' }
#'
#' @seealso \code{\link[Claddis]{calculate_morphological_distances}}, \code{\link[Claddis]{read_nexus_matrix}}, \code{\link[Claddis]{build_cladistic_matrix}}, \code{\link[stats]{cmdscale}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' 
#' @author Thomas Guillerme
# @export

# DEBUG
# source("sanitizing.R")

Claddis.ordination <- function(data, distance = "mord", ..., k, add = TRUE, arg.cmdscale) {
    match_call <- match.call()
    ## Sanitizing

    ## Data
    error_msg <- paste0(" does not contain a matrix.\nUse Claddis::read_nexus_matrix to generate the proper data format.")

    if(!is(data, "cladisticMatrix")) {
        ## Loading the matrix
        check.class(data, "character", msg = error_msg)
        ## Reading the data
        data <- read.nexus.data(data)
        ## Converting into a "Claddis" object
        data <- convert.to.Claddis(data)
        ## If distance is null, simply return the data
        if(is.null(distance)) {
            return(data)
        }
    }

    ## Must have at least one matrix
    # if(!any(names(data) %in% "Matrix")) {
    if(length(grep("matrix_1", names(data))) == 0) {
        stop(paste0("data", error_msg), call. = FALSE)
    }
    ## Matrix must be a matrix
    if(!is(data$matrix_1$matrix, "matrix")) {
        stop(paste0("data", error_msg), call. = FALSE)
    }

    ## Distance
    distances_available <- c("gc", "ged", "red", "mord")
    check.method(distance, distances_available, msg = "distance argument")

    ## Handling cmdscale arguments
    if(missing(arg.cmdscale)) {
        arg.cmdscale <- list()
    }

    ## k
    max_k <- (nrow(data$matrix_1$matrix) -1)
    if(missing(k)) {
        arg.cmdscale$k <- max_k
    } else {
        check.class(k, "numeric")
        check.length(k, 1, " must be a single numeric value.")
        if(k > max_k) {
            stop.call("", paste0("k cannot be greater than the number of rows in data - 1 (data has ", max_k, " rows)."))
        }
    }

    ## add
    check.class(add, "logical")
    arg.cmdscale$add <- add

    ## Compute the distance
    distance_mat <- Claddis::calculate_morphological_distances(data, distance_metric = distance, ...)

    ## Check for NAs
    if(any(is.na(distance_mat$distance_matrix))) {
        stop.call(match_call$data, msg.pre = paste0("The generate distance matrix using \"", distance, "\" distance from "), msg = " contains NA and cannot be ordinated.")
    }

    ## Adding the distance to arg.cmdscale
    arg.cmdscale$d <- distance_mat$distance_matrix

    ## Ordinate the matrix
    ordination <- do.call(stats::cmdscale, arg.cmdscale)

    if(!is(ordination, "matrix")) {
        ordination <- ordination$points
    }

    return(ordination)
}
