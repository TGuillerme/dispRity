#' @title Imports data from geomorph
#'
#' @description Takes geomorph landmark data and computes both a procrustes analysis and an ordination 
#'
#' @param data An array (p x k x n) containing landmark coordinates for a set of specimens.
#' @param k The number of dimensions in the ordination. If left empty, the number of dimensions is set to number of rows - 1.
#' @param ... Any optional arguments to be passed to \code{\link[geomorph]{gpagen}}.
#' 
# @examples
# require(geomorph)
# ## Loading the plethodon dataset
# data(plethodon)
# 
# ## Obtaining the ordination matrix
# ordination <- geomorph.ordination(plethodon) 
#
#' @seealso \code{\link[geomorph]{gpagen}}, \code{\link[stats]{cmdscale}}, \code{\link{custom.subsamples}}, \code{\link{time.subsamples}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' 

#source("sanitizing.R")
geomorph.ordination <- function(data, k, ...) {

    stop("IN DEVELOPMENT")

    # match_call <- match.call()

    # ## Sanitising
    # ## data
    # check.class(data, "list")
    # if(is.null(data$land)) {
    #     stop(paste(match_call$data, "must be a list with a 'land' element."))
    # } else {
    #     if(class(data$land) != "array") {
    #         stop(paste(match_call$data, "$land must be a (p x k x n) array.", sep = ""))
    #     }
    # }

    
    # ## Procrustes
    # Y.gpa <- geomorph::gpagen(data$land)

    # ## Ordination?

    # return(Y.gpa)
}