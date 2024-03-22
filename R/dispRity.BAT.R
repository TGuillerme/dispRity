#' @title Converts \code{dispRity} to \code{BAT}.
#'
#' @description Converts a \code{dispRity} object into \code{BAT} package arguments.
#'
#' @param data A \code{matrix}, \code{data.frame} or \code{dispRity} object.
#' @param subsets Optional, some specific subsets to extract (see \code{\link{get.subsets}}).
#' @param matrix Optional, some specific matrices to extract (see \code{\link{get.matrix}}).
#' @param tree Optional, some specific trees to extract (see \code{\link{get.tree}}).
#'
#' @details
#' Converts the content of a \code{dispRity} object into a list of arguments that can be used by \code{BAT} functions.
#'
#' @return
#' \itemize{
#'      \item \code{comm}
#'      \item \code{}
#'      \item \code{}
#' }
#' 
#' @examples
#' ## Base example:
## Converts a dispRity object into BAT arguments
dispRity.BAT <- function(data, subsets, matrix, tree) {

}

## Transforms the trait matrix into a community one
make.BAT.comm <- function(matrix, data) {
    if(missing(data)) {
        return(matrix(1, nrow = 1, ncol = nrow(matrix)))
    } else {
        return(matrix(as.integer(rownames(data) %in% rownames(matrix)), nrow = 1, ncol = nrow(data)))
    }
}
