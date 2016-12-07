#' @title Separating ordinated data in custom series.
#'
#' @description Splits the ordinated data into a customized series list.
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)}.
#' @param factor A \code{data.frame} with the same \eqn{k} elements as in \code{data} as rownames.
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the split ordinated data (each element is a \code{matrix}).}
#' \item{elements}{A \code{vector} containing all the rownames from the input matrix.}
#' \item{series}{A \code{vector} containing the name of the series.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' 
#' @details
#' The customized series can typically be a factor. For a finite number of taxonomic groups, traits, etc.
#'
#' @examples
#' ## Generating a dummy ordinated matrix
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9,
#'      dimnames = list(letters[1:10]))
#' ## Creating a list of dummy factors (1 or 2)
#' factors <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10,
#'      ncol = 1, dimnames = list(letters[1:10])))
#' ## Splitting the dummy ordinated matrix
#' cust.series(ordinated_matrix, factors)
#'
#' @seealso \code{\link{time.series}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG cust.series")
# source("sanitizing.R")
# source("cust.series_fun.R")
# source("time.series_fun.R")
# data <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
# factor <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))

cust.series <- function(data, factor) {
    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## data must be a matrix
    check.class(data, 'matrix')
    ## data must be of size k*<=k-1
    if(ncol(data) > (nrow(data) - 1)) stop("Input data must have at maximum (rows-1) columns")

    ## FACTOR
    ## must be matrix or data.frame
    if(class(factor) != "matrix") {
        if(class(factor) != "data.frame") {
            stop("factor must be either a 'matrix' or a 'data.frame'.")
        }
    }
    ## must have the same number of rows than data
    if(nrow(factor) != nrow(data)) stop('"factor" must have the same number of rows than "data".')
    ## must have the same labels as data
    if(!all(sort(as.character(rownames(factor))) == sort(as.character(rownames(data))))) stop("'data' and 'factor' arguments do not match.")
    ## must have at least 3 elements per levels
    check.elements <- function(factor) {
        any(table(as.factor(factor)) < 3)
    }
    if(any(apply(factor, 2, check.elements))) stop("There must be at least three elements per series.")

    ## ----------------------
    ##  SPLITING THE DATA INTO A LIST
    ## ----------------------

    ## Creating the series
    series_list <- unlist(apply(factor, 2, split.elements, data), recursive = FALSE)
    ## Adding the original series
    #series_list <- c(make.origin.series(data), series_list)

    ## Output as a dispRity object
    return(make.dispRity(data = data, call = list("series" = "customised"), series = series_list))
}