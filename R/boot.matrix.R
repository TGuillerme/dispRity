#' @title Bootstraps and rarefies data.
#'
#' @description Bootstraps and rarefies either a matrix or a list of matrices.
#' 
#' @param data A matrix or a list of matrices (typically output from \link{time.subsamples} or \link{cust.subsamples}).
#' @param bootstraps The number of bootstrap pseudoreplicates (\code{default = 100}).
#' @param rarefaction Either a \code{logical} value whether to fully rarefy the data or a set of \code{numeric} values used to rarefy the data (see details).
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param boot.type The bootstrap algorithm to use (\code{default = "full"}; see details).
#' 
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{matrix}{the multidimensional space (a \code{matrix}).}
#' \item{call}{A \code{list} containing the called arguments.}
#' \item{subsamples}{A \code{list} containing matrices pointing to the elements present in each subsamples.}
#'
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#'
#' @details  
#' \code{rarefaction}: when the input is \code{numeric}, the number of elements is set to the value(s) for each bootstrap. If some subsamples have fewer elements than the rarefaction value, the subsamples is not rarefied.
#' \code{boot.type}: the different bootstrap algorithms are:
#' \itemize{
#'   \item \code{"full"}: resamples all the rows of the matrix and replaces them with a new random sample of rows (with \code{replace = TRUE}, meaning all the elements can be duplicated in each bootstrap).
#'   \item \code{"single"}: resamples only one row of the matrix and replaces it with a new randomly sampled row (with \code{replace = FALSE}, meaning that only one element can be duplicated in each bootstrap).
#' }
#'
#' @seealso \code{\link{cust.subsamples}}, \code{\link{time.subsamples}}, \code{\link{dispRity}}.
#'
#' @examples
#' ## Load the Beck & Lee 2014 matrix
#' data(BeckLee_mat50)
#' 
#' ## Bootstrapping a matrix
#' ## Bootstrapping an ordinated matrix 20 times
#' boot.matrix(BeckLee_mat50, bootstraps = 20)
#' ## Bootstrapping an ordinated matrix with rarefaction
#' boot.matrix(BeckLee_mat50, bootstraps = 20, rarefaction = TRUE)
#' ## Bootstrapping an ordinated matrix with only elements 7, 10 and 11 sampled
#' boot.matrix(BeckLee_mat50, bootstraps = 20, rarefaction = c(7, 10, 11))
#' ## Bootstrapping an ordinated matrix with only 3 dimensions
#' boot.matrix(BeckLee_mat50, bootstraps = 20, dimensions = 3)
#' 
#' ## Bootstrapping a subsamples of matrices
#' ## Generating a dummy subsamples of matrices
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9,
#'                            dimnames = list(letters[1:10]))
#' matrix_list <- custom.subsamples(ordinated_matrix, list(A = 1:5, B = 6:10))
#' ## Bootstrapping the subsamples of matrices 20 times (each)
#' boot.matrix(matrix_list, bootstraps = 20)
#' 
#' @author Thomas Guillerme

## DEBUG
# stop("DEBUG boot.matrix")
# source("sanitizing.R")
# source("boot.matrix_fun.R")
# data(BeckLee_mat50)
# bootstraps = 7
# rarefaction = FALSE
# dimensions = FALSE
# verbose = FALSE
# boot.type = "full"
# data <- BeckLee_mat50
# bootstraps = 11
# rarefaction = c(5,6)
# data <- cust.subsamples(BeckLee_mat50, groups)
# bootstraps <- 3
# rarefaction <- TRUE

boot.matrix <- function(data, bootstraps = 100, rarefaction = FALSE, dimensions, verbose = FALSE, boot.type = "full") {
    
    parallel <- FALSE

    match_call <- match.call()
    ## ----------------------
    ## Cleaning and checking
    ## ----------------------
    ## DATA
    ## If class is dispRity, data is serial
    if(class(data) != "dispRity") {
        ## Data must be a matrix
        check.class(data, "matrix")
        ## Creating the dispRity object
        dispRity_object <- make.dispRity(data = data)
        #dispRity_object$subsamples$origin$elements <- seq(1:nrow(data))
        data <- dispRity_object

    } else {
        ## Must be correct format
        check.length(data, 3, " must be either a matrix or an output from the time.subsamples or cust.subsamples functions.")
        
        ## With the correct names
        data_names <- names(data)
        if(data_names[[1]] != "matrix" | data_names[[2]] != "call" | data_names[[3]] != "subsamples") {
            stop(paste(match_call$data, "must be either a matrix or an output from the time.subsamples or cust.subsamples functions."))
        }

        if(length(data$subsamples) > 1) {
            ## Check if any subsamples has at least three rows
            elements_check <- unlist(lapply(unlist(data$subsamples, recursive = FALSE), function(X) length(X) < 3))
            if(any(elements_check)) {
                stop(paste("The following subsamples have less than 3 elements: ", paste(unlist(strsplit(names(elements_check)[which(elements_check)], split = ".elements")), collapse = ", ") , "." , sep = ""))
            }
        }
    }

    ## Data must contain a first "bootstrap" (empty list)
    if(length(data$subsamples) == 0) data <- fill.dispRity(data)

    ## BOOTSTRAP
    ## Must be a numeric value
    check.class(bootstraps, c("numeric", "integer"), " must be a single (integer) numerical value.")
    check.length(bootstraps, 1, " must be a single (integer) numerical value.")
    ## Make sure the bootstrap is a whole number
    bootstraps <- round(abs(bootstraps))

    ## RAREFACTION
    ## Is it logical?
    if(class(rarefaction) != "logical") {
        ## Is it numeric?
        check.class(rarefaction, c("numeric", "integer"), " must be either numeric or logical.")
        rare_out <- rarefaction
    } else {
        if(rarefaction) {
            #rarefaction <- lapply(unlist(lapply(data$subsamples, lapply, nrow), recursive = FALSE), seq, to = 3)
            rarefaction <- seq(from = nrow(data$matrix), to = 3)
            rare_out <- "full"
        } else {
            rarefaction <- NULL
            rare_out <- NULL
        }
    }

    ## VERBOSE
    check.class(verbose, "logical")

    ## BOOT.TYPE
    check.class(boot.type, "character")
    boot.type <- tolower(boot.type)
    check.length(boot.type, 1, " must be a single character string")
    
    ## Must be one of these methods
    check.method(boot.type, c("full", "single", "rangers"))
    ## Set up the bootstrap type function
    if(boot.type == "full") boot.type.fun <- boot.full
    if(boot.type == "single") boot.type.fun <- boot.single
    ##  ~~~
    ##  Add some extra method i.e. proportion of bootstrap shifts?
    ##  ~~~

    ## RM.LAST.AXIS
    ## If TRUE, set automatic threshold at 0.95
    if(!missing(dimensions)) {
        ## Else must be a single numeric value (proportional)
        check.class(dimensions, "numeric", " must be logical or a proportional threshold value.")
        check.length(dimensions, 1, " must be logical or a proportional threshold value.", errorif = FALSE)
        if(dimensions < 0) stop("Number of dimensions to remove cannot be less than 0.")
        if(dimensions < 1) dimensions <- round(dimensions * ncol(data$matrix))
        if(dimensions > ncol(data$matrix)) stop("Number of dimensions to remove cannot be more than the number of columns in the matrix.")
        data$call$dimensions <- dimensions
    } else {
        data$call$dimensions <- ncol(data$matrix)
    }

    ## Return object if BS = 0
    if(bootstraps == 0) {
        return(data)
    }

    ## BOOTSRAPING THE DATA
    if(verbose) message("Bootstrapping", appendLF = FALSE)
    ## Bootstrap the data set 
    bootstrap_results <- lapply(data$subsamples, bootstrap.wrapper, bootstraps, rarefaction, boot.type.fun, verbose)
    if(verbose) message("Done.", appendLF = FALSE)

    ## Combining and storing the results back in the dispRity object
    data$subsamples <- mapply(combine.bootstraps, bootstrap_results, data$subsamples, SIMPLIFY = FALSE)

    ## Adding the call information about the bootstrap
    data$call$bootstrap <- c(bootstraps, boot.type, list(rare_out))

    return(data)
}