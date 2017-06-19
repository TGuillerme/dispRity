#' @title Bootstraps an rarefies ordinated data.
#'
#' @description Bootstraps and rarefies either a single ordinated matrix or a list of ordinated matrices.
#'
#' @usage boot.matrix(data, bootstraps = 1000, rarefaction = FALSE, dimensions, verbose = FALSE, boot.type = "full", parallel)
#' 
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)} or a list of matrices (typically output from \link{time.series} or \link{cust.series}).
#' @param bootstraps The number of bootstrap pseudo-replicates (\code{default = 1000}).
#' @param rarefaction Either a \code{logical} value whether to fully rarefy the data or a set of \code{numeric} values to rarefy the data (see details).
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param boot.type The bootstrap algorithm to use (\code{default = "full"}; see details).
#' @param parallel An optional vector containing the number of parallel threads and the virtual connection process type to run the function in parallel (requires \code{snow} package; see \code{\link[snow]{makeCluster}} function).
#' 
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the observed and bootstrapped matrices.}
#' \item{elements}{A \code{vector} containing all the names of the elements from the original matrix.}
#' \item{series}{A \code{vector} containing the name of the series (is \code{"1"} if the input was a single \code{matrix}).}
#' \item{call}{A \code{vector} containing the arguments used for the bootstrapping.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#'
#' @details  
#' \code{rarefaction}: when the input is \code{numeric}, the number of elements is set to the value(s) for each bootstrap. If some series have less elements than the rarefaction value, the series is not rarefied!
#' \code{boot.type}: the different bootstrap algorithms are:
#' \itemize{
#'   \item \code{"full"}: re-samples all the rows of the matrix and replaces them with a new random sample of rows (with \code{replace = TRUE}, meaning all the elements can be duplicated in each bootstrap).
#'   \item \code{"single"}: re-samples only one row of the matrix and replaces it with a new randomly sampled row (with \code{replace = FALSE}, meaning that only one elements can be duplicated in each bootstrap).
#' }
#'
#' @seealso \code{\link{cust.series}}, \code{\link{time.series}}, \code{\link{dispRity}}.
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
#' ## Bootstrapping an ordinated matrix with only 7,10 and 11 elements sampled
#' boot.matrix(BeckLee_mat50, bootstraps = 20, rarefaction = c(7, 10, 11))
#' ## Bootstrapping an ordinated matrix with only 3 dimensions
#' boot.matrix(BeckLee_mat50, bootstraps = 20, dimensions = 3)
#' 
#' ## Bootstrapping a series of matrices
#' ## Generating a dummy series of matrices
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9,
#'      dimnames = list(letters[1:10]))
#' factors <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10,
#'      ncol = 1, dimnames = list(letters[1:10])))
#' matrix.list <- cust.series(ordinated_matrix, factors)
#' ## Bootstrapping the series of matrices 20 times (each)
#' boot.matrix(matrix.list, bootstraps = 20)
#' 
#' \dontrun{
#' ## Bootstrapping a series of matrices using a single thread
#' system.time(boot.matrix(matrix.list, bootstraps = 10000, rarefaction = TRUE))
#' ## Bootstrapping a series of matrices using 4 threads
#' system.time(boot.matrix(matrix.list, bootstraps = 1000, rarefaction = TRUE,
#'      parallel = c(4, "SOCK")))
#' ## System time is three times shorter with parallel but elapsed is > 2 times
#' ## longer.
#' }
#' 
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG boot.matrix")
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
# data <- cust.series(BeckLee_mat50, factors)
# bootstraps <- 3
# rarefaction <- TRUE

boot.matrix <- function(data, bootstraps = 1000, rarefaction = FALSE, dimensions, verbose = FALSE, boot.type = "full", parallel) {
    
    match_call <- match.call()
    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## If class is dispRity, data is serial
    if(class(data) != "dispRity") {
        ## Data must be a matrix
        check.class(data, "matrix")

        ## Creating the dispRity object
        dispRity_object <- make.dispRity(data = data)
        #dispRity_object$series$origin$elements <- seq(1:nrow(data))
        data <- dispRity_object

    } else {
        ## Must be proper format
        check.length(data, 3, " must be either a matrix or an output from the time.series or cust.series functions.")
        
        ## With the proper names
        data_names <- names(data)
        if(data_names[[1]] != "matrix" | data_names[[2]] != "call" | data_names[[3]] != "series") {
            stop(paste(match_call$data, "must be either a matrix or an output from the time.series or cust.series functions."))
        }

        if(length(data$series) > 1) {
            ## Check if any series has at least three rows
            elements_check <- unlist(lapply(unlist(data$series, recursive = FALSE), function(X) length(X) < 3))
            if(any(elements_check)) {
                stop(paste("The following series have less than 3 elements: ", paste( unlist(strsplit(names(elements_check)[which(elements_check)], split = ".elements")), collapse = ", ") , "." , sep = ""))
            }
        }
    }

    ## Data must contain a first "bootstrap" (empty list)
    if(length(data$series) == 0) data <- fill.dispRity(data)

    ## BOOTSTRAP
    ## Must be a numeric value
    check.class(bootstraps, "numeric", " must be a single (entire) numerical value.")
    check.length(bootstraps, 1, " must be a single (entire) numerical value.")
    ## Make sure the bootstrap is a whole number
    bootstraps <- round(abs(bootstraps))

    ## RAREFACTION
    ## Is it not logical?
    if(class(rarefaction) != "logical") {
        ## Is it numeric?
        check.class(rarefaction, "numeric", " must be either numeric or logical.")
        rare_out <- rarefaction
    } else {
        if(rarefaction) {
            #rarefaction <- lapply(unlist(lapply(data$series, lapply, nrow), recursive = FALSE), seq, to = 3)
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
    ## Some humour:
    if(boot.type == "rangers") stop("Nice shoes!")
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

    ## Parallel
    if(missing(parallel)) {
        do_parallel <- FALSE
    } else {
        do_parallel <- TRUE
        check.length(parallel, 2, " must be a vector containing the number of threads and the virtual connection process type.")
        check.class(as.numeric(parallel[1]), "numeric", " must be a vector containing the number of threads and the virtual connection process type.")
        check.class(parallel[2], "character", " must be a vector containing the number of threads and the virtual connection process type.")
        ## Set up the cluster
        cluster <- makeCluster(as.numeric(parallel[1]), parallel[2])
    }

    ## Return object if BS = 0
    if(bootstraps == 0) {
        return(data)
    }

    ## BOOTSRAPING THE DATA
    if(verbose) message("Bootstrapping", appendLF = FALSE)
    ## Bootstrap the data set 
    if(!do_parallel) {
        bootstrap_results <- lapply(data$series, bootstrap.wrapper, bootstraps, rarefaction, boot.type.fun, verbose)
    } else {
        bootstrap_results <- parLapply(cluster, data$series, bootstrap.wrapper, bootstraps, rarefaction, boot.type.fun, verbose)
        stopCluster(cluster)
    }
    if(verbose) message("Done.", appendLF = FALSE)

    ## Combining and storing the results back in the dispRity object
    data$series <- mapply(combine.bootstraps, bootstrap_results, data$series, SIMPLIFY = FALSE)

    ## Adding the call information about the bootstrap
    data$call$bootstrap <- c(bootstraps, boot.type, list(rare_out))

    return(data)
}