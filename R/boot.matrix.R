#' @title Bootstraps and rarefies data.
#'
#' @description Bootstraps and rarefies either a matrix or a list of matrices.
#' 
#' @param data A \code{matrix} or a list of matrices (typically output from \link{chrono.subsets} or \link{custom.subsets} - see details).
#' @param bootstraps The number of bootstrap pseudoreplicates (\code{default = 100}).
#' @param rarefaction Either a \code{logical} value whether to fully rarefy the data or a set of \code{numeric} values used to rarefy the data (see details).
#' @param dimensions Optional, a \code{numeric} value or proportion of the dimensions to keep.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param boot.type The bootstrap algorithm to use (\code{default = "full"}; see details).
#' @param prob Optional, a \code{matrix} or a \code{vector} of probabilities for each element to be selected during the bootstrap procedure. The \code{matrix} or the \code{vector} must have a rownames or names attribute that corresponds to the elements in \code{data}.
#' 
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{matrix}{the multidimensional space (a \code{matrix}).}
#' \item{call}{A \code{list} containing the called arguments.}
#' \item{subsets}{A \code{list} containing matrices pointing to the elements present in each subsets.}
#'
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#'
#' @details  
#' \code{data}: The data is considered as the multidimensional space and is not transformed (e.g. if ordinated with negative eigen values, no correction is applied to the matrix).
#' 
#' \code{rarefaction}: when the input is \code{numeric}, the number of elements is set to the value(s) for each bootstrap. If some subsets have fewer elements than the rarefaction value, the subsets is not rarefied.
#' 
#' \code{boot.type}: the different bootstrap algorithms are:
#' \itemize{
#'   \item \code{"full"}: resamples all the rows of the matrix and replaces them with a new random sample of rows (with \code{replace = TRUE}, meaning all the elements can be duplicated in each bootstrap).
#'   \item \code{"single"}: resamples only one row of the matrix and replaces it with a new randomly sampled row (with \code{replace = FALSE}, meaning that only one element can be duplicated in each bootstrap).
#' }
#' 
#' \code{prob}: This option allows to attribute specific probability to each element to be drawn.
#' A probability of 0 will never sample the element, a probability of 1 will sample.
#' This can also be useful for weighting elements: an element with a weight of 10 will be sampled ten times more.
#' If the argument is a \code{matrix}, it must have rownames attrbiutes corresponding to the element names.
#' If the argument is a \code{vector}, it must have names attributes corresponding to the element names.
#'
#' @seealso \code{\link{cust.subsets}}, \code{\link{chrono.subsets}}, \code{\link{dispRity}}.
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
#' ## Bootstrapping an the matrix but without sampling Cimolestes and sampling Maelestes 10x more
#' boot.matrix(BeckLee_mat50, bootstraps = 20, prob = c("Cimolestes" = 0, "Maelestes" = 10))
#' 
#' ## Bootstrapping a subsets of matrices
#' ## Generating a dummy subsets of matrices
#' ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9,
#'                            dimnames = list(letters[1:10]))
#' matrix_list <- custom.subsets(ordinated_matrix, list(A = 1:5, B = 6:10))
#' ## Bootstrapping the subsets of matrices 20 times (each)
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
# verbose = TRUE
# boot.type = "full"
# data <- BeckLee_mat50
# bootstraps = 11
# rarefaction = c(5,6)
# data <- cust.subsets(BeckLee_mat50, groups)
# bootstraps <- 3
# rarefaction <- TRUE

boot.matrix <- function(data, bootstraps = 100, rarefaction = FALSE, dimensions, verbose = FALSE, boot.type = "full", prob) {
    
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

        ## Check whether it is a distance matrix
        if(check.dist.matrix(data, just.check = TRUE)) {
            warning("boot.matrix is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!", call. = FALSE)
        }

        ## Creating the dispRity object
        dispRity_object <- make.dispRity(data = data)
        #dispRity_object$subsets$origin$elements <- seq(1:nrow(data))
        data <- dispRity_object

    } else {
        ## Must be correct format
        check.length(data, 3, " must be either a matrix or an output from the chrono.subsets or custom.subsets functions.")
        
        ## With the correct names
        data_names <- names(data)
        if(is.null(data_names)) {
            stop.call(match_call$data, " must be either a matrix or an output from the chrono.subsets or custom.subsets functions.")
        } else {
            if(data_names[[1]] != "matrix" | data_names[[2]] != "call" | data_names[[3]] != "subsets") {
                stop.call(match_call$data, "must be either a matrix or an output from the chrono.subsets or custom.subsets functions.")
            }
        }

        if(length(data$subsets) > 1) {
            ## Check if any subsets has at least three rows
            elements_check <- unlist(lapply(unlist(data$subsets, recursive = FALSE), function(X) length(X) < 3))
            if(any(elements_check)) {
                warning(paste("The following subsets have less than 3 elements: ", paste(unlist(strsplit(names(elements_check)[which(elements_check)], split = ".elements")), collapse = ", ") , ".\nThis might effect the bootstrap/rarefaction output." , sep = ""))
            }
        }
    }

    ## Data must contain a first "bootstrap" (empty list)
    if(length(data$subsets) == 0) {
        data <- fill.dispRity(data)
        probabilistic_subsets <- FALSE
    } else {
        ## Check if the subsets have probabilistic data
        probabilistic_subsets <- ifelse(all(unique(unlist(lapply(data$subsets, lapply, ncol))) > 1), TRUE, FALSE)
    }

    if(!missing(prob)) {
        if(probabilistic_subsets) {
            stop.call(match_call$data, paste0(" was generated using a gradual time-slicing (", data$call$subsets[2], ").\nThe prob option is not yet implemented for this case."))
        } else {
            probabilistic_subsets <- TRUE
            ## Check if prob is the right class
            class_prob <- check.class(prob, c("matrix", "numeric"))

            ## Check if it has attributes
            prob_names <- attributes(prob)
            if(is.null(prob_names)) {
                stop.call("", "prob argument must have names (vector) or dimnames (matrix) attributes.")
            } else {
                if(is.null(prob_names$names)) {
                    prob_names <- prob_names$dimnames[[1]]
                } else {
                    prob_names <- names(prob)
                }
            }

            ## Convert into a vector
            if(class_prob == "matrix") {
                prob <- as.vector(prob)
                if(length(prob) !=  length(prob_names)) {
                    stop.call("", "prob argument must be a matrix with one column only.")
                } else {
                    names(prob) <- prob_names 
                }
            }

            ## Check the names
            if(!all(prob_names %in% rownames(data$matrix))) {
                stop.call(msg.pre = "prob argument contains elements not present in ", call =match_call$data, msg = ".")
            } else {
                ## Check if they are any names missing
                missing_rows <- rownames(data$matrix) %in% prob_names
                if(any(missing_rows)) {
                    extra_prob <- rep(1, length(which(!missing_rows)))
                    names(extra_prob) <- rownames(data$matrix)[!missing_rows]
                    prob <- c(extra_prob, prob)
                }
            }

            ## Check the type of probability
            if(any(prob < 0)) {
                stop.call("", "Probabilities given to prob argument must be positive.")
            }

            ## Check if probabilities are weights
            if(any(prob > 1)) {
                prob <- prob/max(prob)
            }

            ## Renaming the elements to match the numbers in subsets
            names(prob) <- match(names(prob), rownames(data$matrix))

            ## Update the dispRity object
            add.prob <- function(one_subset, prob) {

                col1 <- one_subset$elements
                col2 <- rep(NA, nrow(one_subset$elements))
                col3 <- prob[match(one_subset$elements[,1], names(prob))]

                ## Remove any attributes
                attributes(col1) <- NULL
                attributes(col3) <- NULL
                one_subset$elements <- cbind(col1, col2, col3)
                return(one_subset)
            }

            data$subsets <- lapply(data$subsets, add.prob, prob)
        }
    }

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
    check.method(boot.type, c("full", "single"), "boot.type")

    # boot.type_class <- class(boot.type)
    # if(boot.type_class == "character") {
    #     boot.type <- tolower(boot.type)
    #     check.method(boot.type, c("full", "single"), "boot.type")
    #     check.length(boot.type, 1, " must be \"full\", \"single\" or a matrix.")
    # } else {
    #     check.class(boot.type, "matrix")
    #     if(!all(colnames(boot.type) == colnames(data$matrix))) {
    #         stop("The personalised boot.type matrix must have the same rownames as the data.")
    #     }
    # }

    ## Check whether the subsets (if any)

    ## Set up the bootstrap type function
    if(boot.type == "full") {
        if(probabilistic_subsets) {
            boot.type.fun <- boot.full.proba
        } else {
            boot.type.fun <- boot.full
        }
    }
    if(boot.type == "single") {
        if(probabilistic_subsets) {
            boot.type.fun <- boot.single.proba
        } else {
            boot.type.fun <- boot.single
        }
    }
    ##  ~~~
    ##  Add some extra method i.e. proportion of bootstrap shifts?
    ##  ~~~

    ## RM.LAST.AXIS
    ## If TRUE, set automatic threshold at 0.95
    if(!missing(dimensions)) {
        ## Else must be a single numeric value (proportional)
        check.class(dimensions, "numeric", " must be a proportional threshold value.")
        check.length(dimensions, 1, " must be a proportional threshold value.", errorif = FALSE)
        if(dimensions < 0) {
            stop.call("", "Number of dimensions to remove cannot be less than 0.")
        }
        if(dimensions < 1) dimensions <- round(dimensions * ncol(data$matrix))
        if(dimensions > ncol(data$matrix)) {
            stop.call("", "Number of dimensions to remove cannot be more than the number of columns in the matrix.")
        }
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
    bootstrap_results <- lapply(data$subsets, bootstrap.wrapper, bootstraps, rarefaction, boot.type.fun, verbose)
    if(verbose) message("Done.", appendLF = FALSE)

    ## Combining and storing the results back in the dispRity object
    data$subsets <- mapply(combine.bootstraps, bootstrap_results, data$subsets, SIMPLIFY = FALSE)

    ## Adding the call information about the bootstrap
    data$call$bootstrap <- c(bootstraps, boot.type, list(rare_out))

    return(data)
}