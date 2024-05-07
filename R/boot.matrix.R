#' @title Bootstraps and rarefies data.
#'
#' @description Bootstraps and rarefies either a matrix or a list of matrices.
#' 
#' @param data A \code{matrix} or a list of matrices (typically output from \link{chrono.subsets} or \link{custom.subsets} - see details).
#' @param bootstraps The number of bootstrap pseudoreplicates (\code{default = 100}).
#' @param rarefaction Either a \code{logical} value whether to fully rarefy the data, a set of \code{numeric} values used to rarefy the data or \code{"min"} to rarefy at the minimum level (see details).
#' @param dimensions Optional, a vector of \code{numeric} value(s) or the proportion of the dimensions to keep.
#' @param verbose A \code{logical} value indicating whether to be verbose or not.
#' @param boot.type The bootstrap algorithm to use (\code{default = "full"}; see details).
#' @param prob Optional, a \code{matrix} or a \code{vector} of probabilities for each element to be selected during the bootstrap procedure. The \code{matrix} or the \code{vector} must have a row names or names attribute that corresponds to the elements in \code{data}.
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
#' When the input is \code{"min"}, the smallest number of elements is used (or 3 if some subsets have less than 3 elements).
#' 
#' \code{boot.type}: the different bootstrap algorithms are:
#' \itemize{
#'   \item \code{"full"}: resamples all the rows of the matrix and replaces them with a new random sample of rows (with \code{replace = TRUE}, meaning all the elements can be duplicated in each bootstrap).
#'   \item \code{"single"}: resamples only one row of the matrix and replaces it with a new randomly sampled row (with \code{replace = FALSE}, meaning that only one element can be duplicated in each bootstrap).
#'   \item \code{"null"}: resamples all rows of the matrix across subsets. I.e. for each subset of \emph{n} elements, this algorithm resamples \emph{n} elements across \emph{ALL} subsets. If only one subset (or none) is used, this does the same as the \code{"full"} algorithm.
#' }
#' 
#' \code{prob}: This option allows to attribute specific probability to each element to be drawn.
#' A probability of 0 will never sample the element, a probability of 1 will always allow it to be sampled.
#' This can also be useful for weighting elements: an element with a weight of 10 will be sampled ten times more.
#' If the argument is a \code{matrix}, it must have rownames attributes corresponding to the element names.
#' If the argument is a \code{vector}, it must have names attributes corresponding to the element names.
#'
#' Multiple trees: If the given \code{data} is a \code{\link{chrono.subsets}} based on multiple trees, the sampling is proportional to the presence of each element in each tree: \eqn{\sum (1/n) / T} (with \emph{n} being the maximum number of elements among the trees and \emph{T} being the total numbers of trees).
#' For example, for a slice through two trees resulting in the selection of elements \code{A} and \code{B} in the first tree and \code{A}, \code{B} and \code{C} in the second tree, the \code{"full"} bootstrap algorithm will select three elements (with replacement) between \code{A}, \code{B} and \code{C} with a probability of respectively \eqn{p(A) = 1/3} (\eqn{p(A) = (1/3 + 1/3) / 2}), \eqn{p(B) = 1/3} and \eqn{p(C) = 1/6} (\eqn{p(C) = (0 + 1/3) / 2}).
#' 
# Multiple matrices: If the given \code{data} contains multiple matrices, the elements selected for each bootstrap pseudo-replicate are applied to all the matrices. For example if a bootstrap pseudo-replicate selects the elements 2, 3, and 5, they are selected for all matrices. If the given \code{data} is from \code{\link{chrono.subsets}} and contains the same number of matrices and trees (>1) the sampling is distributed for each
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

boot.matrix <- function(data, bootstraps = 100, rarefaction = FALSE, dimensions = NULL, verbose = FALSE, boot.type = "full", prob = NULL) {

    match_call <- match.call()
    ## ----------------------
    ## Cleaning and checking
    ## ----------------------
    is_multi <- FALSE

    ## DATA
    ## If class is dispRity, data is serial
    if(!is(data, "dispRity")) {
        ## Data must be a matrix
        data <- check.dispRity.data(data, returns = c("matrix", "multi"))
        is_multi <- any(is_multi, data$multi)
        data <- data$matrix

        ## Check whether it is a distance matrix
        if(check.dist.matrix(data[[1]], just.check = TRUE)) {
            warning("boot.matrix is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!", call. = FALSE)
        }

        ## Creating the dispRity object
        data <- make.dispRity(data = data)
    } else {
        ## Must not already been bootstrapped
        if(!is.null(data$call$bootstrap) && data$call$bootstrap[[2]] != "covar") {
            stop.call(msg.pre = "", match_call$data, msg = " was already bootstrapped.")
        }

        ## Must be correct format
        check.length(data, 4, " must be either a matrix or an output from the chrono.subsets or custom.subsets functions.")
        
        ## With the correct names
        data_names <- names(data)
        if(is.null(data_names)) {
            stop.call(match_call$data, " must be either a matrix or an output from the chrono.subsets or custom.subsets functions.")
        } else {
            if(!all(data_names %in% c("matrix", "tree", "call", "subsets"))) {
            # if(data_names[[1]] != "matrix" | data_names[[2]] != "tree" | data_names[[3]] != "call" | data_names[[4]] != "subsets") {
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

    check.class(verbose, "logical")

    ## If is multi lapply the stuff
    if((!is.null(data$call$dispRity.multi) && data$call$dispRity.multi) || is_multi) {
        ## Split the data
        split_data <- dispRity.multi.split(data)

        ## Change the verbose call
        boot.matrix.call <- boot.matrix
        if(verbose) {
            ## Find the verbose lines
            start_verbose <- which(as.character(body(boot.matrix.call)) == "if (verbose) message(\"Bootstrapping\", appendLF = FALSE)")
            end_verbose <- which(as.character(body(boot.matrix.call)) == "if (verbose) message(\"Done.\", appendLF = FALSE)")
            ## Comment out both lines
            body(boot.matrix.call)[[start_verbose]] <- body(boot.matrix.call)[[end_verbose]] <- substitute(empty_line <- NULL)
        }

        if(verbose) message("Bootstrapping", appendLF = FALSE)

        ## Apply the custom.subsets
        output <- dispRity.multi.apply(split_data, fun = boot.matrix.call, bootstraps = bootstraps, rarefaction = rarefaction, dimensions = dimensions, verbose = verbose, boot.type = boot.type, prob = prob)

        if(verbose) message("Done.", appendLF = FALSE)
        return(output)
    }

    ## Data must contain a first "bootstrap" (empty list)
    if(length(data$subsets) == 0) {
        data <- fill.dispRity(data)
        probabilistic_subsets <- FALSE
        has_multiple_trees <- FALSE
    } else {
        if(ifelse(all(unique(unlist(lapply(data$subsets, lapply, ncol))) > 1), TRUE, FALSE)) {
            ## Check if the subsets have multiple trees (all are integers)
            has_multiple_trees <- ifelse(class(unlist(data$subsets))[1] == "integer", TRUE, FALSE)
            probabilistic_subsets <- FALSE

            ## Check if it has multiple trees AND has probabilities
            if(!has_multiple_trees) {
                has_multiple_trees <- ifelse(all(unique(unlist(lapply(data$subsets, lapply, ncol))) == 3), FALSE, TRUE)
                probabilistic_subsets <- TRUE
            }
        } else {
            ## Data has no probabilities nor multiple trees
            has_multiple_trees <- FALSE
            probabilistic_subsets <- FALSE
        }
    }

    if(!is.null(prob)) {
        if(probabilistic_subsets || has_multiple_trees) {
            stop.call(match_call$data, paste0(" was generated using a gradual time-slicing or using multiple trees (", data$call$subsets[2], ").\nThe prob option is not yet implemented for this case."))
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
            if(!all(prob_names %in% rownames(data$matrix[[1]]))) {
                stop.call(msg.pre = "prob argument contains elements not present in ", call =match_call$data, msg = ".")
            } else {
                ## Check if they are any names missing
                missing_rows <- rownames(data$matrix[[1]]) %in% prob_names
                if(any(missing_rows)) {
                    extra_prob <- rep(1, length(which(!missing_rows)))
                    names(extra_prob) <- rownames(data$matrix[[1]])[!missing_rows]
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
            names(prob) <- match(names(prob), rownames(data$matrix[[1]]))

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
    if(!is(rarefaction, "logical")) {
        ## Is it numeric?
        rare_class <- check.class(rarefaction, c("numeric", "integer", "character"), " must be either numeric, logical or \"min\".")
        if(rare_class == "character") {
            if(rarefaction != "min") {stop("rarefaction argument must be either numeric, logical or \"min\".", call. = FALSE)}
            rare_out <- min(size.subsets(data))
            rarefaction <- rare_out <- ifelse(rare_out < 3, 3, rare_out)
        } else {
            rare_out <- rarefaction
        }
    } else {
        if(rarefaction) {
            rarefaction <- seq(from = nrow(data$matrix[[1]]), to = 3)
            rare_out <- "full"
        } else {
            rarefaction <- NULL
            rare_out <- NULL
        }
    }

    ## BOOT.TYPE
    check.class(boot.type, "character")
    boot.type <- tolower(boot.type)
    check.length(boot.type, 1, " must be a single character string")
    
    ## Must be one of these methods
    check.method(boot.type, c("full", "single", "null"), "boot.type")

    ## Change boot type to full if single and multiple trees
    if(boot.type == "single" && has_multiple_trees) {
        boot.type <- "full"
        warning(paste0("Multiple trees where used in ", as.expression(match_call$data), ". The 'boot.type' option is set to \"full\"."))
    }

    ## Set up the bootstrap type function
    switch(boot.type,
        "full" = {
            if(probabilistic_subsets) {
                boot.type.fun <- boot.full.proba
            } else {
                boot.type.fun <- boot.full
            }
        },
        "single" = {
            if(probabilistic_subsets) {
                boot.type.fun <- boot.single.proba
            } else {
                boot.type.fun <- boot.single
            }
        },
        "null" = {
            if(probabilistic_subsets) {
                boot.type.fun <- boot.null #TODO: needs to be boot.null.proba
                warning("Bootstrap with the null algorithm not implemented for probabilities. Please remind the maintainer to eventually do it!")
            } else {
                boot.type.fun <- boot.null
            }
        }
    )

    ##  ~~~
    ##  Add some extra method i.e. proportion of bootstrap shifts?
    ##  ~~~

    ## RM.LAST.AXIS
    ## If TRUE, set automatic threshold at 0.95
    if(!is.null(dimensions)) {
        ## Else must be a single numeric value (proportional)
        check.class(dimensions, c("numeric", "integer"), " must be a proportional threshold value.")
        if(length(dimensions == 1)) {
            if(dimensions < 0) {
                stop.call("", "Number of dimensions to remove cannot be less than 0.")
            }
            if(dimensions < 1) dimensions <- 1:round(dimensions * ncol(data$matrix[[1]]))
        } 
        if(any(dimensions > ncol(data$matrix[[1]]))) {
            stop.call("", "Number of dimensions to remove cannot be more than the number of columns in the matrix.")
        }
        data$call$dimensions <- dimensions
    } else {
        data$call$dimensions <- 1:ncol(data$matrix[[1]])
    }

    ## Return object if BS = 0
    if(bootstraps == 0) {
        return(data)
    }

    ## BOOTSRAPPING THE DATA
    if(verbose) message("Bootstrapping", appendLF = FALSE)
    if(length(data$call$subsets) == 5 && as.logical(data$call$subsets[["bind"]])) {
        ## Get the number of trees
        n_trees <- as.numeric(data$call$subsets[["trees"]])
        ## Run the bootstraps for each trees
        bootstraps_per_tree <- bootstraps/n_trees
        ## Warning if the number of bootstraps is incorrect
        if(bootstraps_per_tree != as.integer(bootstraps_per_tree)) {
            bootstraps_per_tree <- ceiling(bootstraps_per_tree)
            bootstraps <- bootstraps_per_tree * n_trees
            warning(paste0("Because the data contains multiple trees and matrices bound together, the number of bootstraps is changed to ", bootstraps, " to distribute them evenly for each tree (", bootstraps_per_tree, " bootstraps * ",  n_trees, " trees)."))
        }

        ## Bootstrapping the subsetted results
        bootstrap_results <- lapply( ## Opens 1
                                lapply( ## Opens 2
                                    lapply( ## Opens 3
                                        data$subsets,
                                        ## Fun 3: Split the data per tree
                                        do.split.subsets, n_trees = n_trees),
                                    ## Fun 2: Apply the bootstraps
                                    lapply, bootstrap.wrapper, bootstraps_per_tree, rarefaction, boot.type.fun, verbose),
                                ## Fun 1: Merge into one normal bootstrap table
                                merge.to.list
                            )
    } else {
        ## Bootstrap the data set 
        bootstrap_results <- lapply(data$subsets, bootstrap.wrapper, bootstraps, rarefaction, boot.type.fun, verbose, all.elements = 1:dim(data$matrix[[1]])[1])
    }
    if(verbose) message("Done.", appendLF = FALSE)

    ## Combining and storing the results back in the dispRity object
    data$subsets <- mapply(combine.bootstraps, bootstrap_results, data$subsets, SIMPLIFY = FALSE)

    ## Adding the call information about the bootstrap
    data$call$bootstrap <- c(bootstraps, boot.type, list(rare_out))

    return(data)
}