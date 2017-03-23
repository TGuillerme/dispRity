## Utilities functions for manipulating dispRity objects

#' @title Creates a \code{dispRity} object.
#' 
#' @usage make.dispRity(data, call, subsamples)
#'
#' @description Creating an empty \code{dispRity} object from a matrix
#'
#' @param data A \code{matrix}.
#' @param call Optional, a \code{list} to be a \code{dispRity} call.
#' @param subsamples Optional, a \code{list} to be a \code{dispRity} subsamples list.
#' 
#' @examples
#' ## An empty dispRity object
#' make.dispRity()
#' 
#' ## Still an empty dispRity object (with a matrix)
#' make.dispRity(data = matrix(rnorm(12), ncol = 3))
#'
#' 
#' @author Thomas Guillerme

make.dispRity <- function(data, call, subsamples) {
    ## Make the empty object
    dispRity_object <- list("matrix" = NULL , "call" = list(), "subsamples" = list())

    ## Add the matrix
    if(!missing(data)) {
        check.class(data, "matrix")
        dispRity_object$matrix <- data
    }

    ## Add the call
    if(!missing(call)) {
        check.class(call, "list")
        dispRity_object$call <- call
    }

    ## Add the subsamples
    if(!missing(subsamples)) {
        check.class(subsamples, "list")
        dispRity_object$subsamples <- subsamples
    }

    class(dispRity_object) <- "dispRity"

    return(dispRity_object)
}

#' @title Fills a \code{dispRity} object.
#'
#' @description Fills a \code{dispRity} object using the data from it's matrix
#'
#' @param data A \code{dispRity} object.
#' 
#' @examples
#' ## An empty dispRity object (with a matrix)
#' empty <- make.dispRity(data = matrix(rnorm(12), ncol = 3))
#' 
#' ## A dispRity object with a matrix of 4*3
#' fill.dispRity(empty)
#' 
#' @author Thomas Guillerme
#' 

fill.dispRity <- function(data) {

    ## Data have a matrix
    if(!is.null(data$matrix)) {
        check.class(data$matrix, "matrix")
        if(ncol(data$matrix) > nrow(data$matrix)) {
            stop("An ordinated matrix cannot have more columns than rows!")
        }
    } else {
        stop("dispRity object contains no matrix. Use:\nmake.dispRity(data = my_matrix)")
    }

    ## Dimensions
    if(length(data$call$dimensions) == 0) {
        data$call$dimensions <- ncol(data$matrix)
    }

    ## Fill empty subsamples
    if(length(data$subsamples) == 0) {
        data$subsamples <- c(data$subsamples, list(list("elements" = as.matrix(1:nrow(data$matrix)))))
        #data$subsamples[[1]][[1]] <- matrix(1:nrow(data$matrix))
    } else {
        for(subsamples in 2:length(data$subsamples)) {
            data$subsamples[[subsamples]] <- list("elements" = as.matrix(data$subsamples[[subsamples]]$elements))
        }
    }

    return(data)
}

#' @name matrix.dispRity
#' @title Fetching a matrix from a \code{dispRity} object.
#' @aliases fetch.matrix
#'
#' @description Fetching a specific matrix from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param subsamples A \code{numeric} value to select a subsamples (\code{0} is no subsamples; default).
#' @param rarefaction A \code{numeric} value to select the rarefaction level (\code{0} is no rarefaction; default).
#' @param bootstrap A \code{numeric} value to select a specific bootstrap draw (\code{0} is no bootstrap; default).
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## To get the original matrix
#' matrix.dispRity(disparity)
#' 
#' ## To get the un-bootstrapped matrix from the second subsamples
#' matrix.dispRity(disparity, subsamples = 2)
#' 
#' ## To get the 52nd bootstrap draw of the second rarefaction level (15) of the
#' ## same subsamples
#' matrix.dispRity(disparity, subsamples = 2, rarefaction = 2, bootstrap = 52)
#' 
#' @author Thomas Guillerme
matrix.dispRity <- function(data, subsamples, rarefaction, bootstrap){

    ## Sanitizing
    check.class(data, "dispRity")
    if(missing(subsamples)) {
        return(data$matrix)
    } else {
        if(missing(rarefaction) || missing(bootstrap)) {
            return(data$matrix[data$subsamples[[subsamples]]$elements, 1:data$call$dimensions])
        } else {
            return(data$matrix[data$subsamples[[subsamples]][[rarefaction+1]][,bootstrap], 1:data$call$dimensions])
        }
    }
}

#' @title Extracts subsamples from a dispRity object.
#' @aliases get.dispRity
#'
#' @description Extracting some subsamples and data from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param subsamples A list of subsamples names or subsamples numbers to be extracted.
#'
#' @return
#' This function outputs a \code{dispRity} object.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#'
#' ## Get one subsamples
#' get.subsamples.dispRity(disparity, "60")
#'
#' ## Get two subsamples
#' get.subsamples.dispRity(disparity, c(1,5))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsamples_full <- time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
# bootstrapped_data <- boot.matrix(subsamples_full, bootstraps = 10, rarefaction = c(3, 5))
# disparity_data <- dispRity(bootstrapped_data, variances)
# get.subsamples.dispRity(bootstrapped_data, subsamples = "66.75552") # 1 subsamples for 23 elements
# get.subsamples.dispRity(subsamples_full, subsamples = 1) # 1 subsamples for 3 elements
# get.subsamples.dispRity(disparity_data, subsamples = c(1,5)) # 2 subsamples for 13 elements

get.subsamples.dispRity <- function(data, subsamples) {
    ## data
    check.class(data, "dispRity")

    ## subsamples
    if(length(subsamples) > length(data$subsamples)) {
        stop("Not enough subsamples in the original data.")
    } else {
        if(class(subsamples) == "numeric" | class(subsamples) == "integer") {
            if(any(is.na(match(subsamples, 1:length(data$subsamples))))) {
                stop("subsamples not found.")
            }
        } else {
            if(class(subsamples) == "character") {
                subsamples <- match(subsamples, names(data$subsamples))
                if(any(is.na(subsamples))) {
                    stop("subsamples not found.")
                }
            } else {
                stop("subsamples argument must be of class \"numeric\" or \"character\".")
            }
        }
    }

    ## create the new data set
    data_out <- list("matrix" = data$matrix, "call" = data$call, "subsamples" = data$subsamples[subsamples])

    ## Add the disparity (if available)
    if(!is.null(data$call$disparity)) {
        data_out$disparity <- data$disparity[subsamples]
    }

    class(data_out) <- "dispRity"
    return(data_out)
}

#' @title Extracting disparity values.
#'
#' @description Extracts the disparity from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object containing disparity results.
#' @param observed A \code{logical} value indicating whether to output the observed (\code{TRUE} (default)) or the bootstrapped values (\code{FALSE}).
#' @param rarefaction Optional, a single \code{numeric} value corresponding to the rarefaction level (as the number of elements; if missing, the non-rarefied values are output).
#' @param subsamples Optional, a \code{numeric} or \code{character} for which subsamples to get (if missing, the value for all subsamples are given).
#' @param concatenate When the disparity metric is a distribution, whether to concatenate it (\code{TRUE}; default) or to return each individual ones.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#'
#' ## Extracting the observed disparity
#' extract.dispRity(disparity)
#'
#' ## Extracting the bootstrapped disparity
#' boot_disp <- extract.dispRity(disparity, observed = FALSE)
#' str(boot_disp)
#' ## Or only the rarefied (5) data
#' boot_disp_rare <- extract.dispRity(disparity, observed = FALSE,
#'      rarefaction = 5)
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{get.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsamples_full <- time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
# bootstrapped_data <- boot.matrix(subsamples_full, bootstraps = 10, rarefaction = c(3, 5))
# data <- dispRity(bootstrapped_data, c(sum,variances))
# extract.dispRity(data, observed = FALSE, rarefaction = 5,subsamples = 2)

extract.dispRity <- function(data, observed = TRUE, rarefaction = FALSE, subsamples, concatenate = TRUE) {
    #----------------------
    # SANITIZING
    #----------------------
    
    ## Data
    check.class(data, "dispRity")
    ## Data must have disparity values
    if(is.null(data$call$disparity)) {
        stop("dispRity object does not contain disparity values.")
    }

    ## Observed
    check.class(observed, "logical")

    ## subsamples
    if(missing(subsamples)) {
        subsamples <- seq(1:length(data$subsamples))
    } else {
        if(length(subsamples) > length(data$subsamples)) {
            stop("Not enough subsamples in the original data.")
        } else {
            if(class(subsamples) == "numeric" | class(subsamples) == "integer") {
                if(any(is.na(match(subsamples, 1:length(data$subsamples))))) {
                    stop("subsamples not found.")
                }
            } else {
                if(class(subsamples) == "character") {
                    subsamples <- match(subsamples, names(data$subsamples))
                    if(any(is.na(subsamples))) {
                        stop("subsamples not found.")
                    }
                } else {
                    stop("subsamples argument must be of class \"numeric\" or \"character\".")
                }
            }
        }
    }

    ## Rarefaction
    if(!observed) {
        if(rarefaction != FALSE) {
            check.class(rarefaction, c("numeric", "integer"))
            check.length(rarefaction, 1, errorif = FALSE, msg = "Only one rarefaction level can be used.")
            if(data$call$bootstrap[[3]][1] != "full" & any(is.na(match(rarefaction, data$call$bootstrap[[3]])))) {
                stop("Rarefaction level not found.")
            }
        } 
    }

    ## Get the disparity values
    if(observed) {
        ## Lapply wrapper for getting the disparity observed values
        lapply.observed <- function(disparity) {
            return(c(disparity$elements))
        }
        return(lapply(data$disparity[subsamples], lapply.observed))
    } else {
        output <- lapply(as.list(subsamples), extract.disparity.values, data, rarefaction, concatenate)
        names(output) <- names(data$subsamples[subsamples])
        return(output)
    }
}

#' @title Scaling and centering disparity results.
#'
#' @description Scales or/and centers the disparity measurements.
#'
#' @param data a \code{dispRity} object.
#' @param center either a \code{logical} value or a \code{numeric} vector of length equal to the number of elements of \code{data} (default is \code{FALSE}).
#' @param scale either a \code{logical} value or a \code{numeric} vector of length equal to the number of elements of \code{data} (default is \code{FALSE}).
#' @param use.all \code{logical}, whether to scale/center using the full distribution (i.e. all the disparity values) or only the distribution within each subsamples of bootstraps (default is \code{TRUE}).
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Scaling the data
#' summary(disparity) # No scaling
#' summary(scale(disparity)) # Dividing by the maximum
#' ## Multiplying by 10 (dividing by 0.1)
#' summary(scale.dispRity(disparity, max = 0.1))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{link[base]{scale}}.
#'
#' @author Thomas Guillerme
#' @export

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat50)
# factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
# customised_subsamples <- cust.subsamples(BeckLee_mat50, factors)
# bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 7, rarefaction = c(10, 25))
# data <- dispRity(bootstrapped_data, metric = c(sum, centroids))

# summary(data) # No scaling
# summary(scale.dispRity(data, scale = TRUE)) # Dividing by the maximum
# summary(scale.dispRity(data, scale = 0.1)) # Multiplying by 10
# summary(scale.dispRity(data, center = TRUE, scale = TRUE)) # Scaling and centering

scale.dispRity <- function(data, center = FALSE, scale = FALSE, use.all = TRUE, ...) {
    ## data
    check.class(data, "dispRity")
    if(is.null(data$call$disparity)) {
        stop("dispRity object does not contain disparity values.")
    }

    ## Get the whole distribution
    all_data <- unlist(extract.dispRity(data))
    if(!is.null(data$call$bootstrap)) {
        all_data <- c(all_data, unlist(extract.dispRity(data, observed = FALSE)))
    }

    ## Getting the center value
    if(class(center) == "logical") {
        if(center & use.all) {
            center <- mean(all_data)
        }
    } else {
        check.class(center, c("numeric", "integer", "logical"))
        check.length(center, 1, " must be either logical or a single numeric value.")
    }

    ## Getting the scale value
    if(class(scale) == "logical") {
        if(scale & use.all) {
            scale <- max(all_data)
        }
    } else {
        check.class(scale, c("numeric", "integer", "logical"))
        check.length(scale, 1, " must be either logical or a single numeric value.")
    }

    ## Lapply functions
    lapply.scale <- function(X, center, scale) {return(t(scale(t(X), center, scale)))}

    data$disparity <- lapply(data$disparity, lapply, lapply.scale, center, scale)

    return(data)
}


#' @title Sorting or ordering a \code{dispRity} object.
#'
#' @description Sort (or order) the subsamples of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param decreasing \code{logical}. Should the sort be increasing or decreasing? Is ignored if \code{sort} is used.
#' @param sort An optional \code{vector} of \code{numeric} values corresponding to the order in which to return the subsamples.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Sorting the data
#' summary(disparity)
#' summary(sort(disparity, decreasing = TRUE))
#' summary(sort(disparity, sort = c(7,1,3,4,5,2,6)))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{\link{plot.dispRity}}, \code{\link{get.dispRity}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme
#' @export

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsamples <- time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", time = 5, model = "acctran")
# data <- dispRity(subsamples, metric = mean)
# summary(data)
# summary(sort(data, decreasing = TRUE))
# summary(sort(data, sort = c(7,1,3,4,5,2,6)))

sort.dispRity <- function(data, decreasing = FALSE, sort, ...) {

    ## Sanitizing

    ## data
    check.class(data, "dispRity")
    ## Initialising subsamples length variable
    length_subsamples <- length(data$subsamples)
    if(length_subsamples == 1) stop("Data contains only one subsamples.")

    ## decreasing
    check.class(decreasing, "logical")

    ## sort
    if(!missing(sort)) {
        check.class(sort, "numeric")
        check.length(sort, length_subsamples, " must be the same length as the number of subsamples in data.")
        if(all.equal(sort(sort), seq(from = 1, to = length_subsamples)) != TRUE) {
            stop(paste("Sort argument can only contain unique numbers between 1 and ", length_subsamples, ".", sep = ""))
        }
    } else {
        if(decreasing == FALSE) sort <- seq(from = 1, to = length_subsamples)
        if(decreasing == TRUE) sort <- rev(seq(from = 1, to = length_subsamples))
    }


    ## Sorting the subsamples
    data$subsamples <- data$subsamples[sort]

    ## Sorting the disparity
    if(!is.null(data$call$disparity)) {
        data$disparity <- data$disparity[sort]
    }

    return(data)
}


