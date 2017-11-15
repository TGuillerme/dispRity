## Utility functions for manipulating dispRity objects

#' @title Creates a \code{dispRity} object.
#' 
#' @usage make.dispRity(data, call, subsets)
#'
#' @description Creating an empty \code{dispRity} object from a matrix
#'
#' @param data A \code{matrix}.
#' @param call Optional, a \code{list} to be a \code{dispRity} call.
#' @param subsets Optional, a \code{list} to be a \code{dispRity} subsets list.
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

make.dispRity <- function(data, call, subsets) {
    ## Make the empty object
    dispRity_object <- list("matrix" = NULL , "call" = list(), "subsets" = list())

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

    ## Add the subsets
    if(!missing(subsets)) {
        check.class(subsets, "list")
        dispRity_object$subsets <- subsets
    }

    class(dispRity_object) <- "dispRity"

    return(dispRity_object)
}

#' @title Fills a \code{dispRity} object.
#'
#' @description Fills a \code{dispRity} object using the data from its matrix
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
    } else {
        stop("dispRity object contains no matrix. Use:\nmake.dispRity(data = my_matrix)")
    }

    ## Dimensions
    if(length(data$call$dimensions) == 0) {
        data$call$dimensions <- ncol(data$matrix)
    }

    ## Fill empty subsets
    if(length(data$subsets) == 0) {
        data$subsets <- c(data$subsets, list(list("elements" = as.matrix(1:nrow(data$matrix)))))
        #data$subsets[[1]][[1]] <- matrix(1:nrow(data$matrix))
    } else {
        for(subsets in 2:length(data$subsets)) {
            data$subsets[[subsets]] <- list("elements" = as.matrix(data$subsets[[subsets]]$elements))
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
#' @param subsets A \code{numeric} value to select subsets (\code{0} is no subsets; default).
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
#' ## To get the un-bootstrapped matrix from the second subset
#' matrix.dispRity(disparity, subsets = 2)
#' 
#' ## To get the 52nd bootstrap draw of the second rarefaction level (15) of the
#' ## same subset
#' matrix.dispRity(disparity, subsets = 2, rarefaction = 2, bootstrap = 52)
#' 
#' @author Thomas Guillerme
matrix.dispRity <- function(data, subsets, rarefaction, bootstrap){

    ## Sanitizing
    check.class(data, "dispRity")
    if(missing(subsets)) {
        return(data$matrix)
    } else {
        if(missing(rarefaction) || missing(bootstrap)) {
            return(data$matrix[data$subsets[[subsets]]$elements, 1:data$call$dimensions])
        } else {
            return(data$matrix[data$subsets[[subsets]][[rarefaction+1]][,bootstrap], 1:data$call$dimensions])
        }
    }
}

#' @title Extracts subsets from a dispRity object.
#' @aliases get.dispRity, get.subsets.dispRity
#'
#' @description Extracting some subsets and data from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param subsets A list of subset names or subset numbers to be extracted.
#'
#' @return
#' This function outputs a \code{dispRity} object.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#'
#' ## Get one subset
#' get.subsets(disparity, "60")
#'
#' ## Get two subsets
#' get.subsets(disparity, c(1,5))
#'
#' @seealso \code{\link{dispRity}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsets_full <- time.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
# bootstrapped_data <- boot.matrix(subsets_full, bootstraps = 10, rarefaction = c(3, 5))
# disparity_data <- dispRity(bootstrapped_data, variances)
# get.subsets(bootstrapped_data, subsets = "66.75552") # 1 subsets for 23 elements
# get.subsets(subsets_full, subsets = 1) # 1 subsets for 3 elements
# get.subsets(disparity_data, subsets = c(1,5)) # 2 subsets for 13 elements

get.subsets <- function(data, subsets) {
    ## data
    check.class(data, "dispRity")

    ## subsets
    check.subsets(subsets, data)

    ## create the new data set
    data_out <- list("matrix" = data$matrix, "call" = data$call, "subsets" = data$subsets[subsets])

    ## Add the disparity (if available)
    if(!is.null(data$call$disparity)) {
        data_out$disparity <- data$disparity[subsets]
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
#' @param subsets Optional, a \code{numeric} or \code{character} for which subsets to get (if missing, the value for all subsets are given).
#' @param concatenate When the disparity metric is a distribution, whether to concatenate it (\code{TRUE}; default) or to return each individual metric.
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
#' @seealso \code{\link{dispRity}}, \code{\link{get.subsets}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsets_full <- time.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
# bootstrapped_data <- boot.matrix(subsets_full, bootstraps = 10, rarefaction = c(3, 5))
# data <- dispRity(bootstrapped_data, c(sum,variances))
# extract.dispRity(data, observed = FALSE, rarefaction = 5,subsets = 2)

extract.dispRity <- function(data, subsets, observed = TRUE, rarefaction = FALSE, concatenate = TRUE) {
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

    ## subsets
    if(missing(subsets)) {
        subsets <- seq(1:length(data$subsets))
    } else {
        check.subsets(subsets, data)
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
        return(lapply(data$disparity[subsets], lapply.observed))
    } else {
        output <- lapply(as.list(subsets), extract.disparity.values, data, rarefaction, concatenate)
        names(output) <- names(data$subsets[subsets])
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
#' @param use.all \code{logical}, whether to scale/center using the full distribution (i.e. all the disparity values) or only the distribution within each subsets of bootstraps (default is \code{TRUE}).
#' @param ... optional arguments to be passed to \code{scale}.
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
# groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
# customised_subsets <- cust.subsets(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 7, rarefaction = c(10, 25))
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
#' @description Sort (or order) the subsets of a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param decreasing \code{logical}. Should the sort be in ascending or descending order? Is ignored if \code{sort} is used.
#' @param sort An optional \code{vector} of \code{numeric} values corresponding to the order in which to return the subsets.
#' @param ... optional arguments to be passed to \code{sort}.
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
#' @seealso \code{\link{dispRity}}, \code{\link{test.dispRity}}, \code{\link{plot.dispRity}}, \code{\link{get.subsets}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme
#' @export

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# subsets <- time.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", time = 5, model = "acctran")
# data <- dispRity(subsets, metric = mean)
# summary(data)
# summary(sort(data, decreasing = TRUE))
# summary(sort(data, sort = c(7,1,3,4,5,2,6)))

sort.dispRity <- function(data, decreasing = FALSE, sort, ...) {

    ## Sanitizing

    ## data
    check.class(data, "dispRity")
    ## Initialising subsets length variable
    length_subsets <- length(data$subsets)
    if(length_subsets == 1) stop("Data contains only one subset.")

    ## decreasing
    check.class(decreasing, "logical")

    ## sort
    if(!missing(sort)) {
        check.class(sort, "numeric")
        check.length(sort, length_subsets, " must be the same length as the number of subsets in data.")
        if(all.equal(sort(sort), seq(from = 1, to = length_subsets)) != TRUE) {
            stop(paste("Sort argument can only contain unique numbers between 1 and ", length_subsets, ".", sep = ""))
        }
    } else {
        if(decreasing == FALSE) sort <- seq(from = 1, to = length_subsets)
        if(decreasing == TRUE) sort <- rev(seq(from = 1, to = length_subsets))
    }


    ## Sorting the subsets
    data$subsets <- data$subsets[sort]

    ## Sorting the disparity
    if(!is.null(data$call$disparity)) {
        data$disparity <- data$disparity[sort]
    }

    return(data)
}


#' @title Combines or cleans subsets.
#'
#' @description Combines multiple subsets together or cleans a subset series to contain at least n elements.
#'
#' @param data A \code{dispRity} object.
#' @param subsets Either a \code{vector} of the number or name of the subsets to merge or a single \code{numeric} value of the minimum of elements for each series (see details).
#' 
#' @details  
#' If \code{subset} is a vector, the subsets are merged in the given input order. \code{c(1, 3, 4)} will merge subsets 1 and 3 into 4, while the opposite, \code{c(3, 4, 1)} will merge subsets 3 and 4 into 1.
#' When a single numeric value is given, subsets are merged with the next subset until the correct number of elements for each subset is reached (apart from the last subset that gets merged with the previous one).
#' 
#' @return
#' A \code{dispRity} object containing the original matrix and subsets.
#' NOTE: if the data are already bootstrapped/rarefied or/and disparity already calculated the operation will have to be performed again.
#' 
#' @examples
#' ## Generate subsets from a dummy matrix
#' dummy_matrix <- matrix(rnorm(120), 40)
#' dummy_subsets <- custom.subsets(dummy_matrix,
#'      group = list("a" = c(1:5), "b" = c(6:10), "c" = c(11:20),
#'                   "d" = c(21:24), "e" = c(25:30), "f" = c(31:40)))
#' 
#' ## Merging the two first subsets
#' merge.subsets(dummy_subsets, c(1,2))
#' 
#' ## Merging the three subsets by name
#' merge.subsets(dummy_subsets, c("d", "c", "e"))
#' 
#' ## Merging the subsets to contain at least 20 taxa
#' merge.subsets(dummy_subsets, 10)
#' 
#' @seealso \code{\link{custom.subsets}}, \code{\link{time.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

#For testing
# stop("DEBUG merge.time.subsets")
# source("sanitizing.R")
# source("dispRity.utilities_fun.R")

# data(disparity)
# data <- disparity
# data1 <- time_binsEQ_Beck
# data2 <- time_binsEQ_Beck
# after = TRUE

merge.subsets <- function(data, subsets) {

    ## Internal cleaning function for only selecting the elements of the list in a subset
    select.elements <- function(subset) {
        subset[-1] <- NULL
        return(subset)
    }

    ## Saving the call
    match_call <- match.call()
    # match_call <- list(data = "data") ; warning("DEBUG merge.subsets")

    ## Sanitizing

    ## Data
    check.class(data, "dispRity")
    
    ## Check for previous data    
    has_disparity <- ifelse(!is.null(data$call$disparity), TRUE, FALSE)
    has_bootstrap <- ifelse(!is.null(data$call$bootstrap), TRUE, FALSE)
    if(has_disparity && has_bootstrap) {
        warning(paste(match_call$data, "contained bootstrap and disparity data that has been discarded in the output."))
        data$disparity <- NULL
        data$call$disparity <- NULL
        data$call$bootstrap <- NULL
        data$subsets <- lapply(data$subsets, select.elements)
    } else {
        if(has_disparity && !has_bootstrap) {
            warning(paste(match_call$data, "contained disparity data that has been discarded in the output."))
            data$disparity <- NULL
            data$call$disparity <- NULL
        } else {
            if(!has_disparity && has_bootstrap) {
                warning(paste(match_call$data, "contained bootstrap data that has been discarded in the output."))
                data$call$bootstrap <- NULL
                data$subsets <- lapply(data$subsets, "[[", 1)
            }
        }
    }

    ## subsets
    subsets_class <- check.class(subsets, c("character", "numeric", "integer"))
    if(length(subsets) == 1 && (subsets_class == "numeric" || subsets_class == "integer")) {
        ## Subsamples is the minimum per subsets
        clean_data <- TRUE
        if(subsets > nrow(data$matrix)) {
            stop(paste("Minimum sample size (", subsets, ") cannot be greater than the number of elements in the matrix (", nrow(data$matrix), ").", sep = ""))
        }

    } else {
        clean_data <- FALSE
        ## Must be at least two long
        if(length(subsets) < 2) stop("Subsamples argument must contain at least two values.")
        ## Must not contain duplicates
        if(length(subsets) != length(unique(subsets))) stop("Subsamples argument must not contain duplicates.")
        if(subsets_class == "character") {
            ## Must be present in the subsets names
            matches <- subsets %in% names(data$subsets)
            if(any(matches == FALSE)) {
                stop(paste(paste(subsets[!matches], collapse = " and "), "don't match with any of the subset names in", match_call$data))
            } else {
                subsets <- match(subsets, names(data$subsets))
            }
        } else {
            if(any(subsets > length(data$subsets))) {
                stop(paste("subsets", paste(subsets[which(subsets > length(data$subsets))], collapse = " and "), "don't match with any of the subsets in", match_call$data))
            }
        }
    }

    if(length(data$subsets) < length(subsets)) {
        stop(paste(match_call$data, "does not contain enough subsets!"))
    }

    if(clean_data) {
        ## Cleaning the data
        for(subs in 1:(length(data$subsets) - 1)) {
            ## Loop oversize buffer
            if(subs > (length(data$subsets) - 1)) {break}
            ## Merging subsets
            while(nrow(data$subsets[[subs]]$elements) < subsets) {
                data <- merge.two.subsets(subs1 = subs, subs2 = (subs + 1), data = data)
            }
        }
        ## Final element
        if(nrow(data$subsets[[length(data$subsets)]]$elements) < subsets) {
            data <- merge.two.subsets(subs1 = length(data$subsets), subs2 = (length(data$subsets) - 1), data = data)
        }
        return(data)
    } else {
        ## Merging two subsets
        names <- names(data$subsets)[subsets]
        name_replace <- names(data$subsets)[subsets[length(subsets)]]
        for(subs in 1:(length(subsets)-1)) {
            ## Loop over size buffer
            if(subs > (length(data$subsets))) {break}
            ## Merging subsets
            replace_2 <- match(name_replace, names(data$subsets))
            replace_1 <- match(names[subs], names(data$subsets))
            name_replace <- paste(names[subs], name_replace, sep = "-") 
            data <- merge.two.subsets(replace_1, replace_2, data)
        }
        return(data)
    }
}



#' @title Size subsets.
#'
#' @description Getting the size (number of elements) from each subsets of a dispRity object.
#'
#' @param data A \code{dispRity} object.
#' 
#' @examples
#' ## Loading a dispRity object
#' data(disparity)
#' 
#' ## What are the number of elements per subsets?
#' size.subsets(disparity)
#' 
#' @seealso \code{\link{custom.subsets}}, \code{\link{time.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

#For testing
# source("sanitizing.R")
# source("dispRity.utilities_fun.R")

# data(disparity)
# data <- disparity

size.subsets <- function(data) {
    ## Getting the size of subsets
    return(unlist(lapply(data$subsets, function(x) nrow(x$elements))))
}