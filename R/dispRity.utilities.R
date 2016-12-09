## Utilities functions for manipulating dispRity objects

#' @title Creates a \code{dispRity} object.
#' 
#' @usage make.dispRity(data, call, series)
#'
#' @description Creating an empty \code{dispRity} object from a matrix
#'
#' @param data A \code{matrix}.
#' @param call Optional, a \code{list} to be a \code{dispRity} call.
#' @param series Optional, a \code{list} to be a \code{dispRity} series list.
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
make.dispRity <- function(data, call, series) {
    ## Make the empty object
    dispRity_object <- list("matrix" = NULL , "call" = list(), "series" = list())

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

    ## Add the series
    if(!missing(series)) {
        check.class(series, "list")
        dispRity_object$series <- series
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

    ## Fill empty series
    if(length(data$series) == 0) {
        data$series <- c(data$series, list(list("elements" = as.matrix(1:nrow(data$matrix)))))
        #data$series[[1]][[1]] <- matrix(1:nrow(data$matrix))
    } else {
        for(series in 2:length(data$series)) {
            data$series[[series]] <- list("elements" = as.matrix(data$series[[series]]$elements))
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
#' @param series A \code{numeric} value to select a series (\code{0} is no series; default).
#' @param rarefaction A \code{numeric} value to select the rarefaction level (\code{0} is no rarefaction; default).
#' @param bootstrap A \code{numeric} value to select a specific bootstrap draw (\code{0} is no bootstrap; default).
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#' 
#' ## Calculating the disparity from a customised series
#' ## Generating the series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping and rarefying the data
#' dispRity_data <- boot.matrix(customised_series, bootstraps = 100,
#'      rarefaction = c(15, 10))
#' 
#' ## To get the original matrix
#' matrix.dispRity(dispRity_data)
#' 
#' ## To get the un-bootstrapped matrix from the second series
#' matrix.dispRity(dispRity_data, series = 2)
#' 
#' ## To get the 52nd bootstrap draw of the second rarefaction level (15) of the
#' ## same series
#' matrix.dispRity(dispRity_data, series = 2, rarefaction = 2, bootstrap = 52)
#' 
#' @author Thomas Guillerme
matrix.dispRity <- function(data, series, rarefaction, bootstrap){

    ## Sanitizing
    check.class(data, "dispRity")
    if(missing(series)) {
        return(data$matrix)
    } else {
        if(missing(rarefaction) || missing(bootstrap)) {
            return(data$matrix[data$series[[series]]$elements, 1:data$call$dimensions])
        } else {
            return(data$matrix[data$series[[series]][[rarefaction+1]][,bootstrap], 1:data$call$dimensions])
        }
    }
}

#' @title Extracts series from a dispRity object.
#' @aliases get.dispRity
#'
#' @description Extracting some series and data from a \code{dispRity} object.
#'
#' @param data A \code{dispRity} object.
#' @param series A list of series names or series numbers to be extracted.
#'
#' @return
#' This function outputs a \code{dispRity} object.
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat99) ; data(BeckLee_tree) 
#'
#' ## Series sub-samples
#' series_full <- time.series(BeckLee_mat99, BeckLee_tree,
#'      method = "continuous",time = 5, model = "acctran")
#' series_full # 5 series for 99 elements
#' 
#' get.series.dispRity(series_full, 1) # 1 series for 3 elements
#'
#' ## Bootstrapped data sub-samples
#' bootstrapped_data <- boot.matrix(series_full, bootstraps = 10,
#'      rarefaction = c(3, 5))
#' bootstrapped_data # 5 series for 99 elements
#' get.series.dispRity(bootstrapped_data, "66.75552") # 1 series for 23 elements
#'
#' ## Disparity data sub-samples
#' disparity_data <- dispRity(bootstrapped_data, variances)
#' disparity_data # 5 series for 99 elements
#' get.series.dispRity(disparity_data, c(1,5)) # 2 series for 13 elements
#'
#' @seealso \code{\link{dispRity}}, \code{\link{extract.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# series_full <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
# bootstrapped_data <- boot.matrix(series_full, bootstraps = 10, rarefaction = c(3, 5))
# disparity_data <- dispRity(bootstrapped_data, variances)
# get.series.dispRity(bootstrapped_data, series = "66.75552") # 1 series for 23 elements
# get.series.dispRity(series_full, series = 1) # 1 series for 3 elements
# get.series.dispRity(disparity_data, series = c(1,5)) # 2 series for 13 elements

get.series.dispRity <- function(data, series) {
    ## data
    check.class(data, "dispRity")

    ## series
    if(length(series) > length(data$series)) {
        stop("Not enough series in the original data.")
    } else {
        if(class(series) == "numeric" | class(series) == "integer") {
            if(any(is.na(match(series, 1:length(data$series))))) {
                stop("Series not found.")
            }
        } else {
            if(class(series) == "character") {
                series <- match(series, names(data$series))
                if(any(is.na(series))) {
                    stop("Series not found.")
                }
            } else {
                stop("Series argument must be of class \"numeric\" or \"character\".")
            }
        }
    }

    ## create the new data set
    data_out <- list("matrix" = data$matrix, "call" = data$call, "series" = data$series[series])

    ## Add the disparity (if available)
    if(!is.null(data$call$disparity)) {
        data_out$disparity <- data$disparity[series]
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
#' @param series Optional, a \code{numeric} or \code{character} for which series to get (if missing, the value for all series are given).
#'  
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat99) ; data(BeckLee_tree)
#'
#' ## Calculating some disparity
#' series <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",
#'      time = c(100, 80, 60), model = "gradual")
#' bootstraps_dat <- boot.matrix(series, bootstraps = 10, rarefaction = TRUE)
#' disparity_data <- dispRity(bootstraps_dat, metric = mean)
#'
#' ## Extracting the observed disparity
#' extract.dispRity(disparity_data)
#'
#' ## Extracting the bootstrapped disparity
#' boot_disp <- extract.dispRity(disparity_data, observed = FALSE)
#' str(boot_disp)
#' ## Or with the minimum rarefaction
#' boot_disp_rare <- extract.dispRity(disparity_data, observed = FALSE,
#'      rarefaction = "min")
#' str(boot_disp_rare)
#' ## Or a certain level of rarefaction
#' boot_disp_rare <- extract.dispRity(disparity_data, observed = FALSE,
#'      rarefaction = 5)
#' str(boot_disp_rare)
#' 
#' ## Extracting bootstrapped distributions of disparity
#' disparity_distr <- dispRity(bootstraps_dat, metric = variances)
#' boot_disp <- extract.dispRity(disparity_distr, observed = FALSE,
#'      concatenate = FALSE, keep.structure = TRUE)
#' str(boot_disp) # A list of three lists of 10 elements
#' 
#' @seealso \code{\link{dispRity}}, \code{\link{get.dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# source("sanitizing.R")
# data(BeckLee_mat99) ; data(BeckLee_tree) 
# series_full <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
# bootstrapped_data <- boot.matrix(series_full, bootstraps = 10, rarefaction = c(3, 5))
# data <- dispRity(bootstrapped_data, c(sum,variances))
# extract.dispRity(data, observed = FALSE, rarefaction = 5,series = 2)

extract.dispRity <- function(data, observed = TRUE, rarefaction, series) {
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

    ## Series
    if(missing(series)) {
        series <- seq(1:length(data$series))
    } else {
        if(length(series) > length(data$series)) {
            stop("Not enough series in the original data.")
        } else {
            if(class(series) == "numeric" | class(series) == "integer") {
                if(any(is.na(match(series, 1:length(data$series))))) {
                    stop("Series not found.")
                }
            } else {
                if(class(series) == "character") {
                    series <- match(series, names(data$series))
                    if(any(is.na(series))) {
                        stop("Series not found.")
                    }
                } else {
                    stop("Series argument must be of class \"numeric\" or \"character\".")
                }
            }
        }
    }

    ## Rarefaction
    if(!observed) {
        if(!missing(rarefaction)) {
            check.class(rarefaction, c("numeric", "integer"))
            check.length(rarefaction, 1, errorif = FALSE, msg = "Only one rarefaction level can be used.")
            if(any(is.na(match(rarefaction, data$call$bootstrap[[3]])))) {
                stop("Rarefaction level not found.")
            }
        } else {
            rarefaction <- FALSE
        }
    }

    ## Get the disparity values
    if(observed) {
        return(lapply(data$disparity[series], lapply.observed)) ## lapply observed lives in summary.dispRity_fun.R
    } else {
        output <- lapply(as.list(series), extract.disparity.values, data, rarefaction)
        names(output) <- names(data$series[series])
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
#' @param use.all \code{logical}, whether to scale/center using the full distribution (i.e. all the disparity values) or only the distribution within each series of bootstraps (default is \code{TRUE}).
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_mat50)
#'
#' ## Setting up two customised series
#' factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2),
#'      rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1,
#'      dimnames = list(rownames(BeckLee_mat50))))
#' customised_series <- cust.series(BeckLee_mat50, factors)
#' ## Bootstrapping the data
#' bootstrapped_data <- boot.matrix(customised_series, bootstraps = 7,
#'      rarefaction = c(10, 25))
#' ## Calculating the sum of centroids
#' disparity <- dispRity(bootstrapped_data, metric = c(sum, centroids))
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
# customised_series <- cust.series(BeckLee_mat50, factors)
# bootstrapped_data <- boot.matrix(customised_series, bootstraps = 7, rarefaction = c(10, 25))
# data <- dispRity(bootstrapped_data, metric = c(sum, centroids))

# summary(data) # No scaling
# summary(scale.dispRity(data, scale = TRUE)) # Dividing by the maximum
# summary(scale.dispRity(data, scale = 0.1)) # Multiplying by 10
# summary(scale.dispRity(data, center = TRUE, scale = TRUE)) # Scaling and centering

scale.dispRity <- function(data, center = FALSE, scale = FALSE, use.all = TRUE) {
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

