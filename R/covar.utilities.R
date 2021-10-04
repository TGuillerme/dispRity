#' @name covar.utilities
#' @aliases axis.covar get.covar
#' @title Utilities for a dispRity object with covariance matrices
#'
#' @description Different utility functions to extract aspects of a \code{MCMCglmm} object.
#'
#' @usage get.covar(data, sample, n, dimensions)
#' @usage axis.covar(data, sample, n, dimensions, level = 0.95, axis = 1)
#'
#' @param data a \code{dispRity} object with a \code{covar} element.
#' @param sample optional, one or more specific posterior sample IDs (is ignored if n is used) or a function to summarise all axes.
#' @param n optional, a random number of covariance matrices to sample (if left empty, all are used).
#' @param dimensions optional, which dimensions to use. If missing the dimensions from \code{data} are used.
#' @param level which confidence interval level to use (default is \code{0.95}).
#' @param axis which major axis to calculate (default is \code{1}, the first one).
#' 
#' @examples
#' ## Load the Charadriiformes dataset
#' data(charadriiformes)
#' ## Making a dispRity object with covar data
#' covar_data <- MCMCglmm.subsets(data       = charadriiformes$data,
#'                                posteriors = charadriiformes$posteriors)
#' 
#' ## Get the two first covar matrices for each level
#' get.covar(covar_data, sample = c(1,2))
#' ## Get 2 random covar matrices in 2D for each level
#' get.covar(covar_data, n = 2, dimensions = c(1,2))
#' ## Get mean covar matrix for each level
#' get.covar(covar_data, sample = mean)
#' 
#' ## Get the 0.95 major axis for the 42th covar matrix
#' axis.covar(covar_data, sample = 42)
#' ## Get the 0.5 major axis for 2 random samples
#' axis.covar(covar_data, n = 1, level = 0.5)
#' ## Get the median 0.95 minor axis of the 2D ellipse
#' axis.covar(covar_data, sample = mean, dimensions = c(1,2), axis = 2)
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export
get.covar <- function(data, sample, n, dimensions) {

    ## Some sanitizing on data, sample, n
    check.class(data, "dispRity")
    if(!missing(sample)) {
        check.class(sample, c("integer", "numeric", "function", "standardGeneric"))
    }
    if(!missing(n)) {
        check.class(n, c("integer", "numeric"))
    }
    if(!missing(dimensions)) {
        check.class(dimensions, c("integer", "numeric"))
        if(any(wrong <- !(dimensions %in% data$call$dimensions))) {
            stop.call(paste0("Incorrect number of dimensions."), call = "")
        }
    } else {
        ## Using the available dimensionality
        dimensions <- NULL
    }
    
    ## Just return everything!
    if(missing(sample) && missing(n)) {
        return(sample.n(data$covar, n = data$call$bootstrap[[1]], dimensions = dimensions))
    }

    ## Just return the n random samples
    if(!missing(n)) {
        if(!missing(sample)) {
            warning("sample argument is ignored since n = ", n, " random samples are asked for.")
        }
        return(sample.n(data$covar, n = n, dimensions = dimensions))
    }

    ## Return specific samples
    if(!missing(sample)) {
        if(is(sample, "function") || is(sample, "standardGeneric")) {
            ## Summarise the results
            return(lapply(lapply(sample.n(data$covar, n = data$call$bootstrap[[1]], dimensions = dimensions), summarise.fun, fun = sample), list))
        } else {
            ## Return specific samples
            return(sample.n(data$covar, selected_n = sample, dimensions = dimensions))
        }
    }
}
axis.covar <- function(data, sample, n, dimensions, level = 0.95, axis = 1) {
    
    check.class(data, "dispRity")
    ## Checks happen internally (in get.covar)
    if(missing(dimensions)) {
        ## Using the available dimensionality
        dimensions <- data$call$dimensions
    }

    ## Handle sample
    if(!missing(sample) && (is(sample, "function") || is(sample, "standardGeneric"))) {
        return(lapply(
                ## Get the axes from the selected covars
                    lapply(
                    ## Get the covar matrices
                    get.covar(data, n), lapply, get.one.axis, axis = axis, level = level, dimensions = dimensions),
                ## Get the mean of all the axes
                function(one_group, fun) list(apply(simplify2array(one_group), 1:2, fun)), fun = sample))
    }

    ## Get the covar matrices
    selected_covars <- get.covar(data, sample, n)

    ## Select all the axis
    return(lapply(selected_covars, lapply, get.one.axis, axis = axis, level = level, dimensions = dimensions))
}

