#' @title as.covar
#'
#' @description Changes a dispRity metric to use the covar element from a dispRity object.
#'
#' @param fun a \code{function} to apply to the \code{$covar} element of \code{dispRity}.
#' @param ... any additional arguments to pass to fun.
#' @param VCV logical, whether to use the \code{$VCV} component of the elements in \code{dispRity$covar} (\code{TRUE}; default) or not (\code{FALSE}).
#' @param loc logical, whether to use the \code{$loc} component of the elements in \code{dispRity$covar} (\code{TRUE}) or not (\code{FALSE}; default).
#' 
#' @details
#' This function effectively transforms the input argument from \code{matrix} (or \code{matrix2}) to \code{matrix = matrix$VCV} and adds a evaluation after the return call to indicate that the function works on a \code{$covar} element.
#' Note that if the function does not have an argument called \code{matrix}, the first argument is estimated as being the one to be transformed (e.g. if the function has its first argument \code{x}, it will transform it to \code{x = x$VCV}).
#' 
#' You can toggle between using the \code{$VCV} or the \code{$loc} argument in the \code{$covar} matrix by using either \code{VCV = TRUE, loc = FALSE} (to access only \code{fun(matrix = matrix$VCV, ...)}), \code{VCV = FALSE, loc = TRUE} (to access only \code{matrix = matrix(matrix$loc, nrow = 1), ...}) or \code{VCV = TRUE, loc = TRUE} (to access \code{fun(matrix = matrix$VCV, loc = matrix$loc, ...)}; provided \code{fun} has an extra \code{loc} argument).

#' @examples
#' ## Creating a dispRity
#' data(charadriiformes)
#' 
#' ## Creating a dispRity object from the charadriiformes model
#' covar_data <- MCMCglmm.subsets(data       = charadriiformes$data,
#'                                posteriors = charadriiformes$posteriors)
#' 
#' ## Get one matrix and one covar matrix
#' one_matrix <- get.matrix(covar_data, subsets = 1)
#' one_covar  <- get.covar(covar_data, subsets = 1, n = 1)[[1]][[1]]
#' 
#' ## Measure the centroids
#' centroids(one_matrix)
#' 
#' ## Measure the centroids on the covar matrix
#' as.covar(centroids)(one_covar)
#' ## Is the same as:
#' centroids(one_covar$VCV)
#' 
#' ## Apply the measurement on a dispRity object:
#' ## On the traitspace:
#' summary(dispRity(covar_data, metric = c(sum, centroids))) 
#' ## On the covariance matrices:
#' summary(dispRity(covar_data, metric = c(sum, as.covar(centroids))))
#' ## The same but with additional options (centre = 100)
#' summary(dispRity(covar_data,
#'                  metric = c(sum, as.covar(centroids)),
#'                  centre = 100))
#' 
#' ## Example with the VCV and loc options
#' ## A metric that works with both VCV and loc
#' ## (the sum of the variances minus the distance from the location)
#' sum.var.dist <- function(matrix, loc = rep(0, ncol(matrix))) {
#'     ## Get the sum of the diagonal of the matrix
#'     sum_diag <- sum(diag(matrix))
#'     ## Get the distance between 0 and the loc
#'     dist_loc <- dist(matrix(c(rep(0, ncol(matrix)), loc), nrow = 2, byrow = TRUE))[1]
#'     ## Return the sum of the diagonal minus the distance
#'     return(sum_diag - dist_loc)
#' }
#' ## Changing the $loc on one_covar for the demonstration
#' one_covar$loc <- c(1, 2, 3)
#' ## Metric on the VCV part only
#' as.covar(sum.var.dist, VCV = TRUE, loc = FALSE)(one_covar)
#' ## Metric on the loc part only
#' as.covar(sum.var.dist, VCV = FALSE, loc = TRUE)(one_covar)
#' ## Metric on both parts
#' as.covar(sum.var.dist, VCV = TRUE, loc = TRUE)(one_covar)
#'
#' @seealso \code{\link{dispRity}} \code{\link{MCMCglmm.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

as.covar <- function(fun, ..., VCV = TRUE, loc = FALSE) {
    ## Finding the correct arguments to convert
    avail_args <- names(formals(fun))

    ## Switching cases
    if(all(c("matrix", "matrix2") %in% avail_args)) {
        ## Toggle between the VCV loc options
        if(VCV && !loc) {
            ## Between groups fun
            fun.covar2 <- function(matrix, matrix2, ...) {
                return(fun(matrix = matrix$VCV, matrix2 = matrix2$VCV, ...))
                ## This should never be evaluated by the function but only internally
                is_covar <- TRUE
            }
        }
        if(!VCV && loc) {
            ## Between groups fun
            fun.covar2 <- function(matrix, matrix2, ...) {
                return(fun(matrix = matrix(matrix$loc, nrow = 1), matrix2 = matrix(matrix2$loc, nrow = 1), ...))
                ## This should never be evaluated by the function but only internally
                is_covar <- TRUE
            }
        }
        if(VCV && loc) {
            ## Between groups fun
            fun.covar2 <- function(matrix, matrix2, ...) {
                return(fun(matrix = matrix$VCV, matrix2 = matrix2$VCV, loc = matrix$loc, loc2 = matrix2$loc, ...))
                ## This should never be evaluated by the function but only internally
                is_covar <- TRUE
            }
        }

        return(fun.covar2)

    } else {
        ## Function for only one matrix

        ## Toggle between the VCV loc options
        if(VCV && !loc) {
            fun.covar <- function(matrix, ...) {
                return(fun(matrix = matrix$VCV, ...))
                ## This should never be evaluated by the function but only internally
                is_covar <- TRUE
            }
        }
        if(!VCV && loc) {
            fun.covar<- function(matrix, ...) {
                return(fun(matrix = matrix(matrix$loc, nrow = 1), ...))
                ## This should never be evaluated by the function but only internally
                is_covar <- TRUE
            }
        }
        if(VCV && loc) {
            fun.covar <- function(matrix, ...) {
                return(fun(matrix = matrix$VCV, loc = matrix$loc, ...))
                ## This should never be evaluated by the function but only internally
                is_covar <- TRUE
            }
        }

        if(!is.null(avail_args) && avail_args[1] != "matrix") {
            
            ## Change the first argument
            names(formals(fun.covar))[1] <- eval(substitute(noquote(avail_args[1])))
            
            ## Change the argument name in the body
            fun_body <- deparse(body(fun.covar))

            ## Toggle between the VCV/loc options
            if(VCV && !loc) {
                new_fun <- paste0(c(fun_body[1], paste0("    return(fun(", avail_args[1], " = ", avail_args[1], "$VCV, ...))"), fun_body[3:4]), collapse="\n")
            }
            if(!VCV && loc) {
                new_fun <- paste0(c(fun_body[1], paste0("    return(fun(", avail_args[1], " = matrix(", avail_args[1], "$loc, nrow = 1), ...))"), fun_body[3:4]), collapse="\n")
            }
            if(VCV && loc) {
                new_fun <- paste0(c(fun_body[1], paste0("    return(fun(", avail_args[1], " = ", avail_args[1], "$VCV, loc = ", avail_args[1], "$loc, ...))"), fun_body[3:4]), collapse="\n")
            }

            body(fun.covar) <- as.expression(parse(text = new_fun))
        }

        return(fun.covar)
    }
}


# ## Testing in dispRity
# test <- as.covar(variances)
# is_covar <- NULL
# cov_var <- as.covar(variances)

# try(eval(body(variances)[[length(body(variances))]]), silent = TRUE)
# is_covar # NULL

# try(eval(body(cov_var)[[length(body(cov_var))]]), silent = TRUE)
# is_covar # TRUE
