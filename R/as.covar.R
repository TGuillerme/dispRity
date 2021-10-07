#' @title as.covar
#'
#' @description Changes a dispRity metric to use the covar element from a dispRity object.
#'
#' @param fun a \code{function} to apply to the \code{$covar} element of \code{dispRity}.
#' @param ... any additional arguments to pass to fun.
#' 
#' @details
#' This function effectively transforms the input argument from \code{matrix} (or \code{matrix2}) to \code{matrix = matrix$VCV} and adds a evaluation after the return call to indicate that the function works on a \code{$covar} element.
#' Note that if the function does not have an argument called \code{matrix}, the first argument is estimated as being the one to be transformed (e.g. if the function has its first argument \code{x}, it will transform it to \code{x = x$VCV}).
#' 
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
#' ## Measure the variances
#' variances(one_matrix)
#' 
#' ## Measure the variances on the covar matrix
#' as.covar(variances)(one_covar)
#' ## Is the same as:
#' variances(one_covar$VCV)
#' 
#' ## Apply the measurement on a dispRity object:
#' ## On the traitspace:
#' summary(dispRity(covar_data, metric = c(sum, variances))) 
#' ## On the covariance matrices:
#' summary(dispRity(covar_data, metric = c(sum, as.covar(variances))))
#' 
#'
#' 
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

as.covar <- function(fun, ...) {
    ## Finding the correct arguments to convert
    avail_args <- names(formals(fun))

    ## Switching cases
    if(all(c("matrix", "matrix2") %in% avail_args)) {
        ## Between groups fun
        fun_covar <- function(matrix, matrix2, ...) {
            return(fun(matrix = matrix$VCV, matrix2 = matrix2$VCV, ...))
            ## This should never be evaluated by the function but only internally
            is_covar <- TRUE
        }
    } else {
        ## Function for only one matrix
        fun_covar <- function(matrix, ...) {
            return(fun(matrix = matrix$VCV, ...))
            ## This should never be evaluated by the function but only internally
            is_covar <- TRUE
        }

        if(!is.null(avail_args) && avail_args[1] != "matrix") {
            
            ## Change the first argument
            names(formals(fun_covar))[1] <- eval(substitute(noquote(avail_args[1])))
            
            ## Change the argument name in the body
            fun_body <- deparse(body(fun_covar))
            new_fun <- paste0(c(fun_body[1], paste0("    return(fun(", avail_args[1], " = ", avail_args[1], "$VCV, ...))"), fun_body[3:4]), collapse="\n")
            body(fun_covar) <- as.expression(parse(text = new_fun))
        }
    }
    return(fun_covar)
}


# ## Testing in dispRity
# test <- as.covar(variances)
# is_covar <- NULL
# cov_var <- as.covar(variances)

# try(eval(body(variances)[[length(body(variances))]]), silent = TRUE)
# is_covar # NULL

# try(eval(body(cov_var)[[length(body(cov_var))]]), silent = TRUE)
# is_covar # TRUE
