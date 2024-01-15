#' @title as.covar
#'
#' @description Changes a dispRity metric to use the covar element from a dispRity object.
#'
#' @param fun a \code{function} to apply to the \code{$covar} element of \code{dispRity}.
#' @param ... any additional arguments to pass to fun.
#' @param VCV logical, whether to use the \code{$VCV} component of the elements in \code{dispRity$covar} (\code{TRUE}; default) or not (\code{FALSE}) (see details).
#' @param loc logical, whether to use the \code{$loc} component of the elements in \code{dispRity$covar} (\code{TRUE}) or not (\code{FALSE}; default) (see details).
#' 
#' @details
#' This function effectively transforms the input argument from \code{matrix} (or \code{matrix2}) to \code{matrix = matrix$VCV} and adds a evaluation after the return call to indicate that the function works on a \code{$covar} element.
#' Note that if the function does not have an argument called \code{matrix}, the first argument is estimated as being the one to be transformed (e.g. if the function has its first argument \code{x}, it will transform it to \code{x = x$VCV}).
#' 
#' You can toggle between using the \code{$VCV} or the \code{$loc} argument in the \code{$covar} matrix by using either \code{VCV = TRUE, loc = FALSE} (to access only \code{fun(matrix = matrix$VCV, ...)}), \code{VCV = FALSE, loc = TRUE} (to access only \code{matrix = matrix(matrix$loc, nrow = 1), ...}) or \code{VCV = TRUE, loc = TRUE} (to access \code{fun(matrix = matrix$VCV, loc = matrix$loc, ...)}; provided \code{fun} has an extra \code{loc} argument).
#' 
#' For \code{between.groups} metrics with \code{matrix} and \code{matrix2} arguments, you can provide multiple logicals for \code{VCV} and \code{loc} to be applied repspectively to \code{matrix} and \code{matrix2}. For example \code{VCV = TRUE} will reinterpret \code{matrix} and \code{matrix2} as \code{matrix$VCV} and \code{matrix2$VCV} but \code{loc = c(TRUE, FALSE)} will only reinterpret \code{matrix} as \code{matrix$loc} (and \code{matrix2} will not be reinterpreted).
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

        ## Check VCV and loc as double logicals
        if(length(VCV) == 1) {
            VCV <- rep(VCV, 2)
        }
        if(length(loc) == 1) {
            loc <- rep(loc, 2)
        }

        ## Function templates
        if(length(unique(VCV)) == 1) {  
            if(all(VCV)) {
                fun.covar2 <- function(matrix, matrix2, ...) {
                    ## This should never be evaluated by the function but only internally
                    fun_is_covar <-TRUE
                    return(fun(
                        matrix  = matrix$VCV,
                        matrix2 = matrix2$VCV,
                        loc     = matrix$loc,
                        loc2    = matrix2$loc,
                        ...))
                }
            }
            if(all(!VCV)) {
                fun.covar2 <- function(matrix, matrix2, ...) {
                    ## This should never be evaluated by the function but only internally
                    fun_is_covar <-TRUE
                    return(fun(
                        matrix  = matrix$loc,
                        matrix2 = matrix2$loc,
                        ...))
                }
            }
        } else {
            if(!VCV[1]) {
                fun.covar2 <- function(matrix, matrix2, ...) {
                    ## This should never be evaluated by the function but only internally
                    fun_is_covar <-TRUE
                    return(fun(
                        matrix2 = matrix2$VCV,
                        matrix  = matrix$loc,
                        loc2    = matrix2$loc,
                        ...))
                }
            }
            if(!VCV[2]) {
                fun.covar2 <- function(matrix, matrix2, ...) {
                    ## This should never be evaluated by the function but only internally
                    fun_is_covar <-TRUE
                    return(fun(
                        matrix  = matrix$VCV,
                        loc     = matrix$loc,
                        matrix2 = matrix2$loc,
                        ...))
                }
            }
        }

        ## Removing the extra arguments (loc or VCV)
        if(!VCV[1]) {
            body(fun.covar2)[3][[1]][[2]][which(as.character(body(fun.covar2)[3][[1]][[2]]) == "matrix$VCV")] <- NULL
        }
        if(!VCV[2]) {
            body(fun.covar2)[3][[1]][[2]][which(as.character(body(fun.covar2)[3][[1]][[2]]) == "matrix2$VCV")] <- NULL
        }
        if(!loc[1]) {
            body(fun.covar2)[3][[1]][[2]][which(as.character(body(fun.covar2)[3][[1]][[2]]) == "matrix$loc")] <- NULL
        }
        if(!loc[2]) {
            body(fun.covar2)[3][[1]][[2]][which(as.character(body(fun.covar2)[3][[1]][[2]]) == "matrix2$loc")] <- NULL
        }

        return(fun.covar2)

    } else {

        ## Toggle between the VCV loc options
        if(VCV && !loc) {
            fun.covar <- function(matrix, ...) {
                ## This should never be evaluated by the function but only internally
                fun_is_covar <-TRUE
                return(fun(matrix = matrix$VCV, ...))
            }
        }
        if(!VCV && loc) {
            fun.covar<- function(matrix, ...) {
                ## This should never be evaluated by the function but only internally
                fun_is_covar <-TRUE
                return(fun(matrix = matrix(matrix$loc, nrow = 1), ...))
            }
        }
        if(VCV && loc) {
            fun.covar <- function(matrix, ...) {
                ## This should never be evaluated by the function but only internally
                fun_is_covar <-TRUE
                return(fun(matrix = matrix$VCV, loc = matrix$loc, ...))
            }
        }

        if(!is.null(avail_args) && avail_args[1] != "matrix") {
            
            ## Change the first argument
            names(formals(fun.covar))[1] <- eval(substitute(noquote(avail_args[1])))
            
            ## Change the argument name in the body
            fun_body <- deparse(body(fun.covar))

            ## Toggle between the VCV/loc options
            if(VCV && !loc) {
                new_fun <- paste0(c(fun_body[1:2], paste0("    return(fun(", avail_args[1], " = ", avail_args[1], "$VCV, ...))"), fun_body[4]), collapse="\n")
            }
            if(!VCV && loc) {
                new_fun <- paste0(c(fun_body[1:2], paste0("    return(fun(", avail_args[1], " = matrix(", avail_args[1], "$loc, nrow = 1), ...))"), fun_body[4]), collapse="\n")
            }
            if(VCV && loc) {
                new_fun <- paste0(c(fun_body[1:2], paste0("    return(fun(", avail_args[1], " = ", avail_args[1], "$VCV, loc = ", avail_args[1], "$loc, ...))"), fun_body[4]), collapse="\n")
            }

            message(new_fun)

            body(fun.covar) <- as.expression(parse(text = new_fun))
        }

        return(fun.covar)
    }
}


# ## Testing in dispRity
# test <- as.covar(variances)
# fun_is_covar <-NULL
# cov_var <- as.covar(variances)

# try(eval(body(variances)[[length(body(variances))]]), silent = TRUE)
# is_covar # NULL

# try(eval(body(cov_var)[[length(body(cov_var))]]), silent = TRUE)
# is_covar # TRUE
