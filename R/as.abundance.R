# #' @title as.abundance
# #'
# #' @description Changes a dispRity metric to use the abundance element from a dispRity object.
# #'
# #' @param fun a \code{function} to apply to the \code{$abundance} element of \code{dispRity}.
# #' @param ... any additional arguments to pass to fun.
# #' 
# #' @details
# #' This function effectively transforms the input argument from \code{matrix} (or \code{matrix2}) to \code{matrix = matrix$abundance} and adds a evaluation after the return call to indicate that the function works on a \code{$abundance} element.
# #' Note that if the function does not have an argument called \code{matrix}, the first argument is estimated as being the one to be transformed (e.g. if the function has its first argument \code{x}, it will transform it to \code{x = x$abundance}).
# #' 
# #' @examples
# #' ## Creating a dispRity object with abundance data
# #' data(BeckLee_mat50) 
# #' abundance_data <- matrix(sample(c(0,1,2,3), 200, replace = TRUE,
# #' 							prob = c(0.4, 0.4, 0.1, 0.1)), nrow = 50, ncol = 4)
# #' disparabundance <- make.dispRity(data = BeckLee_mat50,
# #' 								    abundance = abundance_data)
# #' 
# #' ## Get the mean value of the traitspace ($matrix)
# #' get.disparity(dispRity(disparabundance, metric = mean))
# #' 
# #' ## Get the mean value of the abundance data ($abundance)
# #' get.disparity(dispRity(disparabundance, metric = as.abundance(mean)))
# #'
# #' @seealso \code{\link{dispRity}}
# #' 
# #' @author Thomas Guillerme
# #' @export

# as.abundance <- function(fun, ...) {
#     ## Finding the correct arguments to convert
#     avail_args <- names(formals(fun))

#     ## Switching cases between the first argument not being matrix (x, etc...) and the second argument being matrix2.

#     if(all(c("matrix", "matrix2") %in% avail_args)) {

#     fun.abundance <- function(abundance, ...) {
#     	matrix = abundance
#     	x = abundance

#         return(fun(x = x, ...))
#     }



#     return(fun.abundance)
# }
