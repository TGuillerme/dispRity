#' @name fuzz.make 
#'
#' @title Create fuzzy data
#'
#' @description Transforms data into a "fuzzy matrix" format.
#'
#' @param data the input data (see details).
#' @param distribution the distribution function for each trait (by default the uniform distribution is applied to all traits with \code{runif}).
#' @param parameters a named list of parameters for the distribution function with functions of how to calculate it (by default, the parameters are \code{list(max = max, min = min)} for the uniform distribution).
#' @param ...
#'
#' @details
#' The different \code{data} input formats can be:
#' \itemize{
#'      \item A \code{"matrix"}, \code{"array"} or \code{"list"} of matrices with the observations as rows and traits as columns.
# TODO: the output from \code{multi.ace}
# TODO: the output from \code{ace}
# TODO: the output from \code{treats::map.traits}
# TODO: a \code{treats} object
#' }
#'
#' The parameters should be provided as a named list of arguments that can be passed to the distribution function(s). The names of the list should correspond the argument names and the elements in the list can be values or functions to be passed to the fuzzy.matrix. For example, if the distribution function is \code{runif}, the parameters can be \code{max} or \code{min} so the parameters argument should be a list with these two names. You can then provide a function or a value to determine what these parameters should be. For example \code{parameters = list(max = 1, min = min)} would make set the maximum value to always 1 and the minimum value to the minimum value of the observed trait values. 
#'
#' @examples
#'
#' @seealso 
#'
#' @author Thomas Guillerme
# @export


fuzz.make <- function(data, distribution = runif, parameters = list(max = max, min = min), ...) {

    ## TODO: Sanitizing: check the data input class
    
    ## Convert into a list of matrices
    if(is(data, "matrix")) {
        data <- list(data)
    }
    ## TODO: apply the same for arrays

    ## TODO: Sanitizing: check if has the same dimensions and names

    ## Get the data input characteristics
    dims <- dim(data[[1]])
    n_traits <- dims[2]
    n_obs <- dims[1]
    trait_names <- NULL
    if(!is.null(colnames(data[[1]]))) {
        trait_names <- colnames(data[[1]])         
    }
    if(is.null(rownames(data[[1]]))) {
        obs_names <- paste0("obs_", 1:n_obs)
    } else {
        obs_names <- rownames(data[[1]])
    }


    ## Handling the traits and distributions


    ## Split the distributions per characters
    if(length(distribution) == 1) {
        traits_fun <- list(distribution)
    }

    ## parameters should be a list with parameters objects
    if(!is.null(names(parameters))) {
        traits_param <- list(parameters)
    }





    ## Convert into a fuzzy list
    fuzzy_list <- lapply(apply(do.call(cbind, lapply(data, as.list)), 1, list), unlist, recursive = FALSE)


    ## Generate the function for all traits
    fuzzy_list <- lapply(fuzzy_list, generate.function, distribution = NULL, parameters = traits_param[[1]])





    ## Applying something like this for fuzz.sample
    # do.call(fuzzy_list[[1]]$fun, args = c(n =1, fuzzy_list[[1]]$param))




    ## Apply default distributions

    ## Apply default params
    # do something like param = list(max = max, min = min) for runif. Or list(sd = sd, mean = mean) for rnorm, etc. list(arg_name = function)


    ## Split that list into convertible elements

    ## Create the structure skeleton
    fuzzy.matrix <- list(traits = replicate(n_traits, list()), fun = NULL, param = NULL)

    ## Add colnames
    if(!is.null(trait_names)) {
        names(fuzzy.matrix$traits) <- trait_names
    }

    ## 


    fuzzy_matrix <- matrix(fuzzy_list, nrow = n_obs, ncol = n_traits)

    return(NULL)


    #784 bytes <- just the matrices list
    #3728 bytes <- the matrices list
    #16528 bytes <- the matrix list with functions

data(bird.orders)
x <- rnorm(23)
### Compare the three methods for continuous characters:
ace(x, bird.orders)



mean(c(-0.7250813, 0.16506867))

}


## Generate a cell value for the trait
generate.function <- function(cell, distribution = NULL, parameters) {
    cell_vals <- unlist(cell)
    ## Convert to invariant character
    if(length(invariant <- unique(cell_vals)) == 1) {
        return(list(fun = invariant))
    } else {
        ## Get the function parameters
        fun_param <- lapply(parameters, function(x, values) do.call(x, args = list(x = values)), values = cell_vals)
        return(list(fun = distribution, param = fun_param))
        #TODO: Need to replace fun by NULL if 
    }
}
