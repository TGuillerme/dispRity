#' @name fuzz.make 
#'
#' @title Create fuzzy data
#'
#' @description Transforms data into a "fuzzy matrix" format.
#'
#' @param data the input data (see details).
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
#' @examples
#'
#' @seealso 
#'
#' @author Thomas Guillerme
# @export


fuzz.make <- function(data, ...) {

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


    ## Convert into a fuzzy list
    fuzzy_list <- lapply(apply(do.call(cbind, lapply(data, as.list)), 1, list), unlist, recursive = FALSE)

    ## Convert invariants
    lapply(fuzzy_list)



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
}



.sampler <- function(fun, params = list()) {
    if(!is(fun, "function")) {
        ## Return the unique value
        return(fun)
    } else {
        return(do.call(fun, args = params))
    }
}
