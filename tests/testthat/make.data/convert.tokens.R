#' @title Convert a token to another one
#'
#' @description Convert a character containing a token into another one (e.g. missing)
#'
#' @param matrix A \code{matrix} or \code{list} with the characters for each taxa.
#' @param token A \code{character} string of the token to be replaced.
#' @param replace A \code{character} string of the token to be replace by.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

convert.tokens <- function(matrix, token, replace) {
    
    ## Check the matrix class
    #matrix_class <- check.class(matrix, c("list", "matrix"))
    matrix_class <- class(matrix)

    ## Replacing a character
    replace.token <- function(character, token, replace) {
        convert <- function(x, token, replace) return(ifelse(gregexpr(token, x)[[1]][1] != -1, replace, x))
        return(unname(sapply(character, convert, token, replace)))
    }

    ## For lists
    if(matrix_class == "list") {
        output <- lapply(matrix, replace.token, token, replace)
        ## Name elements
        if(!is.null(names(matrix))) {
            names(output) <- names(matrix)
        }
    } else {
    ## For matrices
        output <- apply(matrix, 2, replace.token, token, replace)
        ## Name elements
        if(!is.null(rownames(matrix))) {
            rownames(output) <- rownames(matrix)
        }
    }

    return(output)
}
