#' @title Character differences
#'
#' @description Calculates the character difference from a discrete morphological matrix
#'
#' @param matrix A discrete morphological matrix or a list containing discrete characters.
#' 
#' @details
#' The character difference is calculated as @@@
#' 
#' @examples
#' ## Comparing two characters
#' char.diff(list(c(0, 1, 0, 1), c(0, 1, 1, 1)))
#' 
#' ## Pairwise comparisons in a morphological matrix
#' morpho_matrix <- matrix(sample(c(0,1), 100, replace = TRUE), 10)
#' char.diff(morpho_matrix)
#' 
#' @author Thomas Guillerme
#' 
#' @references
#' Gower, J.C. 1966. Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika 53:325–338.
#' 
#' Guillerme, T., Brazeau, M. D., 201@. @@@@

char.diff <- function (matrix)  {
    ## Sanitizing
    matrix_class <- check.class(matrix, c("matrix", "list"))
    ## Method is Gower by default
    method = "Gower"

    if(matrix_class == "matrix") {
        ## Convert matrix (if not numeric)
        if(!all(apply(matrix, 2, class) == "numeric")) {
            options(warn = -1)
            matrix <- apply(matrix, 2, as.numeric)
            options(warn = 0)
        }

        ## Options to remove:
        diag = FALSE
        upper = FALSE

        ## Getting the matrix parameters
        matrix <- t(matrix)
        N <- nrow(matrix)
        
        ## Setting the attributes
        attrs <- list(Size = N, Labels = dimnames(matrix)[[1L]], Diag = diag, Upper = upper, method = method, call = match.call(),  class = "dist")

        ## Running the C distance code
        options(warn = -1)
        output <- as.matrix(.Call("C_char_diff", matrix, method, attrs))
        options(warn = 0)

        ## Calculating the character difference
        return(round( 1 - ( abs(output-0.5)/0.5 ), digit = 10))
    } else {
        check.length(matrix, 2, " must contain only two elements", errorif = FALSE)
        ## Run the slower R version
        return(char.diff_R(matrix[[1]], matrix[[2]]))
    }
}
