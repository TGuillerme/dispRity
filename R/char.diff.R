#' @title Character differences
#'
#' @description Calculates the character difference from a discrete matrix
#'
#' @param matrix A discrete matrix or a list containing discrete characters.
#' 
#' @details
#' The character difference is calculated as @@@
#' 
#' @return
#' A character difference value or a matrix of class \code{char.diff}
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

        ## Calculating the gower distance
        options(warn = -1) #TG: NA's get introduced. Don't care!
        output <- as.matrix(.Call("C_char_diff", matrix, method, attrs))
        options(warn = 0)

        ## Calculating the character difference
        output <- round( 1 - ( abs(output-0.5)/0.5 ), digit = 10)

        class(output) <- c("matrix", "char.diff")

        return(output)

    } else {

        check.length(matrix, 2, " must contain only two elements", errorif = FALSE)
        ## Run the slower R version
        return(char.diff_R(matrix[[1]], matrix[[2]]))

    }
}


# #' @title Plots character differences
# #'
# #' @description Plots a correlation matrix from discrete character differences o
# #'
# #' @param matrix A discrete matrix or an already computed character difference matrix of class \code{char.diff}.
# #' @param type Either \code{"correlation"} (or \code{"c"}) or \code{"density"} (or \code{"d"}) for respectively plotting the correlation or the density of the matrix's characters.
# #' @param col Two colors for forming the gradient if \code{type = "correlation"} or for the density lines colors if \code{type = "density"}.
# #' @param legend A logical value stating whether to print the legend or not (default = \code{TRUE}).
# #' @param legend.title A \code{character} string to be displayed as the title of the legend (default = \code{Difference}).
# #' @param legend.pos The position of the legend. Can be two \code{numeric}. Default is \code{"topleft"}.
# #' @param axis A logical value stating whether to print the axis or not (default = \code{TRUE}).
# #' @param xlim Two \code{numeric} values to determine the x axis limits. If missing (default), the limits are calculated automatically to fit the plot window.
# #' @param ylim Two \code{numeric} values to determine the y axis limits. If missing (default), the limits are calculated automatically to fit the plot window.
# #' @param xlab A \code{character} string for the the x axis. Can be missing.
# #' @param ylab A \code{character} string for the the y axis. Can be missing.
# #' @param ... Any additional graphical arguments to be passed to \code{image}.
# #' 
# #' @examples
# #' ##
# #' 
# #' @author Thomas Guillerme
# #' @export
# #' 

# plot.char.diff <- function(matrix, type = "correlation", col = c("blue", "orange"), legend = TRUE, legend.title = "Difference", legend.pos = "topleft", axis = TRUE, xlim, ylim, xlab, ylab, ...) {

#     ## Saving the call
#     match_call <- match.call()

#     if(!any(class(matrix) == "matrix")) {
#         stop("")
#     }


#     if(type == "correlation") {
#         ## Remove the upper triangle
#         matrix[upper.tri(matrix)] <- NA

#         ## Setting the colours
#         colfunc <- grDevices::colorRampPalette(col)
#         colheat <- rev(colfunc(10))

#         ## Plotting the heat map
#         image(matrix, col = colheat, axes = FALSE, ...)
#         if(axis) {
#             axis(1, at = seq(from = 0, to = 1, length.out = ncol(matrix)), labels = FALSE, tick = TRUE)
#             axis(2, at = seq(from = 0, to = 1, length.out = ncol(matrix)), labels = FALSE, tick = TRUE)
#         }

#         ## Adding the legend
#         if(legend) {
#             legend("topleft", legend = c(as.character(round(max(matrix, na.rm = TRUE), 2)), as.character(round(min(matrix, na.rm = TRUE), 2))), title = legend.title, col = col, pch = 19)
#         }
#     } else {
#         plot.char.diff.density(matrix, main, legend, col, xlim, ylim, legend.pos, xlab, ylab)
#     }
# }