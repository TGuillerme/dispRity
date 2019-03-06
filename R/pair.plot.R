#' @title Plots pairwise comparisons
#'
#' @description Plots pairwise comparisons from a data frame (typically output from \code{\link{test.dispRity}}).
#'
#' @param data A \code{matrix} or a \code{data.frame} object with comparisons' pair names as row names. The number of rows must be equal to a pairwise combination of \code{n} elements (see details).
#' @param what A \code{numeric} or \code{character} value designating which column to plot.
#' @param col The two extremes of a color gradient (default = \code{c("black", "white")}).
#' @param legend Logical, whether to plot the legend or not.
#' @param binary Optional, if the results must be binary, a \code{numeric} value for the threshold of acceptance (values greater will be 1, lower will be 0).
#' @param diag Optional, can be \code{"max"} or \code{"min"} or a single \code{numeric} value.
#' @param add Optional, whether to add significance tokens can be \code{numeric} for a point type to print (\code{pch}) or \code{"character"} to print (e.g. \code{"*"}).
#' @param lower Optional, logical, whether to add tokens for values lower than \code{binary} (default is \code{TRUE}; \code{FALSE} will add tokens for values bigger than \code{binary}).
#' @param ... Any other options to be passed to \code{\link[graphics]{plot}}.
#' 
#' @details
#' The number of rows (i.e. comparisons) in \code{matrix} must be equal to the results of a pairwise combination.
#' In general, the number of rows \code{x} must satisfy the equation: \eqn{x  = n^2 / 2 - n / 2} where \code{n} must be an integer greater or equal than 2.
#'
#' @examples
#' ## A small matrix of two pairwise comparisons of seven elements (2*21 comparisons)
#' data <- matrix(data = runif(42), ncol = 2)
#' 
#' ## Plotting the first column as a pairwise comparisons
#' pair.plot(data, what = 1, col = c("orange", "blue"), legend = TRUE, diag = 1)
#' 
#' ## Adding some tokens for each value below 0.2 in the second column
#' pair.plot(data, what = 2, binary = 0.2, add = "*", cex = 2)
#' 
#' ## Loading disparity data
#' data(disparity)
#' 
#' ## Testing the pairwise difference between slices
#' tests <- test.dispRity(disparity, test = wilcox.test, correction = "bonferroni")
#' 
#' ## Plotting the significance
#' pair.plot(as.data.frame(tests), what = "p.value", binary = 0.05)
#' 
#' @seealso \code{\link{test.dispRity}}.
#'
#' @author Thomas Guillerme
#' @export

#testing
#source("sanitizing.R")

pair.plot <- function(data, what, col = c("black", "white"), legend = FALSE, binary, diag, add, lower = TRUE, ...){

    match_call <- match.call()

    #Sanitizing
    #data
    if(class(data) == "matrix") {
        data <- as.data.frame(data)
    } 
    check.class(data, "data.frame")
    #getting the column names
    if(length(grep("-", rownames(data))) != 0 | length(grep(":", rownames(data))) != 0) {
        elements <- unique(unlist(strsplit(rownames(data), split = " : ")))
    } else {
        #inferring the number of columns
        elements <- seq(1:find.num.elements(nrow(data)))
    }

    #what should exist
    check.length(what, 1, " must be a single 'numeric' or 'character' string designating which column to plot.")
    if(class(what) == "numeric") {
        results <- data[,what]
    } else {
        check.class(what, "character", msg = " must be a single 'numeric' or 'character' string designating which column to plot.")
        results <- data[, which(colnames(data) == what)]
    }
    if(length(results) == 0) {
        stop.call(match_call$what, " was not found in the data.frame.")
    }

    #col
    check.class(col, "character")
    if(missing(add)) {
        check.length(col, 2, " must be two colors for calculating the color gradient.")
    }

    col_grad <- grDevices::colorRampPalette(col)(10)

    #legend
    check.class(legend, "logical")

    #binary
    if(!missing(binary)) {
        check.class(binary, "numeric")
        check.length(binary, 1, " must be a single numeric character.")
        #Reset the color gradient.
        col_grad <- col
    }

    #diag
    if(!missing(diag)) {
        if(class(diag) == "character") {
            check.length(diag, 1, " must be a single numeric character or 'max' or 'min'.")
            if(diag != "max" && diag != "min") {
                stop.call("", "diag argument must be a single numeric character or 'max' or 'min'.")
            }     
        } else {
            check.class(diag, "numeric")
            check.length(diag, 1, " must be a single numeric character or 'max' or 'min'.")
        }
    }

    #lower
    check.class(lower, "logical")

    #add
    if(!missing(add)) {
        check.length(add, 1, " must be a single 'numeric' or 'character' string.")
        if(class(add) != "character" && class(add) != "numeric") {
            stop.call("", "add must be a single 'numeric' or 'character' string.")
        }
        if(missing(binary)) {
            stop.call("", "A threshold for binary argument must be provided.")
        }
        #Deactivate legend
        legend <- FALSE
    }

    #Generate the matrix
    matrix_plot <- matrix(NA, nrow = length(elements), ncol = length(elements), dimnames = list(c(elements), c(elements)))
    matrix_plot[lower.tri(matrix_plot)] <- results
    #Adding diagonal
    if(!missing(diag)) {
        if(diag == "max") diag(matrix_plot) <- max(results, na.rm = TRUE)
        if(diag == "min") diag(matrix_plot) <- min(results, na.rm = TRUE)
        if(class(diag) == "numeric") diag(matrix_plot) <- diag
    }

    #Applying threshold
    if(!missing(binary)) {
        matrix_plot <- ifelse(matrix_plot > binary, 1, 0)
    }

    if(missing(add)) {
        ## Correction if all results = 1
        if(all(matrix_plot[!is.na(matrix_plot)] == 1)) {
            col_grad <- rev(col_grad)
        }

        #Plotting the matrix
        image(matrix_plot, col = col_grad, axes = FALSE, ...)
        #image(matrix_plot, col = col_grad, axes = FALSE) ; warning("DEBUG")
        axis(1, seq(from = 0, to = 1, length = length(elements)), labels = elements, las = 2)
        axis(2, seq(from = 0, to = 1, length = length(elements)), labels = elements, las = 2)

        #Adding the legend
        if(legend == TRUE) {
            if(class(what) == "character") {
                legend(0, 1, pch = 15, col = col, c(paste("min =", min(matrix_plot, na.rm = TRUE)), paste("max =", max(matrix_plot, na.rm = TRUE))), bty="n", title = what)
            } else {
                legend(0, 1, pch = 15, col = col, c(paste("min =", min(matrix_plot, na.rm = TRUE)), paste("max =", max(matrix_plot, na.rm = TRUE))), bty="n")
            }
        }
    } else {
        #Selecting the 0s coordinates from the binary matrix
        if(lower == TRUE) {
            if(any(matrix_plot < binary, na.rm = TRUE)) {
                xs <- seq(from = 0, to = 1, length = length(elements))[which(matrix_plot == 0, arr.ind = TRUE)[,1]]
                ys <- seq(from = 0, to = 1, length = length(elements))[which(matrix_plot == 0, arr.ind = TRUE)[,2]]
                #Adding the 0s symbols
                if(class(add) == "numeric") {
                    #Symbol is a point
                    points(x = xs, y = ys, col = col[1], pch = add, ...)
                } else {
                    #Symbol is a character
                    text(x = xs, y = ys, add, col = col[1], ...)
                }
            }
        } else {
            if(any(matrix_plot > binary, na.rm = TRUE)) {
                xs <- seq(from = 0, to = 1, length = length(elements))[which(matrix_plot == 1, arr.ind = TRUE)[,1]]
                ys <- seq(from = 0, to = 1, length = length(elements))[which(matrix_plot == 1, arr.ind = TRUE)[,2]]
                #Adding the 0s symbols
                if(class(add) == "numeric") {
                    #Symbol is a point
                    points(x = xs, y = ys, col = col[1], pch = add, ...)
                } else {
                    #Symbol is a character
                    text(x = xs, y = ys, add, col = col[1], ...)
                }
            }
        }
    }
    #End
}
