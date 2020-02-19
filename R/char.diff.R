#' @title Character differences
#'
#' @description Calculates the character difference from a discrete matrix
#'
#' @param matrix A discrete matrix or a list containing discrete characters. The differences is calculated between the columns (usually characters). Use \code{t(matrix)} to calculate the differences between the rows.
#' @param method The method to measure difference: \code{"hamming"} (default), \code{"gower"}, \code{"euclidean"}, \code{"ged"} or \code{"mord"}.
#' @param translate \code{logical}, whether to translate the characters following the \emph{xyz} notation (\code{TRUE} - default; see details - Felsenstein XXX) or not (\code{FALSE}). Translation works for up to 26 tokens per character.
#' @param special.tokens optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that the symbol "@" is reserved and cannot be used.
#' @param special.behaviours optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.
#' @param ordered \code{logical}, whether the character should be treated as ordered (\code{TRUE}) or not (\code{FALSE} - default). This argument can be a \code{logical} vector equivalent to the number of rows in \code{matrix} to specify ordering for each character.
#' @param algorithm "normal" or "bitwise" DEBUG
#' 
#' 
#' @details
#' The different distances to calculate are:
#' \itemize{
#'      \item \code{"hamming"} The scaled hamming distance: the relative distance between each pairs of comparable characters (i.e. does not interpret the character token significance; the differences are non-ordered Fitch-like: 0-2 = 1).
#'      \item \code{"gower"} The scaled gower distance: the absolute distance between each pairs of comparable characters (e.g. does interpret the character token significance; the differences are ordered and absolute: 0-2 = 2). The characters are translated following the \emph{xyz} notation (The first token is translated to 1, the second to 2, etc. - Felsenstein XXX).
#'      \item \code{"euclidean"} The Euclidean distance: XXX
#'      \item \code{"manhattan"} The Manhattan distance: XXX
#'      \item \code{"ged"} The Generalised Euclidean Distance (Wills XXX): XXX
#'      \item \code{"mord"} The maximum observable distance (Lloyd XXX): XXX
#' }
#' 
#' When using \code{translate = TRUE}, the characters are translated following the \emph{xyz} notation where the first token is translated to 1, the second to 2, etc. For example, the character \code{0, 2, 1, 0} is translated to \code{1, 2, 3, 1}. In other words when \code{translate = TRUE}, the character tokens are not interpreted as numeric values. When using \code{translate = TRUE}, scaled metrics (i.e \code{"hamming"} and \code{"gower"}) are divide by \eqn{n-1} rather than \eqn{n} due to the first character always being equal to 1.
#' 
#' \code{special.behaviours} allows to generate a special rule for the \code{special.tokens}. The functions should can take the arguments \code{character, all_states} with \code{character} being the character that contains the special token and \code{all_states} for the character (which is automatically detected by the function). By default, missing data returns all states, polymorphisms and uncertainties return all present states and inapplicable returns an \code{NA}. Note that \code{NA}s are skipped in the distance calculations.
#' 
#' \itemize{
#'      \item{code{missing = function(x,y) as.integer(y)}}
#'      \item{code{inapplicable = function(x,y) NA}}
#'      \item{code{polymorphism = function(x,y) as.integer(strsplit(x, split = "\\\\&")[[1]])}}
#'      \item{code{uncertainty = function(x,y) as.integer(strsplit(x, split = "\\\\/")[[1]])}}
#' }
#'
#' \code{x, y} as only inputs and should output a single value. Functions in the list should be named following the special token of concern (\code{x}). Elements of the list should be named as in \code{special.tokens}. For example, the special behaviour for the special token \code{"?"} can be coded as: \code{special.behaviours = list(missing = function(x, y) return(NA))} to make all comparisons containing the special token containing \code{"?"} return a difference of \code{NA}.
#' 
#' IMPORTANT: Note that the number of symbols per character is limited to the number of bytes in your machine (32 or 64).
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
#' ## Comparing morphological characters as text
#' 
#' ## Using different special tokens
#' 
#' ## Using different special behaviours
#' 
#' @seealso \code{\link{plot.char.diff}}.
#' 
#' @author Thomas Guillerme
#' 
#' @references
#' Wills XXX
#' Hamming XXX
#' Lloyd XXX
#' Felsenstein XXX
#' Gower, J.C. 1966. Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika 53:325-338.
#' 




# TESTING
# Some character
# char1 <- c("0", "0", "1", "?", "0")
# char2 <- c("1", "1", "2", "?", "3")
# matrix <- list(char1, char2)
#      [,1] [,2]
# [1,]    1    1
# [2,]    1    1
# [3,]    2    2
# [4,]    3    7
# [5,]    1    4





char.diff <- function(matrix, method = "hamming", translate = TRUE, special.tokens, special.behaviours, ordered = FALSE, algorithm = "double") {

    options(warn = -1)

    ## Sanitizing
    matrix_class <- check.class(matrix, c("matrix", "list"))
    if(matrix_class == "list") {
        ## Check length
        check.length(matrix, 2, " must contain only two elements", errorif = FALSE)

        ## Convert into a matrix
        matrix <- matrix(c(unlist(matrix)), byrow = FALSE, ncol = 2)
    }

    ## Convert to character
    if(unique(apply(matrix, 2, class))[1] == "numeric") {
        matrix <- apply(matrix, 2, as.character)
    }

    ## Checking for the reserved character
    reserved <- grep("\\@", matrix)
    if(length(reserved) > 0) {
        stop("The matrix cannot contain the character '@' since it is reserved for this function.")
    }

    ## Method is Gower by default
    avail_methods <- c("hamming", "gower", "manhattan", "euclidean", "ged", "mord", "bitwise")
    check.method(method, avail_methods, msg = "method")
    if(method == "bitwise") {
        warning("Method name debug")
    }

    ## Special tokens
    if(missing(special.tokens)) {
        special.tokens <- character()
    }
    check.class(special.tokens, c("character", "logical")) # can also be NA
    if(is.na(special.tokens["missing"])) {
        special.tokens["missing"] <- "\\?"
    }
    ## Convert NAs to "?" if badly converted
    if(any(is.na(matrix))) {
        matrix <- ifelse(is.na(matrix), special.tokens["missing"], matrix)
        warning(paste0("numeric NAs where converted to \"", special.tokens["missing"], "\"."))
    }
    if(is.na(special.tokens["inapplicable"])) {
        special.tokens["inapplicable"] <- "\\-"
    }
    if(is.na(special.tokens["polymorphism"])) {
        special.tokens["polymorphism"] <- "\\&"
    }
    if(is.na(special.tokens["uncertainty"])) {
        special.tokens["uncertainty"] <- "\\/"
    }

    ## Checking for the reserved character
    reserved <- grep("\\@", special.tokens)
    if(length(reserved) > 0) {
        stop("special.tokens cannot contain the character '@' since it is reserved for this function.")
    }

    ## Special behaviours
    if(missing(special.behaviours)) {
        special.behaviours <- list()
    }
    check.class(special.behaviours, "list")
    if(is.null(special.behaviours$missing)) {
        special.behaviours$missing <- function(x,y) return(as.integer(y))
    }
    if(is.null(special.behaviours$inapplicable)) {
        special.behaviours$inapplicable <- function(x,y) return(NA)
    }
    if(is.null(special.behaviours$polymorphism)) {
        special.behaviours$polymorphism <- function(x,y) return(as.integer(strsplit(x, split = "\\&")[[1]]))
    }
    if(is.null(special.behaviours$uncertainty)) {
        special.behaviours$uncertainty <- function(x,y) return(as.integer(strsplit(x, split = "\\/")[[1]]))
    }

    ## translate
    check.class(translate, "logical")
    translate <- as.integer(translate)

    ## Translate characters (by token)
    if(translate) {
        matrix <- apply(matrix, 2, translate.xyz, special.tokens)
    }

    ## Convert to bitwise format
    matrix <- apply(matrix, 2, convert.character, special.tokens, special.behaviours)

    ## ordered
    check.class(ordered, "logical")
    if(length(ordered) > 1) {
        check.length(ordered, ncol(matrix), msg = paste0(" must be of the same length as the number of columns in the matrix (", ncol(matrix), ")."))
        ## Split the matrix in two if there are two ordered/non-ordered characters
        double_matrix <- TRUE
        ordered_matrix <- matrix[,ordered]
        unorder_matrix <- matrix[,!ordered]
        stop("Does not work with different ordered/unord characters yet.")
    } else {
        double_matrix <- FALSE
        order <- as.integer(ordered)
    }

    if(algorithm != "bitwise") {
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
    } else {
        warning("DEBUG: char.diff with bitwise distance.")

        ## Making the matrix as integers
        matrix <- apply(matrix, c(1,2), as.integer)

        ## Options to remove:
        diag = FALSE
        upper = FALSE

        ## Getting the matrix parameters
        matrix <- t(matrix)
        nrows <- nrow(matrix)
        
        ## Setting the attributes
        attrs <- list(Size = nrows,
                      Labels = dimnames(matrix)[[1L]],
                      Diag = diag,
                      Upper = upper,
                      method = method,
                      call = match.call(),
                      class = "dist")

        ## Calculating the gower distance
        options(warn = -1) #TG: NA's get introduced. Don't care!
        output <- as.matrix(.Call("C_bitwisedist", matrix, method, translate, order, attrs))
        options(warn = 0)
    }

    ## Calculating the character difference
    #output <- round( 1 - ( abs(output-0.5)/0.5 ), digits = 10)
    output <- round(output, digits = 10)

    if(ncol(output) == 2) {
        ## Return a single numeric value if comparing two characters
        output <- as.numeric(output[1,2])
        options(warn = 0)
        return(output)
    }

    class(output) <- c("matrix", "char.diff")
    
    options(warn = 0)

    return(output)
}