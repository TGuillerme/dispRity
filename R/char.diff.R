#' @title Character differences
#'
#' @description Calculates the character difference from a discrete matrix
#'
#' @param matrix A discrete matrix or a list containing discrete characters. The differences is calculated between the columns (usually characters). Use \code{t(matrix)} or \code{by.col = FALSE} to calculate the differences between the rows.
#' @param method The method to measure difference: \code{"hamming"} (default; Hamming 1950), \code{"manhattan"}, \code{"comparable"}, \code{"euclidean"}, \code{"maximum"}, \code{"mord"} (Lloyd 2016), \code{"none"} or \code{"binary"}.
#' @param translate \code{logical}, whether to translate the characters following the \emph{xyz} notation (\code{TRUE} - default; see details - Felsenstein 2004) or not (\code{FALSE}). Translation works for up to 26 tokens per character.
#' @param special.tokens optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that \code{NA} values are not compared and that the symbol "@" is reserved and cannot be used.
#' @param special.behaviours optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.
#' @param order \code{logical}, whether the character should be treated as order (\code{TRUE}) or not (\code{FALSE} - default). This argument can be a \code{logical} vector equivalent to the number of rows or columns in \code{matrix} (depending on \code{by.col}) to specify ordering for each character.
#' @param by.col \code{logical}, whether to measure the distance by columns (\code{TRUE} - default) or by rows (\code{FALSE}).
#' @param correction optional, an eventual \code{function} to apply to the matrix after calculating the distance.
#' 
#' 
#' @details
#' 
#' Each method for calculating distance is expressed as a function of \eqn{d(x, y)} where \eqn{x} and \eqn{y} are a pair of columns (if \code{by.col = TRUE}) or rows in the matrix and \emph{n} is the number of comparable rows (if \code{by.col = TRUE}) or columns between them and \emph{i} is any specific pair of rows (if \code{by.col = TRUE}) or columns.
#' The different methods are:
#' 
#' \itemize{
#'      \item \code{"hamming"} The relative distance between characters. This is equal to the Gower distance for non-numeric comparisons (e.g. character tokens; Gower 1966).
# \eqn{\sum (1/n) / T}
#'          \eqn{d(x,y) = \sum[i,n](abs(x[i] - y[i])/n}
#'      \item \code{"manhattan"} The "raw" distance between characters:
#'          \eqn{d(x,y) = \sum[i,n](abs(x[i] - y[i])}
#'      \item \code{"comparable"} The number of comparable characters (i.e. the number of tokens that can be compared):
#'          \eqn{d(x,y) = \sum[i,n]((x[i] - y[i])/(x[i] - y[i]))}
#'      \item \code{"euclidean"} The euclidean distance between characters:
#'          \eqn{d(x,y) = \sqrt(\sum[i,n]((x[i] - y[i])^2))}
#'      \item \code{"maximum"} The maximum distance between characters:
#'          \eqn{d(x,y) = max(abs(x[i] - y[i]))}
#'      \item \code{"mord"} The maximum observable distance between characters (Lloyd 2016):
#'          \eqn{d(x,y) =  \sum[i,n](abs(x[i] - y[i])/\sum[i,n]((x[i] - y[i])/(x[i] - y[i])}
#'      \item \code{"none"} Returns the matrix with eventual converted and/or translated tokens.
#'      \item \code{"binary"} Returns the matrix with the binary characters.
#' }
#' 
#' When using \code{translate = TRUE}, the characters are translated following the \emph{xyz} notation where the first token is translated to 1, the second to 2, etc. For example, the character \code{0, 2, 1, 0} is translated to \code{1, 2, 3, 1}. In other words when \code{translate = TRUE}, the character tokens are not interpreted as numeric values. When using \code{translate = TRUE}, scaled metrics (i.e \code{"hamming"} and \code{"gower"}) are divide by \eqn{n-1} rather than \eqn{n} due to the first character always being equal to 1.
#' 
#' \code{special.behaviours} allows to generate a special rule for the \code{special.tokens}. The functions should can take the arguments \code{character, all_states} with \code{character} being the character that contains the special token and \code{all_states} for the character (which is automatically detected by the function). By default, missing data returns and inapplicable returns \code{NA}, and polymorphisms and uncertainties return all present states.
#' 
#' \itemize{
#'      \item{\code{missing = function(x,y) NA}}
#'      \item{\code{inapplicable = function(x,y) NA}}
#'      \item{\code{polymorphism = function(x,y) strsplit(x, split = "\\\\&")[[1]]}}
#'      \item{\code{uncertainty = function(x,y) strsplit(x, split = "\\\\/")[[1]]}}
#' }
#'
#' Functions in the list must be named following the special token of concern (e.g. \code{missing}), have only \code{x, y} as inputs and a single output a single value (that gets coerced to \code{integer} automatically). For example, the special behaviour for the special token \code{"?"} can be coded as: \code{special.behaviours = list(missing = function(x, y) return(y)} to make all comparisons containing the special token containing \code{"?"} return any character state \code{y}.
#' 
#' IMPORTANT: Note that for any distance method, \code{NA} values are skipped in the distance calculations (e.g. distance(\code{A = {1, NA, 2}, B = {1, 2, 3}}) is treated as distance(\code{A = {1, 2}, B = {1, 3}})).
#' 
#' IMPORTANT: Note that the number of symbols (tokens) per character is limited by your machine's word-size (32 or 64 bits). If you have more than 64 tokens per character, you might want to use continuous data.
#' 
#' @return
#' A character difference value or a matrix of class \code{char.diff}
#' 
#' @examples
#' ## Comparing two binary characters
#' char.diff(list(c(0, 1, 0, 1), c(0, 1, 1, 1)))
#' 
#' ## Pairwise comparisons in a morphological matrix
#' morpho_matrix <- matrix(sample(c(0,1), 100, replace = TRUE), 10)
#' char.diff(morpho_matrix)
#' 
#' ## Adding special tokens to the matrix
#' morpho_matrix[sample(1:100, 10)] <- c("?", "0&1", "-")
#' char.diff(morpho_matrix)
#' 
#' ## Modifying special behaviours for tokens with "&" to be treated as NA
#' char.diff(morpho_matrix,
#'           special.behaviours = list(polymorphism = function(x,y) return(NA)))
#' 
#' ## Adding a special character with a special behaviour (count "%" as "100")
#' morpho_matrix[sample(1:100, 5)] <- "%"
#' char.diff(morpho_matrix,
#'           special.tokens = c("paragraph" = "\\%"),
#'           special.behaviours = list(paragraph = function(x,y) as.integer(100)))
#' 
#' ## Comparing characters with/without translation
#' char.diff(list(c(0, 1, 0, 1), c(1, 0, 1, 0)), method = "manhattan")
#' # no character difference
#' char.diff(list(c(0, 1, 0, 1), c(1, 0, 1, 0)), method = "manhattan",
#'           translate = FALSE)
#' # all four character states are different
#' 
#' @seealso \code{\link{plot.char.diff}}, \code{\link[vegan]{vegdist}}, \code{\link[stats]{dist}}, \code{\link[Claddis]{calculate_morphological_distances}}, \code{\link[cluster]{daisy}}
#' 
#' @author Thomas Guillerme
#' 
#' @references
#' Felsenstein, J. \bold{2004}. Inferring phylogenies vol. 2. Sinauer Associates Sunderland.
#' Gower, J.C. \bold{1966}. Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika 53:325-338.
#' Hamming, R.W. \bold{1950}. Error detecting and error correcting codes. The Bell System Technical Journal. DOI: 10.1002/j.1538-7305.1950.tb00463.x.
#' Lloyd, G.T. \bold{2016}. Estimating morphological diversity and tempo with discrete character-taxon matrices: implementation, challenges, progress, and future directions. Biological Journal of the Linnean Society. DOI: 10.1111/bij.12746. 
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


# method = "hamming"
# translate = TRUE
# special.tokens <- character()
# special.behaviours <- list()
# order <- FALSE


char.diff <- function(matrix, method = "hamming", translate = TRUE, special.tokens, special.behaviours, order = FALSE, by.col = TRUE, correction) {

    match_call <- match.call()

    ## Sanitizing
    matrix_class <- check.class(matrix, c("matrix", "list"))
    if(matrix_class == "list") {
        ## Check length
        if(length(matrix) != 2) {
            stop(paste0("When matrix argument is a list, it must contain only two elements.\nYou can convert ", as.expression(match_call$matrix), " to a matrix using:\n", as.expression(match_call$matrix), " <- do.call(rbind, ", as.expression(match_call$matrix), ")"))
        }

        ## Convert into a matrix
        matrix <- matrix(c(unlist(matrix)), byrow = FALSE, ncol = 2)
    }
    ## Save the dimension names
    matrix_dimnames <- dimnames(matrix)

    ## Convert to character
    if(class(matrix[[1]]) %in% c("numeric", "integer")) {
        matrix <- apply(matrix, 2, as.character)
    }

    ## Checking for the reserved character
    reserved <- grep("\\@", matrix)
    if(length(reserved) > 0) {
        stop("The matrix cannot contain the character '@' since it is reserved for the dispRity::char.diff function.")
    }

    ## Method is hamming by default
    avail_methods <- c("hamming", "manhattan", "comparable", "euclidean", "maximum", "mord", "none", "binary")
    check.method(method, avail_methods, msg = "method")
    c_method <- pmatch(method, avail_methods)

    ## Special tokens
    if(missing(special.tokens)) {
        special.tokens <- character()
    }
    check.class(special.tokens, c("character", "logical")) # Can be NA
    not.exist <- function(special.tokens, token) {
        name_token <- names(special.tokens[token])
        return(is.null(name_token) || is.na(name_token))
    }
    if(not.exist(special.tokens, "missing")) {
        special.tokens["missing"] <- "\\?"
    }
    if(not.exist(special.tokens, "inapplicable")) {
        special.tokens["inapplicable"] <- "\\-"
    }
    if(not.exist(special.tokens, "polymorphism")) {
        special.tokens["polymorphism"] <- "\\&"
    }
    if(not.exist(special.tokens, "uncertainty")) {
        special.tokens["uncertainty"] <- "\\/"
    }

    ## Checking for the reserved character
    reserved <- c("\\@", "@") %in% special.tokens
    if(any(reserved)) {
        stop("special.tokens cannot contain the character '@' since it is reserved for the dispRity::char.diff function.")
    }

    ## Checking whether the special.tokens are unique
    if(length(unique(special.tokens)) != length(special.tokens)) {
        stop("special.tokens cannot contain duplicated tokens.")
    }

    ## If any special token is NA, convert them as "N.A" temporarily
    if(any(is.na(special.tokens))) {
        matrix <- ifelse(is.na(matrix), "na", matrix)
        special.tokens[is.na(special.tokens)] <- "na"
    }

    ## Special behaviours
    if(missing(special.behaviours)) {
        special.behaviours <- list()
    }
    check.class(special.behaviours, "list")
    if(is.null(special.behaviours$missing)) {
        special.behaviours$missing <- function(x,y) return(NA)
    }
    if(is.null(special.behaviours$inapplicable)) {
        special.behaviours$inapplicable <- function(x,y) return(NA)
    }
    if(is.null(special.behaviours$polymorphism)) {
        special.behaviours$polymorphism <- function(x,y) return(strsplit(x, split = "\\&")[[1]])
    }
    if(is.null(special.behaviours$uncertainty)) {
        special.behaviours$uncertainty <- function(x,y) return(strsplit(x, split = "\\/")[[1]])
    }

    ## Match the behaviours and tokens in the same order
    special.behaviours <- special.behaviours[sort(names(special.behaviours))]
    special.tokens <- special.tokens[sort(names(special.tokens))]

    ## by.col
    check.class(by.col, "logical")

    ## translate
    check.class(translate, "logical")

    ## correction
    if(!missing(correction)) {
        check.class(correction, "function")
        test_correction <- make.metric(correction, silent = TRUE)$type
        if(!is.null(test_correction) && test_correction == "error") {
            stop("Incorrect correction function.")
        }
    }

    ## Translate characters (by token)
    if(translate) {
        matrix <- apply(matrix, 2, translate.xyz, special.tokens)
    }
    ## Convert as integer for C
    translate <- as.integer(translate)

    ## return translated
    if(method == "none") {
        return(matrix)
    }

    ## Convert to bitwise format
    suppressWarnings(matrix <- apply(matrix, 2, convert.bitwise, special.tokens, special.behaviours))

    ## return binarised
    if(method == "binary") {
        return(matrix)
    }

    ## order
    check.class(order, "logical")
    if(length(order) > 1) {
        ## Checking the ordering vector
        check.length(order, ifelse(!by.col, ncol(matrix), nrow(matrix)), msg = paste0(" must be of the same length as the number of ", ifelse(!by.col, "columns", "rows"), " in the matrix (", ifelse(!by.col, ncol(matrix), nrow(matrix)), ")."))
    } else {
        ## Creating the ordering vector for each comparisons
        # n <- ifelse(by.col, ncol(matrix), nrow(matrix))
        # order <- rep(order, (n*(n-1))/2)
        order <- rep(order, ifelse(!by.col, ncol(matrix), nrow(matrix)))
    }
    ## Coercing the logical vector into an integer one
    order <- as.integer(order)

    ## Making the matrix as integers
    # matrix <- apply(matrix, c(1,2), as.integer)

    ## Options to remove:
    diag = FALSE
    upper = FALSE

    ## Getting the matrix parameters
    if(by.col){
        matrix <- t(matrix)
        labels <- matrix_dimnames[[2]]
    } else {
        labels <- matrix_dimnames[[1]]
    }
    nrows <- nrow(matrix)
    if(is.null(labels)) {
        labels <- seq(1:nrows)
    }

    ## Setting the attributes
    attrs <- list(Size = nrows,
                  Labels = labels,
                  Diag = diag,
                  Upper = upper,
                  method = method,
                  call = match.call(),
                  class = "dist")

    ## Calculating the gower distance
    suppressWarnings(output <- as.matrix(.Call("C_bitwisedist", matrix, c_method, translate, order, attrs)))

    ## Remove NAs for diagonal if "comparable"
    if(method == "comparable") {
        ## Get comparable characters for the diagonal
        diag(output) <- apply(matrix, 1, function(x, ncol){return(ncol - sum(is.na(x)))}, ncol = ncol(matrix))
    }

    if(ncol(output) == 2) {
        ## Return a single numeric value if comparing two characters
        output <- as.numeric(output[1,2])
        return(output)
    }

    if(!missing(correction)) {
        ## Apply the correction
        output <- correction(output)
    }

    class(output) <- c("matrix", "char.diff")

    return(output)
}
