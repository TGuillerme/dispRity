#' @title Plots the correlation matrix
#'
#' @description Plots the correlation matrix.
#'
#' @param X,Y Two morphological characters.
#' @param missing The algorithm to deal with missing characters. Either \code{"Max"} or \code{"Gower"}. See details.
#' 
#' @details
#' \code{missing = "Max"} maximises the difference between two characters by counting the missing character states tokens (\code{"?"}) as a separate state. In this scenario the difference between \code{A = c(0,1,1,1)} and \code{B = c(0,0,?,?)} is always maximise.
#' \code{missing = "Gower"} uses a Gower (1966) like character comparisons: only the comparable characters states are used. In the same example as above, only \code{A = c(0,1)} and \code{B = c(0,0)} will be used.
#' 
#' @examples
#' ##
#' 
#' @author Thomas Guillerme
#' @export
#' 
#Gower J.C. 1966. Some distance properties of latent root and vector methods used in multivariate analysis. Biometrika 53:325–338.


# TODO: deal with '&' characters
# TODO: deal with '?' and a minimising algorithm


#dyn.load("char.diff.so")


char.diff <- function (x, method = "Gower")  {

    ## Convert matrix (if not numeric)
    if(!all(apply(x, 2, class) == "numeric")) {
        options(warn = -1)
        x <- apply(x, 2, as.numeric)
        options(warn = 0)
    }

    ## Options to remove:
    diag = FALSE
    upper = FALSE
    #p = 2

    ## Getting the matrix parameters
    #x <- apply(x, 2, normalise.character)
    x <- t(x)
    N <- nrow(x)
    
    ## Setting the attributes
    attrs <- list(Size = N, Labels = dimnames(x)[[1L]], Diag = diag, Upper = upper, method = method, call = match.call(),  class = "dist")

    ## Running the C distance code
    options(warn = -1)
    output <- as.matrix(.Call("CharDiff", x, method, attrs))
    options(warn = 0)

    ## Calculating the character difference
    return(round( 1 - ( abs(output-0.5)/0.5 ), digit = 10))
}


## R version of the code (SLOW!)
# char.diff_R <- function(X,Y){ 

#     # Convert character
#     convert.character <- function(X) {
#         if(class(X) == "numeric") {
#             X <- LETTERS[X+1]
#         } else {
#             X <- as.factor(X)
#             levels(X) <- 1:length(levels(X))
#             X <- as.numeric(X)
#         }
#         return(X)
#     }

#     # Transform states into similar values
#     normalise.character <- function(X, states) {
#         # Convert X to character
#         if(class(X) != "numeric") {
#             X <- convert.character(X)
#         }
#         X <- as.character(X)
#         # Get the states of X
#         states_match <- sort(match(states, X))

#         # Replacing the original states
#         for(state in 1:length(states)) {
#             X <- gsub(X[states_match[state]], LETTERS[state], X)
#         }
#         X <- convert.character(X)
#         return(X)
#     }

#     # Convert the characters to numeric (if needed)
#     if(class(X) != "numeric") {
#         X <- convert.character(X)
#     }
#     if(class(Y) != "numeric") {
#         Y <- convert.character(Y)
#     }

#     # Remove any uncomparable characters
#     na_X <- which(is.na(X))
#     na_Y <- which(is.na(Y))
#     if(length(c(na_X, na_Y)) > 0) {
#         X <- X[-c(na_X, na_Y)]
#         Y <- Y[-c(na_X, na_Y)]
#     }
    
#     if(length(X) == 0) {
#         return(NA)
#     }

#     # Check if characters are binary
#     states_X <- as.numeric(levels(as.factor(X)))
#     states_Y <- as.numeric(levels(as.factor(Y)))

#     if(length(states_X) <= 2 & length(states_Y) <= 2) {
#         # Simple binary Fitch comparison (fast)
#         differences <- ifelse(X-Y != 0, 1, 0)

#         # Calculate the difference
#         return( round( 1 - ( abs(sum(abs(differences))/length(X)-0.5)/0.5 ), digit = 10))
#     } else {
#         # Normalise the characters
#         X <- normalise.character(X, states_X)
#         Y <- normalise.character(Y, states_Y)

#         # Calculate the differences
#         differences <- X-Y

#         #Default fitch for now.
#         #type <- "Fitch"

#         #if(type == "Fitch") {
#             # Make the differences binary (i.e. if the difference is != 0, set to 1)
#             differences <- ifelse(differences != 0, 1, 0)
#         #}

#         # Get the characters difference
#         return( round( 1 - ( abs(sum(abs(differences))/length(X)-0.5)/0.5 ), digit = 10))
#     }
# }
