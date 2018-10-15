# #' @title Generates a birth death tree.
# #'
# #' @description Generates a birth death tree with a set number of taxa and a random birth and death rate.
# #'
# #' @param n an integer giving the number of tips in the tree.
# #' 
# #' @author Thomas Guillerme

# rtree.bd <- function(n) {
#     #Random parameters selector
#     rand.birth.death <- function() {
#         lambda <- runif(1)
#         mu <- runif(1, 0, lambda)
#         return(cbind(lambda, mu))
#     }

#     tree <- diversitree::tree.bd(rand.birth.death(), max.taxa = n)

#     # Make sure tree is not null
#     while(is.null(tree)) {
#         tree <- diversitree::tree.bd(rand.birth.death(), max.taxa = n)
#     }

#     return(tree)
# }


#' @title Generates a contrast matrix.
#'
#' @description Creates a contrast matrix using the observed character states in an input matrix.
#'
#' @param matrix a discrete morphological character matrix.
#' 
#' @examples
#' ## A random multistate matrix
#' random_matrix <- matrix(sample(c(0,1,2), 100, TRUE), 10, 10)
#' 
#' ## Get the contrast matrix
#' get.contrast.matrix(random_matrix)
#' 
#' ## Adding inapplicable and missing data to the matrix
#' random_matrix[sample(1:100, 10)] <- "?"
#' random_matrix[sample(1:100, 10)] <- "-"
#' 
#' ## Get the contrast matrix
#' get.contrast.matrix(random_matrix)
#' 
#' @seealso \code{\link{check.morpho}}
#' 
#' @author Thomas Guillerme

get.contrast.matrix <- function(matrix) {
    
    ## Extracting the states
    states <- sort(unique(as.vector(matrix)))
    
    ## Check if there is a "?" token
    if(any(states == "?")) {
        ## remove the "?" state
        states_num <- states[-which(states == "?")]
        ## Create a simple square matrix with 0s...
        contrast_matrix <- matrix(data = rep(0, length(states_num)*length(states_num)), ncol = length(states_num), dimnames = list(as.character(states_num), as.character(states_num)))
        ## Set the diagonal to 1
        diag(contrast_matrix) <- 1
        ## Add the joker character as a row full of 1s
        joker_matrix <- matrix(data = rep(1, length(states_num)), ncol = length(states_num), dimnames = list("?", as.character(states_num)))
        contrast_matrix <- rbind(contrast_matrix, joker_matrix)
    } else {
        ## Create a simple square matrix with 0s...
        contrast_matrix <- matrix(data = rep(0, length(states)*length(states)), ncol = length(states), dimnames = list(as.character(states), as.character(states)))
        ## Set the diagonal to 0 
        diag(contrast_matrix) <- 1
    }

    return(contrast_matrix)
}


#' @title Apply inapplicable characters to a matrix.
#'
#' @description Apply inapplicable characters to discrete morphological matrix.
#'
#' @param matrix A discrete morphological matrix.
#' @param NAs Either a numeric value of how many characters to make inapplicable or vector of characters inapplicability source (either \code{"character"} or \code{"clade"}; see details). The length of this vector must be at maximum half the total number of characters.
#' @param tree If any inapplicable source is \code{"clade"}, a tree from where to select the clades.
#' @param invariant Whether to allow invariant sites among the characters with inapplicable data. If \code{invariant = FALSE} the algorithm will try to remove such characters (if possible).
#' @param verbose Whether to be verbose or not.
##' @param ... Any additional arguments.
#' 
#' @details
#' If the \code{NAs} argument is a numeric value n, generates n characters with inapplicable data based on the \code{"clade"} source.
#' 
#' The \code{NAs} argument intakes a vector of character inapplicability source rendering a number of characters inapplicable using the following sources:
#'      
#'      \code{"character"} draws inapplicable characters directly from the character matrix, ignoring the phylogeny (i.e. for a random character X, an other random character Y will have inapplicable characters for each character states 0 for character X).
#'      
#'      \code{"clade"} draws inapplicable characters from the phylogeny: it will randomly apply inapplicable characters states for some characters by randomly selecting clades from the provided tree. The algorithm randomly assigns an inapplicable token for this character for all taxa in this clade or all taxa outside this clade.
#' 
#' For example \code{NAs = c(rep("character", 2), rep("clade", 2))} will generate 4 characters with inapplicable data, two using previous characters and two other using random clades.
#' 
#' @examples
#' set.seed(4)
#' ## A random tree with 15 tips
#' tree <- rcoal(15)
#' ## setting up the parameters
#' my_rates = c(rgamma, rate = 10, shape = 5)
#' my_substitutions = c(runif, 2, 2)
#'
#' ## A Mk matrix (10*50)
#' matrixMk <- sim.morpho(tree, characters = 100, model = "ER",
#'      states = c(0.85, 0.15), rates = my_rates, invariant = FALSE)
#' 
#' ## Setting the number and source of inapplicable characters
#' my_inapplicables <- c(rep("character", 5), rep("clade", 5))
#' 
#' ## Apply some inapplicable characters to the matrix
#' matrix <- apply.NA(matrixMk, my_inapplicables, tree, verbose = TRUE)
#'
#' @seealso \code{\link{sim.morpho}}
#' 
#' @author Thomas Guillerme

apply.NA <- function(matrix, NAs, tree, invariant = FALSE, verbose = FALSE){#, ...) {

    ## SANITIZING
    ## matrix
    check.class(matrix, "matrix")

    ## inapplicables
    class_inapplicable <- check.class(NAs, c("character", "numeric"))
    if(class_inapplicable == "character") {
        inap.source_options <- c("character", "clade")

        if(!all(NAs %in% inap.source_options)) {
            stop.call("", paste0("NAs argument must be a vector containing at least one of the following: \"", paste(inap.source_options, collapse = "\", \""), "\"."))
        }        
        if(length(NAs) > ncol(matrix)/2) {
            stop.call("", "Only half the number of characters can be NAs.")
        }
    } else {
        if(NAs > ncol(matrix)/2) {
            stop.call("", "Only half the number of characters can be NAs.")
        }
        NAs <- rep("character", NAs)
    }

    ## tree
    if(any(NAs == "clade") && missing(tree)) {
        stop.call("", "Tree argument is missing for applying inapplicable characters on random clades.")
    } else {
        if(any(NAs == "clade")) {
            ## tree must be same size as the matrix
            if(any(sort(row.names(matrix)) != sort(tree$tip.label))) {
                stop.call("", "Provided tree doe not have the same taxa as the matrix.")
            }
        }
    }

    ## invariant
    check.class(invariant, "logical")

    ## verbose
    check.class(verbose, "logical")

    ## APPLY INAPPLICABLE CHARACTERS
    ## Setting the output matrix
    matrix_out <- matrix

    ## From the characters first (if any)
    if(any(NAs == "character")) {
        ## Get the number of inapplicable characters
        inapplicables_characters <- length(which(NAs == "character"))
        ## Create the characters to make inapplicable
        target_characters <- sample(seq(from = 2, to = ncol(matrix), by = 2), inapplicables_characters)
        ## Create the characters to match inapplicability from
        pattern_characters <- target_characters-1

        ## Get the target characters with inapplicables
        matrix_inapplicable <- mapply(mapply.inap.character, as.list(target_characters), as.list(pattern_characters), MoreArgs=list(matrix, invariant))

        ## Invariant warning
        if(invariant == FALSE) {
            invariants <- length(which(apply(matrix_inapplicable, 2, function(X) length(unique(X))) <= 1))
            if(invariants != 0) {
                warning(paste(invariants, "characters are now invariant due inapplicable data."))
            }
        }

        ## Include these characters in the matrix
        matrix_out[,target_characters] <- matrix_inapplicable
    } else {
        inapplicables_characters <- 0
        target_characters <- NULL
    }

    ## From the clades (if any)
    if(any(NAs == "clade")) {
        ## Get the number of inapplicable characters
        inapplicables_clades <- length(which(NAs == "clade"))
        
        ## Create the characters to make inapplicable (after the inapplicable characters if any)
        if(!is.null(target_characters)) {
            target_characters <- sample(seq(from = 2, to = ncol(matrix), by = 2)[-target_characters], inapplicables_clades)
        } else {
            target_characters <- sample(seq(from = 2, to = ncol(matrix), by = 2), inapplicables_clades)
        }

        ## Get the target characters with inapplicables
        matrix_inapplicable <- matrix(unlist(lapply(as.list(target_characters), lapply.inap.clade, matrix, tree, invariant)), ncol = inapplicables_clades, byrow = FALSE, dimnames = list(rownames(matrix)))

        ## Include these characters in the matrix
        matrix_out[,target_characters] <- matrix_inapplicable
    }

    return(matrix_out)
}