#' @title Simulates morphological data.
#'
#' @description Generates a morphological matrix using \code{\link[ape]{rTraitDisc}} or \code{\link[phyclust]{gen.seq.HKY}} functions.
#'
#' @param tree A phylogenetic tree to use for generating the characters.
#' @param characters The number of morphological characters to generate.
#' @param model Either an implemented (\code{"ER"}, \code{"HKY"} or \code{"MIXED"}) or user defined model (see details).
#' @param states A \code{numeric} string of probabilities for the number of states for each character (\code{default = 1}; i.e. 100\% binary state characters; see details).
#' @param rates A function an its parameters for the rates distribution (see details).
#' @param substitution A function an its parameters for the substitutions distribution (see details; \code{default = c(runif, 2, 2)}).
#' @param invariant \code{logical}, whether to allow any invariant sites (\code{default = TRUE}).
#' @param verbose Whether to be verbose or not (\code{default = FALSE}).
#'
#' @details
#' \itemize{
#' 
#' \item The \code{model} arguments must be either a user's defined function for generating the discrete morphological characters (that takes the states, rates and substitution arguments) or one of the two following:
#'      \itemize{
#'          \item \code{"ER"} uses the \code{ape::rTraitDisc} function with the \code{"ER"} model argument (= Mk model).
#'          \item \code{"HKY"} uses the \code{phyclust::gen.seq.HKY} function with \code{kappa} sampled from the \code{substitution} argument, \code{pi = runif(4)} (divided by \code{sum(runif(4))}), \code{rate.scale} sampled from the \code{rates} distribution and \code{L} being the number of \code{characters} and transforms the purines (A, G) into 0 and the pyrimidines (C, T) into 1.
#'          \item \code{"MIXED"} randomly uses \code{"ER"} or \code{"HKY"} for binary characters and \code{"ER"} for any character with more than two states.
#'          \item the user defined model must be a \code{function} that generates \emph{a single} discrete morphological character and takes one element from at least the following arguments: \code{tree}, \code{states}, \code{rates}, \code{substitution}.
#'      }
#'
#' \item The \code{states} argument attributes a number of states to each character by using the given probability vector for each number of states starting from two.
#' For example \code{states = c(0.7, 0.2, 0.1)} will generate 70\% of characters with two states, 20\% of characters with three states and 10\% of characters with four states. 
#' 
#' \item The \code{rates} and \code{substitution} arguments require a function that outputs a distribution and its optional parameters. For example \code{rates = c(runif, 1, 10)} creates a uniform distribution between 1 and 10 for the rates distribution.
#' 
#' }
#' 
#' @examples
#' 
#' set.seed(4)
#' ## A random tree with 15 tips
#' tree <- rcoal(15)
#' ## Setting up the parameters
#' my_rates = c(rgamma, rate = 10, shape = 5)
#' my_substitutions = c(runif, 2, 2)
#' 
#' ## HKY binary (15*50)
#' matrixHKY <- sim.morpho(tree, characters = 50, model = "HKY",
#'      rates = my_rates, substitution = my_substitutions)
#' 
#' ## Mk matrix (15*50) (for Mkv models)
#' matrixMk <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates) 
#' 
#' ## Mk invariant matrix (15*50) (for Mk models)
#' matrixMk <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates,
#'      invariant = FALSE)
#'
#' ## MIXED model invariant matrix (15*50)
#' matrixMixed <- sim.morpho(tree, characters = 50, model = "MIXED",
#'      rates = my_rates, substitution = my_substitutions,  invariant = FALSE,
#'      verbose = TRUE)
#' 
#' @seealso \code{\link{check.morpho}}, \code{\link[ape]{rTraitDisc}}, \code{\link[phyclust]{gen.seq.HKY}}
#' 
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG sim.morpho")
# source("sanitizing.R")
# source("sim.morpho_fun.R")

sim.morpho <- function(tree, characters, states = 1, model = "ER", rates, substitution = c(stats::runif, 2, 2), invariant = TRUE, verbose = FALSE)
{

    #SANITIZNG
    #tree
    check.class(tree, "phylo")

    #characters
    check.class(characters, "numeric")
    check.length(characters, 1, " must be a single numeric value.")

    #states
    check.class(states, "numeric")
    if(sum(states) != 1) {
        stop.call("", "States argument must sum up to 1.")
    }
    #Set to 1 if model is HKY
    if(length(states) > 1 && model == "HKY") {
        states <- 1
        warning("The HKY model only allows the use of two states characters (binary).")
    }

    #model
    if(class(model) != "function") {
        #model is not a sure function
        model <- toupper(model)
        implemented_models <- c("ER", "HKY", "MIXED")
        check.method(model, implemented_models, "The model")
        model_name <- model
        #Setting up the model
        if(class(model) != "function" && model_name == "ER") {
            model <- rTraitDisc.mk
            #Warning on the substitutions:
            substitution <- c(stats::runif, 1, 1)
            #message("Substitution parameter is ignored for the ER model.")
        }
        if(class(model) != "function" && model_name == "HKY") {
            model <- gen.seq.HKY.binary
        }
        if(class(model) != "function" && model_name == "MIXED") {
            model <- MIXED.model
        }

    } else {
        stop.call("", "User functions are not supported yet for the model argument.\nTry using \"HKY\", \"ER\" or \"MIXED\".")
        #Add checker for arguments to be passed to users function
    }

    #rate
    if(length(rates) == 1) {
        # Is only a function (default function arguments)
        check.class(rates, "function")
    } else {
        # Is a function with options
        check.class(rates[[1]], "function")
        # Check if arguments work
        test <- NULL ; try(test <- sample.distribution(1, rates), silent = TRUE)
        if(length(test) != 1) {
            stop.call("", "Error in rates argument format. Should be c(function, arg1, arg2, ...).")
        }
    }

    #substitution
    if(length(substitution) == 1) {
        # Is only a function (default function arguments)
        check.class(substitution, "function")
    } else {
        # Is a function with options
        check.class(substitution[[1]], "function")
        # Check if arguments work
        test <- NULL ; try(test <- sample.distribution(1, substitution), silent = TRUE)
        if(length(test) != 1) {
            stop.call("", "Error in substitution argument format. Should be c(function, arg1, arg2, ...).")
        }
    }

    #invariant
    check.class(invariant, "logical")

    #verbose
    check.class(verbose, "logical")

    #GENERATING THE CHARACTERS

    #Creating the matrix
    if(verbose == TRUE) cat(paste("Generating a matrix of ", characters, " characters for ", Ntip(tree), " taxa:", sep=""))
        matrix <- replicate(characters, model(tree = tree, states = states, rates = rates, substitution = substitution, verbose = verbose))
    if(verbose == TRUE) cat("Done.\n")


    if(invariant == FALSE) {
        if(any(apply(matrix, 2, is.invariant))) {
            if(verbose == TRUE) cat("Re-simulating ", length(which(apply(matrix, 2, is.invariant)) == TRUE), " invariant characters:", sep="") 
            #Repeat the invariant characters sampling
            while(any(apply(matrix, 2, is.invariant))) {
                matrix[, which(apply(matrix, 2, is.invariant) == TRUE)] <- replicate(length(which(apply(matrix, 2, is.invariant) == TRUE)), model(tree = tree, states = states, rates = rates, substitution = substitution, verbose = verbose))
                if(verbose == TRUE) cat(".")
            }
            if(verbose == TRUE) cat("Done.\n")
        }
    }

    #Adding the row names
    if(length(tree$tip.label) != 0) {
        rownames(matrix) <- tree$tip.label
    }
    return(matrix)
    
}