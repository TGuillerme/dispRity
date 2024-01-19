#' @title Ancestral states estimations with multiple trees
#'
#' @description Fast ancestral states estimations run on multiple trees using the Mk model from castor::asr_mk_model.
#'
#' @param data A \code{matrix}, \code{data.frame} or \code{list} with the characters for each taxa.
#' @param tree A \code{phylo} or \code{mutiPhylo} object (if the \code{tree} argument contains node labels, they will be used to name the output).
#' @param models A \code{vector} of models to be passed to \code{castor::asr_mk_model}.


#TODO: add info about continuous characters


#' @param threshold either \code{logical} for applying a relative threshold (\code{TRUE} - default) or no threshold (\code{FALSE}) or a \code{numeric} value of the threshold (e.g. 0.95). See details.
#' @param special.tokens optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that \code{NA} values are not compared and that the symbol "@" is reserved and cannot be used.
#' @param special.behaviours optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.
#' @param brlen.multiplier optional, a vector of branch length modifiers (e.g. to convert time branch length in changes branch length) or a list of vectors (the same length as \code{tree}).
#' @param verbose \code{logical}, whether to be verbose (\code{TRUE}) or not (\code{FALSE} - default).
#' @param parallel \code{logical}, whether to use parallel algorithm (\code{TRUE}) or not (\code{FALSE} - default).
#' @param output optional, see Value section below.
#' @param castor.options optional, a named list of options to be passed to function called by \code{castor::asr_mk_model}.
#' @param estimation.details optional, whether to also return the details for each estimation as returned by \code{castor::asr_mk_model}. This argument can be left \code{NULL} (default) or be any combination of the elements returned by \code{castor::asr_mk_model} (e.g. \code{c("loglikelihood", "transition_matrix")}).
#' 
#' @details
#' 
#' Depending on the type of characters \code{models} argument can be either:
#' \itemize{
#'      \item the name of a single model to apply to all characters (if all characters are discrete or all are continuous); see below for the list of available names. For example \code{models = "ER"} applies the Equal Rates model to all characters (assuming they are all discrete characters).
#'      \item a vector of model names to apply to different type of characters (see below for the list). For example \code{models = c("ER", "ER", "BM")} applies the Equal Rates model to the two first characters (discrete) and the \code{"BM"} model to the third character (continuous).
#'      \item a transition \code{"matrix"} to be applied to all characters (if discrete). For example \code{models = matrix(0.2, 2, 2)}.
#'      \item an single named list of arguments to be applied to all characters by passing it to \code{ape::ace} (if continuous). For example \code{models = list(method = "GLS", corStruct = corBrownian(1, my_tree))}.
#'      \item an un-ambiguous list of arguments to be passed to either \code{castor::asr_mk_model} (discrete characters) or \code{ape::ace} (continuous characters). For example \code{models = list("char1" = list(transition_matrix = matrix(0.2, 2, 2)), "char2" = list(method = "GLS", corStruct = corBrownian(1, my_tree)))} to be specifically passed to the characters named "char1" and "char2"
#'}
#'
#' The available built-in models for discrete characters in \code{castor::asr_mk_model} are:
#' \itemize{
#'  \item \code{"ER"} for all equal rates
#'  \item \code{"SYM"} for symmetric rates
#'  \item \code{"ARD"} all rates are different
#'  \item \code{"SUEDE"} equal stepwise transitions (e.g. for meristic/counting characters)
#'  \item \code{"SRD"} different stepwise transitions 
#' }
#' See directly \code{castor::asr_mk_model} for more models.
#'
#' The available built-in models and methods for continuous characters in \code{ape::ace} are:
#' \itemize{
#'  \item \code{"BM"} model: for a default Brownian Motion with the "REML" method
#'  \item \code{"REML"} method: for a default Brownian Motion with the "REML" method (same as above)    
#'  \item \code{"ML"} method: for a default Brownian Motion with the "ML" method
#'  \item \code{"pic"} method: for a default Brownian Motion with the "pic" (least squared) method    
#'}
#' 
#' The \code{threshold} option allows to convert ancestral states likelihoods into discrete states. When \code{threshold = FALSE}, the ancestral state estimated is the one with the highest likelihood (or at random if likelihoods are equal). When \code{threshold = TRUE}, the ancestral state estimated are all the ones that are have a scaled likelihood greater than the maximum observed scaled likelihood minus the inverse number of possible states (i.e. \code{select_state >= (max(likelihood) - 1/n_states)}). This option makes the threshold selection depend on the number of states (i.e. if there are more possible states, a lower scaled likelihood for the best state is expected). Finally using a numerical value for the threshold option (e.g. \code{threshold = 0.95}) will simply select only the ancestral states estimates with a scaled likelihood equal or greater than the designated value. This option makes the threshold selection absolute. Regardless, if more than one value is select, the uncertainty token (\code{special.tokens["uncertainty"]}) will be used to separate the states. If no value is selected, the uncertainty token will be use between all observed characters (\code{special.tokens["uncertainty"]}).
#' 
#' \code{special.behaviours} allows to generate a special rule for the \code{special.tokens}. The functions should can take the arguments \code{character, all_states} with \code{character} being the character that contains the special token and \code{all_states} for the character (which is automatically detected by the function). By default, missing data returns and inapplicable returns all states, and polymorphisms and uncertainties return all present states.
#' \itemize{
#'      \item{\code{missing = function(x,y) y}}
#'      \item{\code{inapplicable = function(x,y) y}}
#'      \item{\code{polymorphism = function(x,y) strsplit(x, split = "\\\\&")[[1]]}}
#'      \item{\code{uncertainty = function(x,y) strsplit(x, split = "\\\\/")[[1]]}}
#' }
#'
#' Functions in the list must be named following the special token of concern (e.g. \code{missing}), have only \code{x, y} as inputs and a single output a single value (that gets coerced to \code{integer} automatically). For example, the special behaviour for the special token \code{"?"} can be coded as: \code{special.behaviours = list(missing = function(x, y) return(NA)} to make ignore the character for taxa containing \code{"?"}. 
#' 
#' When using the parallel option (either through using \code{parallel = TRUE} by using the number of available cores minus on or manually setting the number of cores - e.g. \code{parallel = 5}), the \code{castor::asr_mk_model} function will use the designated number of cores (using the option \code{Nthreads = <requested_number_of_cores>}). Additionally, if the input \code{tree} is a \code{"multiPhylo"} object, the trees will be run in parallel for each number of cores, thus decreasing computation time accordingly (e.g. if 3 cores are requested and \code{tree} contains 12 \code{"phylo"} objects, 4 different \code{"phylo"} objects will be run in parallel on the 3 cores making the calculation around 3 times faster).
#' 
#' @return
#' Returns a \code{"matrix"} or \code{"list"} of ancestral states. By default, the function returns the ancestral states in the same format as the input \code{matrix}. This can be changed using the option \code{output = "matrix"} or \code{"list"} to force the class of the output.
#' To output the combined ancestral states and input, you can use \code{"combined"} (using the input format) or \code{"combined.matrix"} or \code{"combined.list"}.
#' \emph{NOTE} that if the input data had multiple character types (continuous and discrete) and that \code{"matrix"} or \code{"combined.matrix"} output is requested, the function returns a \code{"data.frame"}.


#TODO: add dispRity format



# To output the light version to be passed to \code{dispRity} functions (a list of two elements: 1) the input \code{matrix} and 2) a list of ancestral states matrices) you can use \code{output = "dispRity"}.
#' 
#' @examples
#' set.seed(42)
#' ## A simple example:
#' ## A random tree with 10 tips
#' tree <- rcoal(10)
#' ## Setting up the parameters
#' my_rates = c(rgamma, rate = 10, shape = 5)
#' 
#' ## A random Mk matrix (10*50)
#' matrix_simple <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates,
#'                             invariant = FALSE)
#' 
#' ## Run a basic ancestral states estimations
#' ancestral_states <- multi.ace(matrix_simple, tree)
#' ancestral_states[1:5, 1:5]
#' 
#' ## A more complex example
#' ## Create a multiple list of 5 trees
#' multiple_trees <- rmtree(5, 10)
#' 
#' ## Modify the matrix to contain missing and special data
#' matrix_complex <- matrix_simple
#' matrix_complex[sample(1:length(matrix_complex), 50)] <- "-"
#' matrix_complex[sample(1:length(matrix_complex), 50)] <- "0%2"
#' matrix_complex[sample(1:length(matrix_complex), 50)] <- "?"
#' matrix_complex[1:5,1:5]
#' 
#' ## Set a list of extra special tokens
#' my_spec_tokens <- c("weirdtoken" = "%")
#' 
#' ## Set some special behaviours for the "weirdtoken" and for "-" and "?"
#' my_spec_behaviours <- list()
#' ## Inapplicable tokens "-" are ignored
#' my_spec_behaviours$inapplicable <- function(x,y) return(NA)
#' ## Missing tokens "?" are considered as all states
#' my_spec_behaviours$missing      <- function(x,y) return(y)
#' ## Weird tokens are considered as state 0 and 3
#' my_spec_behaviours$weirdtoken   <- function(x,y) return(c(1,2))
#' 
#' ## Create a random branch length modifier to apply to each tree
#' branch_lengths <- rnorm(18)^2
#' 
#' ## Setting a list of model ("ER" for the 25 first characters and then "SYM")
#' my_models <- c(rep("ER", 25), rep("SYM", 25))
#' 
#' ## Run the ancestral states on all the tree with multiple options
#' ancestral_states <- multi.ace(matrix_complex, multiple_trees,
#'                               verbose = TRUE,
#'                               models = my_models,
#'                               threshold = 0.95,
#'                               special.tokens = my_spec_tokens,
#'                               special.behaviours = my_spec_behaviours,
#'                               brlen.multiplier = branch_lengths,
#'                               output = "combined.matrix")
#' 
#' ## The results for the the two first characters for the first tree
#' ancestral_states[[1]][, 1:2]
#' 
#' \dontrun{
#' ## The same example but running in parallel
#' ancestral_states <- multi.ace(matrix_complex, multiple_trees,
#'                               verbose = TRUE,
#'                               models = my_models,
#'                               threshold = 0.95,
#'                               special.tokens = my_spec_tokens,
#'                               special.behaviours = my_spec_behaviours,
#'                               brlen.multiplier = branch_lengths,
#'                               output = "combined.matrix",
#'                               parallel = TRUE)
#' }
#' @seealso
#' \code{castor::asr_mk_model}, \code{char.diff}
# \code{fit.ace.model}, 
#' 
#' @author Thomas Guillerme
#' @export

multi.ace <- function(data, tree, models = "ER", threshold = TRUE, special.tokens, special.behaviours, brlen.multiplier, verbose = FALSE, parallel = FALSE, output, castor.options, estimation.details = NULL) {

    match_call <- match.call()

    ## SANITIZING

    ## matrix
    matrix <- data
    input_class <- check.class(matrix, c("matrix", "list", "data.frame"))
    ## Convert the matrix if not a list
    class_matrix <- class(matrix)
    if(class_matrix[[1]] == "list") {
        matrix <- do.call(rbind, matrix)
    }
    ## Get the characters
    n_characters <- ncol(matrix)

    ## tree
    check.class(tree, c("phylo", "multiPhylo"))
    if(is(tree, "phylo")) {
        tree <- list(tree)
        class(tree) <- "multiPhylo"
    }

    ## Check the tree and data
    cleaned_data <- clean.data(matrix, tree)
    if(!is.na(cleaned_data$dropped_tips) || !is.na(cleaned_data$dropped_rows)) {
        stop(paste0("Some names in the data or the tree(s) are not matching.\nYou can use dispRity::clean.data(", as.expression(match_call$data), ", ", as.expression(match_call$tree), ") to find out more."))
    }

    ## Find the node labels (and eventually add them to the trees)
    node_labels <- lapply(tree, get.node.labels)
    ## Split the trees and the labels
    tree <- lapply(node_labels, `[[`, 1)
    class(tree) <- "multiPhylo"
    node_labels <- lapply(node_labels, `[[`, 2)





    #########
    ##
    ## Handle the other options (threshold, brlen, verbose, parallel, output, estimation.details)
    ##
    #########

    ## threshold
    check.class(threshold, c("logical", "numeric"))
    if(is(threshold, "logical")) {
        if(threshold) {
            ## Use the relative threshold function
            threshold.type <- "relative"
        } else {
            ## Use no threshold (just max)
            threshold.type <- "max" 
        }
    } else {
        ## Use an absolute threshold
        threshold.type <- "absolute"
    }

    
    ## brlen multiplier
    if(!missing(brlen.multiplier)) {
        ## Check class
        brlen.multiplier_class <- check.class(brlen.multiplier, c("numeric", "list", "integer"))

        if(brlen.multiplier_class == "list") {
            ## Check the class and length of each list element
            for(one_tree in 1:length(tree)) {
                check.class(brlen.multiplier[[one_tree]], c("numeric", "integer"), msg = paste0(": brlen.multiplier[[", one_tree, "]] must contain ", Nedge(tree[[one_tree]]), " numeric values (number of edges)."))
                check.length(brlen.multiplier[[one_tree]],  Nedge(tree[[one_tree]]), msg = paste0(": brlen.multiplier[[", one_tree, "]] must contain ", Nedge(tree[[one_tree]]), " numeric values (number of edges)."))
            }
        } else {
            ## check the length
            check.length(brlen.multiplier, Nedge(tree[[1]]), msg = paste0(" must contain ", Nedge(tree[[1]]), " values (number of edges)."))
            ## Replicate it for each tree
            brlen.multiplier <- replicate(length(tree), brlen.multiplier, simplify = FALSE)
        }

        ## Multiply the branch lengths
        multiply.brlen <- function(tree, multiplier) {
            tree$edge.length <- tree$edge.length * multiplier
            return(tree)
        }
        tree <- mapply(multiply.brlen, tree, brlen.multiplier, SIMPLIFY = FALSE)
        class(tree) <- "multiPhylo"
    }

    ## verbose
    check.class(verbose, "logical")

    ## Set up parallel arguments
    check.class(parallel, c("logical", "numeric", "integer"))
    if(is.logical(parallel)) {
        do_parallel <- ifelse(parallel, TRUE, FALSE)
        ## Get the number of cores
        if(do_parallel) {
            cores <- parallel::detectCores() - 1
        } else {
            cores <- 1
        }
    } else {
        check.class(parallel, "numeric")
        check.length(parallel, 1, " must be logical or the number of cores to use.")
        do_parallel <- TRUE
        ## Get the number of cores
        cores <- parallel
    }

    ## output
    if(missing(output)) {
        output <- class(matrix)[1]
    } else {
        check.class(output, "character")
        available_methods <- c("matrix", "list", "combined", "combined.list", "combined.matrix")#, "dispRity")
        check.method(output, available_methods, "output option")
        ## Combined
        if(output == "combined") {
            output <- paste(output, class(matrix)[1], sep = ".")
        }
    }

    ## Check the estimation details
    if(!is.null(estimation.details)) {
        ## The return args from castor::asr_mk_model (1.6.6)
        return_args <- c("success", "Nstates", "transition_matrix", "loglikelihood", "ancestral_likelihoods")
        check.method(estimation.details, return_args, msg = "estimation.details")
    }

    #########
    ## Handle the characters
    #########

    ## Preparing the data
    if(verbose) cat("Preparing the data:.")

    ## Detecting the continuous or discrete characters    
    character_is_continuous <- logical()
    ## Looping to allow dropping the levels from matrix
    for(col in 1:ncol(matrix)) {
        character_is_continuous <- c(character_is_continuous, is.numeric(matrix[, col, drop = TRUE]))
    }
    do_discrete <- do_continuous <- FALSE
    continuous_char_ID <- discrete_char_ID <- numeric()

    ## Split the matrices by character types
    if(any(character_is_continuous)) {
        ## Split the matrix for continuous characters
        matrix_continuous <- matrix[, character_is_continuous]
        n_characters_continuous <- sum(character_is_continuous)
        do_continuous <- TRUE
        continuous_char_ID <- which(character_is_continuous)
    }
    if(any(!character_is_continuous)) {
        ## Split the matrix for discrete characters
        matrix_discrete <- matrix[, !character_is_continuous]
        ## Convert into characters
        matrix_discrete <- apply(matrix_discrete, 2, as.character)
        rownames(matrix_discrete) <- rownames(matrix)
        n_characters_discrete <- sum(!character_is_continuous)
        do_discrete <- TRUE
        discrete_char_ID <- which(!character_is_continuous)
    }

    ## Handle the tokens
    if(do_discrete) {
        ## Special tokens
        if(missing(special.tokens)) {
            special.tokens <- character()
        }
        check.class(special.tokens, "character")
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
            matrix_discrete <- ifelse(is.na(matrix_discrete), "N.A", matrix_discrete)
            special.tokens[is.na(special.tokens)] <- "N.A"
        }

        ## Special behaviours
        if(missing(special.behaviours)) {
            special.behaviours <- list()
        }
        check.class(special.behaviours, "list")
        if(is.null(special.behaviours$missing)) {
            special.behaviours$missing <- function(x,y) return(y)
        }
        if(is.null(special.behaviours$inapplicable)) {
            special.behaviours$inapplicable <- function(x,y) return(y)
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

        ## Translate the characters using the special behaviours
        characters_discrete <- unlist(apply(do.call(cbind, apply(matrix_discrete, 2, convert.bitwise, special.tokens, special.behaviours, bitwise = FALSE)), 2, list), recursive = FALSE)
        if(verbose) cat(".")
        
        ## Get a list of character states
        characters_states <- lapply(characters_discrete, function(char) sort(unique(na.omit(unlist(char)))))
        if(verbose) cat(".")

        ## Find invariant characters
        invariants <- which(lengths(characters_states) < 2)

        ## Handle invariant characters
        if(length(invariants) > 0) {
            has_invariants <- TRUE

            ## Stop if they are only invariant characters
            if(length(invariants) == n_characters_discrete) {
                warning(match_call$data, " contains only invariant discrete characters.")
            }

            ## Remove the characters
            invariant_characters <- characters_discrete[invariants]
            invariant_characters_states <- characters_states[invariants]
            characters_discrete <- characters_discrete[-invariants]
            characters_states <- characters_states[-invariants]

            ## Tell the user
            warning(paste0("The character", ifelse(length(invariants) > 1, "s", "") , " ", paste0(invariants, collapse = ", "), ifelse(length(invariants) > 1, " are", " is"), " invariant (using the current special behaviours for special characters) and", ifelse(length(invariants) > 1, " are", " is"), " simply duplicated for each node."), call. = FALSE)
        } else {
            has_invariants <- FALSE
            invariant_characters_states <- NULL
        }

        if(verbose) cat(".")

        ## Get the character tables
        characters_tables <- mapply(convert.char.table, characters_discrete, characters_states, SIMPLIFY = FALSE)
        if(verbose) cat(".")
    }
    
    ## Handle the continuous characters
    if(do_continous) {
        ## Make the continuous characters as lists
        characters_continuous <- unlist(apply(matrix_continuous, 2, list), recursive = FALSE)
        if(verbose) cat(".")
    }

    #########
    ## Handle the models for each character
    #########

    ## Default (missing models)
    if(missing(models)) {
        if(do_discrete) {
            models_discrete <- replicate(n_characters_discrete, "ER", simplify = FALSE)
        }
        if(do_continous) {
            models_continuous <- replicate(n_characters_continuous, set.continuous.args.ace(), simplify = FALSE)
        }
    } else {
        ## Input models
        models_class <- check.class(models, c("character", "list", "matrix"))
        
        ## Models is a vector of models
        if(models_class == "character") {
            ## Check the different models
            available_models_discrete <- c("ER", "SYM", "ARD", "SUEDE", "SRD")
            available_models_continuous <- c("BM", "REML", "ML", "pic")

            ## Unique model
            if(length(models) == 1) {
                if(do_discrete && !do_continuous) {
                    check.method(models, available_models_discrete, msg = "model applied to all discrete characters")
                    models_discrete <- replicate(n_characters_discrete, models, simplify = FALSE)
                }
                if(!do_discrete && do_continuous) {
                    check.method(models, available_models_continuous, msg = "model applied to all continuous characters")
                    models_continuous <- set.continuous.args.ace.models(models, n = n_characters_continuous)
                }
                if(do_discrete && do_continuous) {
                    stop("Only one model is specified but both discrete and continuous characters are detected.")
                }
            } else {
                ## Vector of models
                if(length(models) != n_characters) {
                    stop(paste0("Incorrect number of models specified: ", length(models), " models for ", n_characters, " characters."))
                } else {
                    check.method(models, c(available_models_discrete, available_models_continuous), msg = "models applied to characters")
                    ## Check models per character types
                    ## Discrete
                    if(sum(models %in% available_models_discrete) != n_characters_discrete) {
                        stop(paste0("Incorrect number of models specified: ", sum(models %in% available_models_discrete), " models for ", n_characters, " discrete characters."))
                    } else {
                        ## Discrete models (valid)
                        models_discrete <- as.list(models[models %in% available_models_discrete])
                    }
                    ## Continuous
                    if(sum(models %in% available_models_continuous) != n_characters_continuous) {
                        stop(paste0("Incorrect number of models specified: ", sum(models %in% available_models_continuous), " models for ", n_characters, " continuous characters."))
                    } else {
                        ## Continuous models (valid)
                        models_continuous <- sapply(models[models %in% available_models_continuous], set.continuous.args.ace.models, n = 1)
                    }
                }
            }

            ##TODO
            ## Check the length and if they match the available models and type of characters

            ## Check the different models
            available_models_discrete <- c("ER", "SYM", "ARD", "SUEDE", "SRD")
            available_models_continuous <- c("BM", "REML", "ML", "pic")


            if(do_discrete) {
                models_discrete <- as.list(models[discrete_char_ID])
            }
            if(do_continous) {
                ## Make the models list using set.continuous.args.ace
                models_continuous <-
            }
        }

        ## Models is a transition matrix (discrete only)
        if(models_class == "matrix") {
            if(do_continous) {
                stop("Transition matrices can be used as models only for discrete characters.")
            } else {
                models_discrete <- replicate(n_characters_discrete, models, simplify = FALSE)
            }
        }

        ## Models is a complicated list
        if(models_class == "list") {
            if(length(models) == 1) {
                if(do_discrete && do_continuous) {
                    stop("Only one model is specified but both discrete and continuous characters are detected.")
                }
                ## Set the models for discrete
                if(do_discrete) {
                    models_discrete <- replicate(n_characters_discrete, models, simplify = FALSE)
                }
                ## Set the models for continuous
                if(do_continous) {
                    models_continuous <- replicate(n_characters_continuous, do.call(set.continuous.args.ace, models), simplify = FALSE)
                }
            }

            ## Models is a list of models
            check.length(models, n_characters, msg = paste0(" list must be the same length as the number of characters (", n_characters, ")."))
            ## Separate the models per type
            if(do_discrete) {
                models_discrete <- models[discrete_char_ID]
            }
            if(do_continous) {
                models_continuous <- models[continuous_char_ID]
                ## Format correctly
                models_continuous <- lapply(models_continuous, function(x) do.call(set.continuous.args.ace, x))
            }
        }

        ## Remove invariant characters
        if(do_discrete && has_invariants) {
            ## Remove the models
            models_discrete <- models_discrete[-invariants]
        }
    }











    #########
    ##
    ## Handle the options
    ##
    #########












    ## castor.options
    warning("TODO: change castor options to just options (parsed to castor or ape)")
                warning("TODO: multi.ace. ape options are: CI, ")


    if(missing(castor.options)) {
        ## No options
        castor.options <- NULL
    } else {
        ## must be list with names
        check.class(castor.options, "list")
        if(is.null(names(castor.options))) {
            stop("castor.options must be a named list of options for castor::asr_mk_model().", call. = FALSE)
        }
    }









 

    ## Setting up discrete characters
    tree_args_list_discrete <- tree_args_list_continous <- NULL

    ## Running discrete characters
    if(do_discrete) {
       

        ## Set up the arguments for one tree
        args_list <- mapply(make.args, characters_tables, characters_states, models,
                            MoreArgs = list(castor.options, cores, estimation.details), SIMPLIFY = FALSE)

        ## Add up the tree arguments
        add.tree <- function(tree, args_list) {
            return(lapply(args_list, function(arg, tree) c(arg, tree = list(tree)), tree))
        }
        tree_args_list <- lapply(tree, add.tree, args_list)
    
        if(verbose) cat("Done.\n")

        if(do_parallel) {
            ## Remove verbose
            if(verbose) {
                cat(paste0("Running the estimation for ", length(tree), " tree", ifelse(length(tree) > 1, "s ", " "), "using ", cores, " core", ifelse(cores == 1, "...", "s...")))
                was_verbose <- TRUE
                verbose <- FALSE
            } else {
                was_verbose <- FALSE
            }

            ## Set up the cluster
            cluster <- parallel::makeCluster(cores)

            ## Get the current environment
            current_env <- environment()
            ## Get the export lists
            export_arguments_list <- c("tree_args_list",
                                       "special.tokens",
                                       "invariants",
                                       "threshold.type",
                                       "threshold",
                                       "verbose",
                                       "characters_states",
                                       "invariant_characters_states")
            export_functions_list <- c("one.tree.ace",
                                       "castor.ace",
                                       "tree.data.update",
                                       "add.state.names",
                                       "translate.likelihood")

            ## Export from this environment
            parallel::clusterExport(cluster, c(export_arguments_list, export_functions_list), envir = current_env)

            ## Call the cluster
            results_out <- parLapply(cl = cluster, tree_args_list, one.tree.ace, special.tokens, invariants, characters_states, threshold.type, threshold, invariant_characters_states, verbose)

            ## Stop the cluster
            parallel::stopCluster(cluster)

            ## Reactivate the verbose
            if(was_verbose) {
                cat("Done.")
                verbose <- TRUE
            }
        } else {
            ## Running the ACE for each tree
            results_out <- lapply(tree_args_list, one.tree.ace, special.tokens, invariants, characters_states, threshold.type, threshold, invariant_characters_states, verbose)
        }
        ## Separating results and details
        details_out_discrete <- lapply(results_out, `[[`, 2)
        results_out_discrete <- lapply(results_out, `[[`, 1)
    }



    ## Running continuous characters
    if(do_continous) {

        models
        tree
        data
        #TODO: set up the code for continuous characters (model and shit)
        warning("TODO: multi.ace")


        ## Set the model list (should be handled above - when selecting the models)
        character_models <- list("char1" = list(method = "REML",
                                        model  = "BM",
                                        scaled = TRUE),
                         "char2" = list(method = "REML",
                                        model  = "BM",
                                        scaled = TRUE),
                         "char3" = list(method = "REML",
                                        model  = "BM",
                                        scaled = TRUE),
                         "char4" = list(method = "REML",
                                        model  = "BM",
                                        scaled = TRUE),
                         "char5" = list(method = "REML",
                                        model  = "BM",
                                        scaled = TRUE))

        ## Set the characters as a list (should be handled above - when splitting discrete characters)
        characters <- apply(data, 2, list)

        ## Create the character arguments
        character_args <- mapply(function(character, ace.args) return(c(x = character, ace.args)), characters, character_models, SIMPLIFY = FALSE)

        ## Create the character and tree arguments
        tree_character_args <- list()
        for(one_tree in 1:length(tree)) {
            tree_character_args[[one_tree]] <- lapply(character_args, function(character, tree) {character$phy <- tree; return(character)}, tree[[one_tree]])
        }

        ## Run all the ace
        raw_estimates <- lapply(tree_character_args, lapply, function(x) do.call(what = ape::ace, args = x))
        ## Remove the ugly call
        raw_estimates <- lapply(raw_estimates, lapply, function(x) {x$call <- "ape::ace"; return(x)})

    }


    ## Standardise the whole procedure to:
    #1- characters are sorted in two categories
    #2- characters in each category get their models sorted
    #3- characters from both categories get their trees attributed
    #4- characters and tree are run with do.call

    estimates_continuous <- estimates_discrete <- NULL
    if(do_continous) {
        ## Run all the ace
        estimates_continuous <- lapply(tree_character_args_continuous, lapply, function(x) do.call(what = ape::ace, args = x))
        ## Remove the ugly call
        estimates_continuous <- lapply(estimates_continuous, lapply, function(x) {x$call <- "ape::ace"; return(x)})
    }
    if(do_discrete) {
        ## Run all the castor
        estimates_discrete <- lapply(tree_character_args_continuous, lapply, function(x) do.call(what = castor::asr_mk_model, args = x))
        ## Remove the ugly call
        estimates_discrete <- lapply(estimates_discrete, lapply, function(x) {x$call <- "ape::ace"; return(x)})
    }













    ## Reorder the results
    #TODO: reorder the results using continuous_char_ID and discrete_char_ID: like
    details_out <- details_out[c(continuous_char_ID, discrete_char_ID)]
    results_out <- results_out[c(continuous_char_ID, discrete_char_ID)]
    warning("TODO: multi.ace")

    ## Output a matrix
    make.matrix <- function(results) {
        return(lapply(results, function(data) do.call(cbind, data)))
    }
    ## Combine the ace matrix with the tips
    add.tips <- function(ace, matrix) {
        return(rbind(matrix, ace))
    }
    ## Output a list from a matrix
    make.list <- function(results) {
        ## Make into a list
        return(unlist(apply(results, 1, list), recursive = FALSE))
    }

    ## Make the basic output matrix
    output_matrix <- make.matrix(results_out)
    
    ## Handle output
    output_return <- switch(output,
        matrix          = output_matrix,
        list            = lapply(output_matrix, make.list),
        combined.matrix = lapply(output_matrix, add.tips, matrix = matrix),
        combined.list   = lapply(lapply(output_matrix, add.tips, matrix = matrix), make.list)#
        #dispRity        = return(list("tips" = matrix, "nodes" = output_matrix))
        )

    #TODO: set up "dispRity" output
    #TODO: make sure node.labels available in output
    warning("DEBUG: multi.ace")

    ## Results out
    if(is.null(estimation.details)) {
        if(length(tree) == 1) {
            return(output_return[[1]])
        } else {
            return(output_return)
        }
    } else {
        if(length(tree) == 1) {
            return(list(estimations = output_return[[1]], details = details_out[[1]]))
        } else {
            return(list(estimations = output_return, details = details_out))
        }        
    }
}



# matrix <- matrix(sample(c("0", "1", "0&1", NA),
#   500, replace = TRUE), nrow = 10, dimnames = 
#   list(paste0("t", 1:10), c()))

# trees <- rmtree(10, 10)

# claddis.wrapper <- function(tree, matrix) {
#     return(estimate_ancestral_states(matrix, tree,
#                              estimate_all_nodes = TRUE))
# }

# # Reformat for use elsewhere in Claddis:
# Claddis_matrix <- build_cladistic_matrix(matrix)



# serial_start <- Sys.time()
# ancestral_states <- multi.ace(data = matrix, tree = trees, special.tokens = c("missing" = NA))
# serial_end <- Sys.time()



# ## The same example but running in parallel
# parallel_start <- Sys.time()
# ancestral_states <- multi.ace(matrix, trees, special.tokens = c("missing" = NA))
# parallel_end <- Sys.time()



# claddis_start <- Sys.time()
# test <- lapply(trees, claddis.wrapper, Claddis_matrix)
# claddis_end <- Sys.time()


# serial_time <- serial_end-serial_start
# parallel_time <- parallel_end-parallel_start
# claddis_time <- claddis_end-claddis_start

