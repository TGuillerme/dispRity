#' @title Ancestral states estimations with multiple trees
#'
#' @description Fast ancestral states estimations run on multiple trees using the Mk model from castor::asr_mk_model.
#'
#' @param data A \code{matrix}, \code{data.frame} or \code{list} with the characters for each taxa.
#' @param tree A \code{phylo} or \code{mutiPhylo} object (if the \code{tree} argument contains node labels, they will be used to name the output).
#' @param models A \code{character} vector, unambiguous named \code{list} or \code{matrix} to be passed as model arguments to \code{castor::asr_mk_model} or \code{ape::ace} (see details).
#' @param sample An \code{integer} for the number of matrices to sample per tree (default is \code{1}). See details.
#' @param sample.fun If \code{sample > 1}, the sampling distribution for continuous characters (default is \code{runif}) See details.
#' @param threshold either \code{logical} for applying a relative threshold (\code{TRUE} - default) or no threshold (\code{FALSE}) or a \code{numeric} value of the threshold (e.g. 0.95). See details.
#' @param special.tokens optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that \code{NA} values are not compared and that the symbol "@" is reserved and cannot be used.
#' @param special.behaviours optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.
#' @param brlen.multiplier optional, a vector of branch length modifiers (e.g. to convert time branch length in changes branch length) or a list of vectors (the same length as \code{tree}).
#' @param verbose \code{logical}, whether to be verbose (\code{TRUE}) or not (\code{FALSE} - default).
#' @param parallel Either a \code{logical}, whether to use parallel algorithm (\code{TRUE}) or not (\code{FALSE} - default); or directly an \code{integer} indicating the number of cores to use (note that if \code{parallel = 1}, one core will be used but the parallel integration will still be called).
#' @param output optional, see \code{Value} section below.
#' @param options.args optional, a named list of options to be passed to function called by \code{castor::asr_mk_model}.
#' @param estimation.details optional, whether to also return the details for each estimation as returned by \code{castor::asr_mk_model} or \code{ape::ace}. This argument can be left \code{NULL} (default) or be any combination of the elements returned by \code{castor::asr_mk_model} or \code{ape::ace} (e.g. \code{c("loglikelihood", "transition_matrix", "CI95")}).
#' 
#' @details
#' 
#' Depending on the type of characters \code{models} argument can be either:
#' \itemize{
#'      \item the name of a single model to apply to all characters (if all characters are discrete or all are continuous); see below for the list of available names. For example \code{models = "ER"} applies the Equal Rates model to all characters (assuming they are all discrete characters).
#'      \item a vector of model names to apply to different type of characters (see below for the list). For example \code{models = c("ER", "ER", "BM")} applies the Equal Rates model to the two first characters (discrete) and the \code{"BM"} model to the third character (continuous).
#'      \item a transition \code{"matrix"} to be applied to all characters (if discrete). For example \code{models = matrix(0.2, 2, 2)}.
#'      \item an single named list of arguments to be applied to all characters by passing it to \code{ape::ace} (if continuous). For example \code{models = list(method = "GLS", corStruct = corBrownian(1, my_tree))}.
#'      \item an un-ambiguous list of arguments to be passed to either \code{castor::asr_mk_model} (discrete characters) or \code{ape::ace} (continuous characters). For example \code{models = list("char1" = list(transition_matrix = matrix(0.2, 2, 2)), "char2" = list(method = "GLS", corStruct = corBrownian(1, my_tree)))} to be specifically passed to the characters named "char1" and "char2".
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
#' When using the default \code{sample = 1}, only one estimation is sampled per tree:
#' \itemize{
#'      \item For continuous characters, this estimation is the average estimated ancestral value;
#'      \item For discrete characters, this estimation is the one calculated using the \code{threshold} option (see details below). 
#' }
#' When using \code{sample > 1}, multiple estimations are sampled per tree:
#' \itemize{
#'      \item For continuous characters, this estimation is sample from the 95% confidence interval using the sampling probability function provided by \code{sample.fun}. By default (\code{runif}), the function samples from a uniform bounded by the 95% confidence interval estimation (see below for modifications).;
#'      \item For discrete characters, the estimations are sampled using their scaled likelihood.
#' }
#'
#' The \code{threshold} option allows to convert ancestral states likelihoods into discrete states. When \code{threshold = FALSE}, the ancestral state estimated is the one with the highest likelihood (or at random if likelihoods are equal). When \code{threshold = TRUE}, the ancestral state estimated are all the ones that are have a scaled likelihood greater than the maximum observed scaled likelihood minus the inverse number of possible states (i.e. \code{select_state >= (max(likelihood) - 1/n_states)}). This option makes the threshold selection depend on the number of states (i.e. if there are more possible states, a lower scaled likelihood for the best state is expected). Finally using a numerical value for the threshold option (e.g. \code{threshold = 0.95}) will simply select only the ancestral states estimates with a scaled likelihood equal or greater than the designated value. This option makes the threshold selection absolute. Regardless, if more than one value is select, the uncertainty token (\code{special.tokens["uncertainty"]}) will be used to separate the states. If no value is selected, the uncertainty token will be use between all observed characters (\code{special.tokens["uncertainty"]}).
#'
#' The \code{sample.fun} option allows to specify a function and parameters for the sampling of the continuous traits. The default is \code{sample.fun = list(fun = runif, param = list(min = min, max = max))} for applying a random uniform sampling (\code{runif}) with the parameters (the minimum and the maximum are applied using respectively the \code{min} and \code{max} functions on the estimated data). For applying different samplings to different traits, you can use a list of arguments in the sample format as \code{sample.fun} (e.g. \code{sample.fun = list(trait_uniform = list(fun = runif, param = list(min = min, max = max)), trait_normal = list(fun = rnorm, param = list(mean = mean, sd = function(x)return(diff(range(x))/4)))} - here the standard deviation is calculated as a quarter of the 95% CI range).
#' 
#' @return
#' Returns a \code{"matrix"} or \code{"list"} of ancestral states. By default, the function returns the ancestral states in the same format as the input \code{matrix}. This can be changed using the option \code{output = "matrix"} or \code{"list"} to force the class of the output.
#' To output the combined ancestral states and input, you can use \code{"combined"} (using the input format) or \code{"combined.matrix"} or \code{"combined.list"}.
#' If using continuous characters only, you can use the output option \code{"dispRity"} to directly output a usable \code{dispRity} object with all trees and all the data (estimated and input).
#' \emph{NOTE} that if the input data had multiple character types (continuous and discrete) and that \code{"matrix"} or \code{"combined.matrix"} output is requested, the function returns a \code{"data.frame"}.
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

multi.ace <- function(data, tree, models, sample = 1, sample.fun = list(fun = runif, param = list(min = min, max = max)), threshold = TRUE, special.tokens, special.behaviours, brlen.multiplier, verbose = FALSE, parallel = FALSE, output, options.args, estimation.details = NULL) {

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
        stop(paste0("Some names in the data or the tree(s) are not matching.\nYou can use dispRity::clean.data(", as.expression(match_call$data), ", ", as.expression(match_call$tree), ") to find out more."), call. = FALSE)
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

    ## Sampling
    check.class(sample, c("integer", "numeric"))
    do_sample <- sample > 1
    if(do_sample) {
        ## Override the threshold arguments (no threshold used)
        threshold.type <- "sample"
        threshold <- sample
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

        ## Check the sampling (if required)
        if(do_sample) {
            sample.fun_class <- check.class(sample.fun, "list")
            ## TODO: check names and types of arguments
            if(names(sample.fun)[1] == "fun") {
                sample_funs <- replicate(length(continuous_char_ID), sample.fun, simplify = FALSE)
                
            }
            ## TODO if it's a list of sample.fun, check length and arguments.
        }
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
    ## Correct input class if all continuous
    if(do_continuous && !do_discrete && input_class == "data.frame") {
        matrix <- as.matrix(matrix)
        input_class <- "matrix"
    }

    ## output
    if(missing(output)) {
        output <- class(matrix)[1]
    } else {
        check.class(output, "character")
        available_methods <- c("matrix", "list", "combined", "combined.list", "combined.matrix", "dispRity")
        check.method(output, available_methods, "output option")
        ## Combined
        if(output == "combined") {
            output <- paste(output, class(matrix)[1], sep = ".")
        }
        ## Check dispRity
        if(output == "dispRity" && do_discrete) {
            stop("Only ancestral state estimations for continuous characters can be converted into a dispRity object.\nSelect an other output method.", call. = FALSE)
        }
    }
    ## Set data.frame output to matrix
    if(output == "data.frame") {
        output <- "matrix"
    }

    ## Handle the tokens
    # special.tokens <- character(); special.behaviours <- list() ; warning("DEBUG: multi.ace")
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
            stop("special.tokens cannot contain the character '@' since it is reserved for the dispRity::char.diff function.", call. = FALSE)
        }

        ## Checking whether the special.tokens are unique
        if(length(unique(special.tokens)) != length(special.tokens)) {
            stop("special.tokens cannot contain duplicated tokens.", call. = FALSE)
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
            invariants_ID <- discrete_char_ID[invariants]
            has_invariants <- TRUE

            ## Stop if they are only invariant characters
            if(do_continuous) {
                if(length(invariants) == n_characters_discrete) {
                    warning(match_call$data, " contains only invariant discrete characters.")
                }
            } else {
                if(length(invariants) == n_characters_discrete) {
                    stop.call(call = match_call$data, " contains only invariant characters.")
                }
            }

            ## Remove the characters
            invariant_characters <- characters_discrete[invariants]
            invariant_characters_states <- characters_states[invariants]
            characters_discrete <- characters_discrete[-invariants]
            characters_states <- characters_states[-invariants]

            ## Tell the user
            invar_IDs <- paste0(invariants_ID, collapse = ", ")
            warning(paste0("The character", ifelse(length(invariants) > 1, "s", "") , " ", invar_IDs, ifelse(length(invariants) > 1, " are", " is"), " invariant (using the current special behaviours for special characters) and", ifelse(length(invariants) > 1, " are", " is"), " simply duplicated for each node."), call. = FALSE)
        } else {
            invariants_ID <- integer()
            has_invariants <- FALSE
            invariant_characters_states <- NULL
        }

        if(verbose) cat(".")

        ## Get the character tables
        characters_tables <- mapply(convert.char.table, characters_discrete, characters_states, SIMPLIFY = FALSE)
        if(verbose) cat(".")
    }
    
    ## Handle the continuous characters
    if(do_continuous) {
        ## Make the continuous characters as lists
        characters_continuous <- apply(matrix_continuous, 2, list)
        if(verbose) cat(".")
    }
    if(verbose) cat("Done.\n")

    #########
    ## Handle the models for each character
    #########

    ## Default (missing models)
    if(missing(models)) {
        if(do_discrete) {
            models_discrete <- replicate(n_characters_discrete, "ER", simplify = FALSE)
        }
        if(do_continuous) {
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
                    stop("Only one model is specified but both discrete and continuous characters are detected.", call. = FALSE)
                }
            } else {
                ## Vector of models
                if(length(models) != n_characters) {
                    stop(paste0("Incorrect number of models specified: ", length(models), " models for ", n_characters, " characters."), call. = FALSE)
                } else {
                    check.method(models, c(available_models_discrete, available_models_continuous), msg = "models applied to characters")
                    ## Check models per character types
                    ## Discrete
                    if(do_discrete) {
                        if(sum(models %in% available_models_discrete) != n_characters_discrete) {
                            stop(paste0("Incorrect number of models specified: ", sum(models %in% available_models_discrete), " models for ", n_characters, " discrete characters."), call. = FALSE)
                        } else {
                            ## Discrete models (valid)
                            models_discrete <- as.list(models[models %in% available_models_discrete])
                        }
                    }
                    ## Continuous
                    if(do_continuous) {
                        if(sum(models %in% available_models_continuous) != n_characters_continuous) {
                            stop(paste0("Incorrect number of models specified: ", sum(models %in% available_models_continuous), " models for ", n_characters, " continuous characters."), call. = FALSE)
                        } else {
                            ## Continuous models (valid)
                            models_continuous <- sapply(models[models %in% available_models_continuous], set.continuous.args.ace.models, n = 1)
                        }
                    }
                }
            }
        }

        ## Models is a transition matrix (discrete only)
        if(models_class == "matrix") {
            if(do_continuous) {
                stop("Transition matrices can be used as models only for discrete characters.", call. = FALSE)
            } else {
                models_discrete <- replicate(n_characters_discrete, models, simplify = FALSE)
            }
        }

        ## Models is a complicated list
        if(models_class == "list") {
            if(length(models) == 1) {
                if(do_discrete && do_continuous) {
                    stop("Only one model is specified but both discrete and continuous characters are detected.", call. = FALSE)
                }
                ## Set the models for discrete
                if(do_discrete) {
                    models_discrete <- replicate(n_characters_discrete, models, simplify = FALSE)
                }
                ## Set the models for continuous
                if(do_continuous) {
                    models_continuous <- replicate(n_characters_continuous, do.call(set.continuous.args.ace, models), simplify = FALSE)
                }
            }

            ## Models is a list of models
            check.length(models, n_characters, msg = paste0(" list must be the same length as the number of characters (", n_characters, ")."))
            ## Separate the models per type
            if(do_discrete) {
                models_discrete <- models[discrete_char_ID]
            }
            if(do_continuous) {
                models_continuous <- models[continuous_char_ID]
                ## Format correctly
                models_continuous <- lapply(models_continuous, function(x) do.call(set.continuous.args.ace, x))
            }
        }
    }
    if(do_discrete && has_invariants) {
        models_discrete <- models_discrete[-invariants]
    }
    #########
    ##
    ## Handle the options
    ##
    #########
    if(missing(options.args)) {
        ## No options
        options.ace <- options.castor <- options.args <- NULL
    } else {
        ## must be list with names
        check.class(options.args, "list")
        options_error <- "options.args must be an unambiguous named list of options for castor::asr_mk_model() or ape::ace()."
        ## Check the available names
        options_avail <- c(names(formals(castor::asr_mk_model)), names(formals(ape::ace)))
        if(is.null(names(options.args)) || !all(names(options.args) %in% options_avail)) {
            stop(options_error, call. = FALSE)
        }
        ## Sort the options
        options.ace <- options.castor <- NULL
        if(do_continuous) {
            options.ace <- options.args[names(options.args) %in% names(formals(ape::ace))]
            if(length(options.ace) == 0) {
                options.ace <- NULL
            }
        }
        if(do_discrete) {
            options.castor <- options.args[names(options.args) %in% names(formals(castor::asr_mk_model))]
            if(length(options.castor) == 0) {
                options.castor <- NULL
            }
        }
    }

    ## Check the estimation details
    if(!is.null(estimation.details)) {
        ## The return args from castor::asr_mk_model (1.6.6)
        return_args_discrete <- c("success", "Nstates", "transition_matrix", "loglikelihood", "ancestral_likelihoods")
        return_args_continuous <- c("CI95", "sigma2", "loglik")
        if(do_discrete && do_continuous) {
            return_args <- c(return_args_discrete, return_args_continuous)
        } else {
            if(do_discrete) {
                return_args <- return_args_discrete
            }
            if(do_continuous) {
                return_args <- return_args_continuous
            }
        }
        ## Check the requested details
        check.method(estimation.details, return_args, msg = "estimation.details")
    } else {
        return_args_discrete <- return_args_continuous <- NULL
    }

    #########
    ##
    ## set the arguments for calls
    ##
    #########

    ## Setting the continuous characters call
    if(do_continuous) {
        ## Create the character arguments
        character_continuous_args <- mapply(function(character, ace.args, options = NULL) return(c(x = character, ace.args, options)), characters_continuous, models_continuous, MoreArgs = list(options = options.ace), SIMPLIFY = FALSE)

        ## Create the character and tree arguments
        tree_character_continuous_args <- list()
        for(one_tree in 1:length(tree)) {
            tree_character_continuous_args[[one_tree]] <- lapply(character_continuous_args, function(character, tree) {character$phy <- tree; return(character)}, tree[[one_tree]])
        }
        ## Set verbose fun
        if(verbose) {
            fun_continuous <- function(...) {
                cat(".")
                return(ape::ace(...))
            }
        } else {
            fun_continuous <- ape::ace
        }
    }
    ## Setting the discrete characters call
    if(do_discrete) {

        ## Set the details to return (if any)
        if(any(return_args_discrete %in% estimation.details)) {
            details_out <- return_args_discrete[return_args_discrete %in% estimation.details]
        } else {
            details_out <- NULL
        }

        ## Set up the arguments for one tree
        character_discrete_args <- mapply(make.args, characters_tables, characters_states, models_discrete, MoreArgs = list(estimation.details = details_out, castor.options = options.castor), SIMPLIFY = FALSE)

        ## Create the character and tree arguments
        tree_character_discrete_args <- list()
        for(one_tree in 1:length(tree)) {
            tree_character_discrete_args[[one_tree]] <- lapply(character_discrete_args, function(character, tree) {character$tree <- tree; return(character)}, tree[[one_tree]])
        }
    }


    #########
    ##
    ## run the calls
    ##
    #########

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

        export_arguments_list <- export_functions_list <- character()

        if(do_discrete) {
            ## Get the export lists
            export_arguments_list <- c("tree_character_discrete_args",
                                       "special.tokens",
                                       "invariants",
                                       "threshold.type",
                                       "threshold",
                                       "verbose",
                                       "characters_states",
                                       "invariant_characters_states",
                                       "do_sample")
            export_functions_list <- c("one.tree.ace",
                                       "castor.ace",
                                       "tree.data.update",
                                       "add.state.names",
                                       "translate.likelihood")
        }
        if(do_continuous) {
            export_arguments_list <- c(export_arguments_list, "tree_character_continuous_args")
            export_functions_list <- c(export_functions_list, "fun_continuous")
        }

        ## Export from this environment
        parallel::clusterExport(cluster, c(export_arguments_list, export_functions_list), envir = current_env)

        ## Call the cluster
        if(do_discrete) {
            discrete_estimates <- parLapply(cl = cluster, tree_character_discrete_args, one.tree.ace, special.tokens, invariants, characters_states, threshold.type, threshold, invariant_characters_states, verbose, do_sample)
        }
        if(do_continuous) {
            continuous_estimates <- parLapply(cl = cluster, tree_character_continuous_args, lapply, function(x) do.call(fun_continuous, x))
            ## Remove the ugly call
            continuous_estimates <- lapply(continuous_estimates, lapply, function(x) {x$call <- "ape::ace"; return(x)})
        }

        ## Stop the cluster
        parallel::stopCluster(cluster)

        ## Reactivate the verbose
        if(was_verbose) {
            cat("Done.")
            verbose <- TRUE
        }
    } else {
        ## Make the functions verbose
        if(verbose) cat("Running ancestral states estimations:")

        ## Run the continuous characters
        if(do_continuous) {
            ## Run all the ace
            continuous_estimates <- lapply(tree_character_continuous_args, lapply, function(x) do.call(fun_continuous, x))
            ## Remove the ugly call
            continuous_estimates <- lapply(continuous_estimates, lapply, function(x) {x$call <- "ape::ace"; return(x)})
        }
        ## Run the discrete characters
        if(do_discrete) {
            ## Run all the ace for discrete
            discrete_estimates <- lapply(tree_character_discrete_args, one.tree.ace, special.tokens, invariants, characters_states, threshold.type, threshold, invariant_characters_states, verbose, do_sample)
        }
        if(verbose) cat("Done.\n")
    }

    #########
    ##
    ## handle the outputs
    ##
    #########

    ## Handle the continuous characters
    if(do_continuous) {
        
        if(!do_sample) {
            ## Get the results in a matrix format
            results_continuous <- lapply(lapply(continuous_estimates, lapply, `[[`, "ace"), function(x) do.call(cbind, x))
        } else {
            ## Sample the results for n_matrices
            sample.ace.per.tree <- function(tree_estimate, sample_funs, sample) {
                ## Sample all characters
                samples_list <- mapply(sample.ace, tree_estimate, sample_funs, MoreArgs = list(samples = sample), SIMPLIFY = FALSE)
                ## Return a list of samples for all characters
                return(lapply(as.list(1:sample), function(one_sample, character) do.call(cbind, lapply(character, function(x, one_sample) do.call(rbind, x)[, one_sample, drop = FALSE], one_sample = one_sample)), character = samples_list))
            }
            results_continuous <- lapply(continuous_estimates, sample.ace.per.tree, sample_funs = sample_funs, sample = sample)
        }
        
        ## Get the details for continuous
        if(any(return_args_continuous %in% estimation.details)) {
            ## Get which details to grep
            details_out <- return_args_continuous[return_args_continuous %in% estimation.details]

            ## Get the details
            details_continuous <- lapply(continuous_estimates, lapply, function(x, details_out) return(x[details_out]), details_out)
        } else {
            details_continuous <- NULL
        }
    }

    if(do_discrete) {
        if(!do_sample) {
            ## Get the results in a matrix format
            results_discrete <- lapply(lapply(discrete_estimates, `[[`, 1), function(x) do.call(cbind, x))
        } else {
            ## Split the results into n matrices
            split.per.tree <- function(tree_estimate, sample) {
                return(lapply(as.list(1:sample), function(one_sample, tree_estimate) t(do.call(rbind, lapply(tree_estimate, function(x, one_sample) return(x[one_sample, , drop = FALSE]), one_sample = one_sample))), tree_estimate = tree_estimate$results))
            }
            results_discrete <- lapply(discrete_estimates, split.per.tree, sample = sample)
        }

        ## Get the details
        details_discrete <- lapply(discrete_estimates, `[[`, 2)
    }

    ## Handle output
    ## Combine the traits
    if(do_discrete && do_continuous) {
        ## Combine the traits
        results_out <- mapply(bind.characters, results_continuous, results_discrete,
            MoreArgs = list(order = list("continuous" = continuous_char_ID, "discrete" = unique(c(discrete_char_ID, invariants_ID))), do_sample = do_sample),
            SIMPLIFY = FALSE)
        ## Return the details per characters
        if(is.null(details_continuous)) {
            ## Make a list of nulls
            details_continuous <- replicate(length(tree), lapply(as.list(1:n_characters_continuous), function(x) return(NULL)), simplify = FALSE)
        }
        if(is.null(details_discrete[[1]])) {
            ## Make a list of nulls
            details_discrete <- replicate(length(tree), lapply(as.list(1:n_characters_discrete), function(x) return(NULL)), simplify = FALSE)
        }
        details_out <- mapply(bind.details, details_continuous, details_discrete,
            MoreArgs = list(order = list("continuous" = continuous_char_ID, "discrete" = unique(c(discrete_char_ID, invariants_ID)))),
            SIMPLIFY = FALSE)
    }
    if(do_discrete && !do_continuous) {
        results_out <- results_discrete
        details_out <- details_discrete
    }
    if(do_continuous && !do_discrete) {
        results_out <- results_continuous
        details_out <- details_continuous
    }
    
    ## Handle output
    if(!do_sample) {
        output_return <- switch(output,
            matrix          = results_out,
            list            = lapply(results_out, make.list),
            combined.matrix = lapply(results_out, add.tips, matrix = matrix),
            combined.list   = lapply(lapply(results_out, add.tips, matrix = matrix), make.list),
            dispRity        = make.dispRity(data = lapply(results_out, add.tips, matrix = matrix), tree = tree)
            )
    } else {
        output_return <- switch(output,
            matrix          = results_out,
            list            = lapply(results_out, lapply, make.list),
            combined.matrix = lapply(results_out, lapply, add.tips, matrix = matrix),
            combined.list   = lapply(lapply(results_out, lapply, add.tips, matrix = matrix), lapply, make.list),
            dispRity        = make.dispRity(data = lapply(results_out, lapply, add.tips, matrix = matrix), tree = tree)
            )
    }

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

