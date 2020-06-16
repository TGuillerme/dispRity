#' @title Ancestral states estimations with multiple trees
#'
#' @description Fast ancestral states estimations run on multiple trees using the Mk model from castor::asr_mk_model.
#'
#' @param data A \code{matrix} or \code{list} with the characters for each taxa.
#' @param tree A \code{phylo} or \code{mutiPhylo} object (if the \code{tree} argument contains node labels, they will be used to name the output).
#' @param models A \code{vector} of models to be passed to \code{\link[castor]{asr_mk_model}}. If left empty, the it will use the \code{\link{fit.ace.model}} function to find the best model using the first tree. See details.
#' @param threshold either \code{logical} for applying a relative threshold (\code{TRUE} - default) or no threshold (\code{FALSE}) or a \code{numeric} value of the threshold (e.g. 0.95). See details.
#' @param special.tokens optional, a named \code{vector} of special tokens to be passed to \code{\link[base]{grep}} (make sure to protect the character with \code{"\\\\"}). By default \code{special.tokens <- c(missing = "\\\\?", inapplicable = "\\\\-", polymorphism = "\\\\&", uncertainty = "\\\\/")}. Note that \code{NA} values are not compared and that the symbol "@" is reserved and cannot be used.
#' @param special.behaviours optional, a \code{list} of one or more functions for a special behaviour for \code{special.tokens}. See details.
#' @param brlen.multiplier optional, a vector of branch length modifiers (e.g. to convert time branch length in changes branch length) or a list of vectors (the same length as \code{tree}).
#' @param verbose \code{logical}, whether to be verbose (\code{TRUE}) or not (\code{FALSE} - default).
#' @param parallel \code{logical}, whether to use parallel algorithm (\code{TRUE}) or not (\code{FALSE} - default).
#' @param output optional, see Return section below.
#' @param castor.options optional, a named list of options to be passed to function called by \code{\link[castor]{asr_mk_model}}.
#' 
#' @details
#' 
#' The \code{models} argument can be a single or a list of transition \code{matrix}, a single or a a vector of builtin model(s) (see below) or a list of both matrices and builtin models:
#' The available builtin models in \code{\link[castor]{asr_mk_model}} are:
#' \itemize{
#'  \item \code{"ER"} for all equal rates
#'  \item \code{"SYM"} for symmetric rates
#'  \item \code{"ARD"} all rates are different
#'  \item \code{"SUEDE"} equal stepwise transitions (e.g. for meristic/counting characters)
#'  \item \code{"SRD"} different stepwise transitions 
#' }
#' See directly \code{\link[castor]{asr_mk_model}} for more models.
# TODO: add note about fit.ace.model
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
#' When using the parallel option (either through using \code{parallel = TRUE} by using the number of available cores minus on or manually setting the number of cores - e.g. \code{parallel = 5}), the \code{\link[castor]{asr_mk_model}} function will use the designated number of cores (using the option \code{Nthreads = <requested_number_of_cores>}). Additionally, if the input \code{tree} is a \code{"multiPhylo"} object, the trees will be run in parallel for each number of cores, thus decreasing computation time accordingly (e.g. if 3 cores are requested and \code{tree} contains 12 \code{"phylo"} objects, 4 different \code{"phylo"} objects will be run in parallel on the 3 cores making the calculation around 3 times faster).
#' 
#' @return
#' Returns a \code{"matrix"} or \code{"list"} of ancestral states. By default, the function returns the ancestral states in the same format as the input \code{matrix}. This can be changed using the option \code{output = "matrix"} or \code{"list"} to force the class of the output.
#' To output the combined ancestral states and input, you can use \code{"combined"} (using the input format) or \code{"combined.matrix"} or \code{"combined.list"}.
#' To output the light version to be passed to \code{dispRity} functions (a list of two elements: 1) the input \code{matrix} and 2) a list of ancestral states matrices) you can use \code{output = "dispRity"}.
#' 
#' @examples
#' set.seed(42)
#' ## A simple example:
#' ## A random tree with 15 tips
#' tree <- rcoal(15)
#' ## Setting up the parameters
#' my_rates = c(rgamma, rate = 10, shape = 5)
#' 
#' ## A random Mk matrix (15*50)
#' matrix_simple <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates,
#'                             invariant = FALSE)
#' 
#' ## Run a basic ancestral states estimations
#' ancestral_states <- multi.ace(matrix_simple, tree)
#' ancestral_states[1:5, 1:5]
#' 
#' ## A more complex example
#' ## Create a multiple list of 10 trees
#' multiple_trees <- rmtree(10, 15)
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
#' branch_lengths <- rnorm(28)^2
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
#' \code{fit.ace.model}, \code{\link[castor]{asr_mk_model}}, \code{char.diff}
#' 
#' @author Thomas Guillerme
#' @export

multi.ace <- function(data, tree, models = "ER", threshold = TRUE, special.tokens, special.behaviours, brlen.multiplier, verbose = FALSE, parallel = FALSE, output, castor.options) {

    match_call <- match.call()

    ## SANITIZING

    ## matrix
    matrix <- data
    input_class <- check.class(matrix, c("matrix", "list"))
    ## Convert the matrix if not a list
    class_matrix <- class(matrix)
    if(class_matrix[[1]] == "list") {
        matrix <- do.call(rbind, matrix)
    }
    ## Get the characters
    n_characters <- ncol(matrix)

    ## tree
    check.class(tree, c("phylo", "multiPhylo"))
    if(class(tree) == "phylo") {
        tree <- list(tree)
        class(tree) <- "multiPhylo"
    }
    ## Find the node labels (and eventually add them to the trees)
    node_labels <- lapply(tree, get.node.labels)
    ## Split the trees and the labels
    tree <- lapply(node_labels, `[[`, 1)
    class(tree) <- "multiPhylo"
    node_labels <- lapply(node_labels, `[[`, 2)

    ## The available models for castor
    available_models <- c("ER", "SYM", "ARD", "SUEDE", "SRD")

    ## models
    if(missing(models)) {
        models <- replicate(n_characters, "ER", simplify = FALSE)
    } else {
        ## What is the model list
        model_class <- check.class(models, c("character", "matrix", "list"))

        ## Models is a list of one matrix or one character
        if(model_class == "list") {
            ## Check the class of of the list
            list_class <- unique(unlist(lapply(models, class)))
                if(length(list_class) == 1 || all(list_class %in% c("matrix", "array"))) {
                    model_class <- ifelse(list_class[1] %in% c("character", "matrix"), model_class[1], stop.call(call = "", msg = "models must be a list containing characters of matrices."))
                } else {
                    check.length(models, n_characters, msg = paste0(" should be list of characters or/and matrices of length ", ncol(matrix), "."))

                    ## Check all models
                    silent <- lapply(models, check.model.class, available_models)
                }
        }

        ## Models are character or matrix
        switch(model_class,
            "character" = {
                ## Check the model names
                available_models <- c("ER", "SYM", "ARD", "SUEDE", "SRD")
                silent <- sapply(models, check.method, all_arguments = available_models, msg = "model")
                if(length(models) == 1) {
                    models <- replicate(n_characters, models, simplify = FALSE)
                } else {
                    check.length(models, n_characters, msg = paste0(" should be a single character string or a vector of models for each ", ncol(matrix), " characters."))
                }
            },
            "matrix" = {
                ## Check the class of the matrix content
                check.class(c(models), c("numeric", "integer"), msg = "models must be numerical matrices")
                models <- replicate(n_characters, models, simplify = FALSE)
            }
        )
    }

    ## castor.options
    if(missing(castor.options)) {
        ## No options
        castor.options <- NULL
    } else {
        ## must be list with names
        check.class(castor.options, "list")
        if(is.null(names(castor.options))) {
            stop.call(match_call$castor.options, " must be a named list of options for castor::asr_mk_model().")
        }
    }

    ## threshold
    check.class(threshold, c("logical", "numeric"))
    if(class(threshold) == "logical") {
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

    ## Special tokens
    if(missing(special.tokens)) {
        special.tokens <- character()
    }
    check.class(special.tokens, c("character", "logical"))
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
        matrix <- ifelse(is.na(matrix), "N.A", matrix)
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
    if(is.logical(parallel)) {
        do_parallel <- ifelse(parallel, TRUE, FALSE)
        ## Get the number of cores
        if(do_parallel) {
            cores <- parallel::detectCores() - 1
        } else {
            cores <- 1
        }
    } else {
        do_parallel <- TRUE
        ## Get the number of cores
        cores <- parallel
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
    }

    ## Convert the potential missing data
    if(verbose) cat("Preparing the data:.")

    ## Translate the characters using the special behaviours
    characters <- unlist(apply(do.call(cbind, apply(matrix, 2, convert.bitwise, special.tokens, special.behaviours, bitwise = FALSE)), 2, list), recursive = FALSE)
    if(verbose) cat(".")
    
    ## Get a list of character states
    characters_states <- lapply(characters, function(char) sort(unique(na.omit(unlist(char)))))
    if(verbose) cat(".")

    # Find invariant characters
    invariants <- which(lengths(characters_states) < 2)
    if(length(invariants) > 0) {

        ## Stop if they are only invariant characters
        if(length(invariants) == n_characters) {
            stop.call(match.call$data, " contains only invariant characters.")
        }

        ## Remove the characters
        invariant_characters <- characters[invariants]
        invariant_characters_states <- characters_states[invariants]
        characters <- characters[-invariants]
        characters_states <- characters_states[-invariants]

        ## Remove the models
        models <- models[-invariants]

        ## Tell the user
        warning(paste0("The character", ifelse(length(invariants > 1), "s", "") , " ", paste0(invariants, collapse = ", "), " are invariant (using the current special behaviours for special characters) and are simply duplicated for each node."), call. = FALSE)
    }
    if(verbose) cat(".")

    ## Get the character tables
    characters_tables <- mapply(convert.char.table, characters, characters_states, SIMPLIFY = FALSE)
    if(verbose) cat(".")

    ## Set up the arguments for one tree
    args_list <- mapply(make.args, characters_tables, characters_states, models,
                        MoreArgs = list(castor.options, cores), SIMPLIFY = FALSE)

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
                                   "characters_states")
        export_functions_list <- c("castor.ace",
                                   "update.tree.data",
                                   "add.state.names",
                                   "translate.likelihood")

        ## Export from this environment
        parallel::clusterExport(cluster, c(export_arguments_list, export_functions_list), envir = current_env)

        ## Call the cluster
        results_out <- parLapply(cl = cluster, tree_args_list, one.tree.ace, special.tokens, invariants, threshold.type, threshold, verbose)

        ## Stop the cluster
        parallel::stopCluster(cluster)

        ## Reactivate the verbose
        if(was_verbose) {
            cat("Done.")
            verbose <- TRUE
        }
    } else {
        ## Running the ACE for each tree
        results_out <- lapply(tree_args_list, one.tree.ace, special.tokens, invariants, threshold.type, threshold, verbose)
    }

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

    if(length(tree) == 1) {
        output_matrix <- output_matrix[[1]]
    }

    ## Handle output
    switch(output,
        matrix          = return(output_matrix),
        list            = return(lapply(output_matrix, make.list)),
        combined.matrix = return(lapply(output_matrix, add.tips, matrix)),
        combined.list   = return(lapply(lapply(output_matrix, add.tips, matrix), make.list)),
        dispRity        = return(list("tips" = matrix, "nodes" = output_matrix))
        )
}




# library(phytools)
# library(castor)

# set.seed(1)
# tree <- rcoal(20)
# character_rand <- sample(c(1,2), 20, replace = TRUE)
# character_evol <- as.numeric(as.vector(sim.morpho(tree, 1, states = 1, model = "ER", rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)))+1


# convert.table <- function(token) {
#     if(token == 1) {
#         return(c(1,0))
#     } else {
#         return(c(0,1))
#     }
# }

# character_rand_table <- t(sapply(character_rand, convert.table))
# character_evol_table <- t(sapply(character_evol, convert.table))
# colnames(character_rand_table) <- colnames(character_evol_table) <- c(1,2)
# rownames(character_rand_table) <- rownames(character_evol_table) <- tree$tip.label


# phytools_reroot_rand <- rerootingMethod(tree, character_rand_table, model = "ER")$marginal.anc
# phytools_reroot_evol <- rerootingMethod(tree, character_evol_table, model = "ER")$marginal.anc
# ape_ace_rand <- ace(character_rand, tree, type = "discrete")$lik.anc
# ape_ace_evol <- ace(character_evol, tree, type = "discrete")$lik.anc
# castor_tip_rand <- asr_mk_model(tree, character_rand, rate_model = "ER", Ntrials = 2)$ancestral_likelihoods
# castor_tip_evol <- asr_mk_model(tree, character_evol, rate_model = "ER", Ntrials = 2)$ancestral_likelihoods
# castor_table_rand <- asr_mk_model(tree, tip_states = NULL, tip_priors = character_rand_table, rate_model = "ER", Ntrials = 2)$ancestral_likelihoods
# castor_table_evol <- asr_mk_model(tree, tip_states = NULL, tip_priors = character_evol_table, rate_model = "ER", Ntrials = 2)$ancestral_likelihoods




# test <- castor::asr_mk_model(tree = tree, tip_states = NULL, Nstates = length(character_states), rate_model = model, Ntrials = 2, tip_priors = character)$ancestral_likelihoods