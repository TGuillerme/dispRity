#' @title Ancestral states estimations with multiple trees
#'
#' @description Fast ancestral states estimations run on multiple trees.
#'
#' @param matrix A \code{matrix} or \code{list} with the characters for each taxa.
#' @param tree A \code{phylo} or \code{mutiPhylo} object.
#' @param models A \code{vector} of models to be passed to \code{\link[castor]{asr_mk_model}}. If left empty, the assumed model is \code{"ER"}. If only one model is specified, it will be applied to all characters.
#' @param use.poly \code{logical}, whether to treat polymorphism as missing data (\code{FALSE} - default) or not (\code{TRUE}).
#' @param use.uncertain \code{logical}, whether to treat uncertainty as missing data (\code{FALSE} - default) or not (\code{TRUE}).
#' @param use.inapp \code{logical}, whether to treat inapplicable data as missing data (\code{FALSE} - default) or not (\code{TRUE}).
#' @param threshold either \code{logical} for applying a relative threshold (\code{TRUE} - default) or no threshold (\code{FALSE}) or a \code{numeric} value of the threshold (e.g. 0.95).
#' @param verbose \code{logical}, whether to be verbose (\code{TRUE}) or not (\code{FALSE} - default).
#' @param parallel \code{logical}, whether to use parallel algorithm (\code{TRUE}) or not (\code{FALSE} - default).
#' @param special.tokens optional, a named \code{vector} of special tokens. By default \code{special.tokens <- list(missing = "?", inapplicable = "-", polymorphism = "&", uncertainty = "/")}.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

# library(ape)
# library(Claddis)
# library(dispRity)
# source("get.taxa.ages.R")
# source("read.nexus.data.R") ## While waiting for ape 5.4
# source("MakeMorphMatrix.R") ## Updated from Claddis
# source("AncStateEstMatrixFast.R") ## Updated from Claddis

# source("convert.tokens.R")

# morpho_matrix <- read.nexus.data("../Data/Morphology/227t_682c_morphology.nex")
# matrix <- do.call(rbind, morpho_matrix)
# no_overlap <- reduce.matrix(ifelse(matrix == "?" | matrix == "-", NA, 1))
# no_overlap$rows.to.remove
# matrix <- morpho_matrix[-which(names(morpho_matrix) %in% no_overlap$rows.to.remove)]
# load("../Data/Processed/tree_list.Rda")
# tree <- tree_list[[1]]
# use.poly = FALSE
# use.uncertain = FALSE
# use.inapp = FALSE
# models <- "ER"
# parallel = FALSE
# special.tokens <- list()
# threshold = TRUE
# verbose = TRUE


# tree <- list(tree, tree, tree, tree)
# class(tree) <- "multiPhylo"


# test <- multi.ace(matrix, tree, models = "ER", use.poly = TRUE, use.uncertain = TRUE, verbose = TRUE)

# matrix <- morpho_matrix
# tree <- cal3_trees
# use.poly = TRUE
# use.uncertain = TRUE
# use.inapp = FALSE
# models <- "ER"
# parallel = FALSE
# special.tokens <- list()
# threshold = TRUE
# verbose = TRUE


##TODO: allow tree to be a multiPhylo object + a sample element that randomly samples a tree everytime and runs ACE on all trees?

##TODO: add node label names
##TODO: if input is matrix, output is combined matrix (option add = TRUE default)

multi.ace <- function(matrix, tree, models, use.poly = FALSE, use.uncertain = FALSE, use.inapp = FALSE, threshold = TRUE, verbose, parallel = FALSE, special.tokens) {

    ## SANITIZING

    ## Special tokens
    #check.class(special.tokens, "list")
    if(missing(special.tokens)) {
        special.tokens <- list()
    }
    if(is.null(special.tokens$missing)) {
        special.tokens$missing <- "?"
    }
    if(is.null(special.tokens$inapplicable)) {
        special.tokens$inapplicable <- "-"
    }
    if(is.null(special.tokens$polymorphism)) {
        special.tokens$polymorphism <- "&"
    }
    if(is.null(special.tokens$uncertainty)) {
        special.tokens$uncertainty <- "/"
    }

    ## Threshold
    #check.class(threshold, c("logical", "numeric"))
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

    #check.class(tree, c("phylo", "multiPhylo"))
    if(class(tree) == "phylo") {
        tree <- list(tree)
        class(tree) <- "multiPhylo"
    }


    #check.class(matrix, c("matrix", "list"))
    ## Convert the matrix if not a list
    class_matrix <- class(matrix)
    if(class_matrix == "list") {
        matrix <- do.call(rbind, matrix)
    }

    ## Get the characters
    characters <- unlist(apply(matrix, 2, list), recursive = FALSE)

    #check.class(models, c("character"))
    ## Get the models
    if(missing(models)) {
        models <- rep("ER", length(characters))
    } else {
        if(length(models) == 1) {
            models <- rep(models, length(characters))
        } else {
            if(length(models != length(characters))) {
                stop("models should be a single character string or a vector of models for each ", ncol(matrix), " characters.")
            }
        }
    }

    ## Convert the potential missing data
    if(verbose) cat("Preparing the data:")
    ## Convert inapplicables
    if(!use.inapp) {
        characters <- convert.tokens(characters, token = special.tokens$inapplicable,
                                                 replace = special.tokens$missing)
        if(verbose) cat(".")
    }
    ## Convert polymorphisms
    if(!use.poly) {
        characters <- convert.tokens(characters, token = special.tokens$polymorphism,
                                                 replace = special.tokens$missing)
        if(verbose) cat(".")
    }
    ## Convert uncertainties
    if(!use.uncertain) {
        characters <- convert.tokens(characters, token = special.tokens$uncertainty,
                                                 replace = special.tokens$missing)
        if(verbose) cat(".")
    }

    ## Get a list of character states
    characters_states <- lapply(characters, unique)
    ## Remove unknowns and poly/uncertainties
    get.only.states <- function(states, special.tokens) {
        if(length(grep(special.tokens$polymorphism, states)) > 0) {
            states <- unique(unlist(sapply(states, strsplit, split = special.tokens$polymorphism)))
        }
        if(length(grep(special.tokens$uncertainty, states)) > 0) {
            states <- unique(unlist(sapply(states, strsplit, split = special.tokens$uncertainty)))
        }
        ## Remove the missing data
        states <- sort(states[which(states != special.tokens$missing)])
    }
    characters_states <- lapply(characters_states, get.only.states, special.tokens)
    if(verbose) cat(".")

    # Find invariant characters
    invariants <- which(lengths(characters_states) < 2)
    if(length(invariants) > 0) {
        ## Remove the characters
        invariant_characters <- characters[invariants]
        invariant_characters_states <- characters_states[invariants]
        characters <- characters[-invariants]
        characters_states <- characters_states[-invariants]

        ## Remove the models
        models <- models[-invariants]

        ## Tell the user
        warning(paste0("The character", ifelse(length(invariants > 1), "s", "") , " ", paste0(invariants, collapse = ", "), " are invariant (using the current options for uncertain/polymorphic/inapplicable data) and are simply duplicated for each node."), call. = FALSE)
    }
    if(verbose) cat(".")


    ## Set up parallel arguments
    if(is.logical(parallel)) {
      do_parallel <- ifelse(parallel, TRUE, FALSE)
      ## Get the number of cores
      if(do_parallel) {
        cores <- parallel::detectCores() - 1
      }
    } else {
      do_parallel <- TRUE
      ## Get the number of cores
      cores <- parallel
    }
    if(verbose) cat(".")

    ## Convert the characters into probability tables
    convert.char.table <- function(character, character_states, special.tokens) {
        convert.one.taxon <- function(taxon, character_states, special.tokens) {
            if(taxon != "?") {
                table <- rep(0, length(character_states))
                ## Find the number of tokens
                taxon_tokens <- strsplit(taxon, split = paste0("[", special.tokens$polymorphism, special.tokens$uncertainty, "]"))[[1]]
                ## Fill the table
                table[character_states %in% taxon_tokens] <- 1/length(taxon_tokens)
                return(table)
            } else {
                return(rep(1/length(character_states), length(character_states)))
            }
        }
        return(t(unname(sapply(character, convert.one.taxon, character_states, special.tokens))))
    }
    characters_tables <- mapply(convert.char.table, characters, characters_states, MoreArgs = list(special.tokens))
    if(verbose) cat(".")

    ## Set up the characters arguments
    make.args <- function(character, character_states, model) {
        return(list(list(character = character,
                         character_states = character_states,
                         model = model)))
    }
    args_list <- mapply(make.args, characters_tables, characters_states, as.list(models))

    ## Add up the tree arguments
    add.tree <- function(tree, args_list) {
        return(lapply(args_list, function(arg, tree) c(arg, tree = list(tree)), tree))
    }
    tree_args_list <- lapply(tree, add.tree, args_list)

    ## Function for running ace on a single tree.
    one.tree.ace <- function(args_list, special.tokens, invariants, threshold.type, threshold, verbose) {

        ## Estimate the ancestral states
        castor.ace <- function(character, character_states, model, tree) {
            est <- castor::asr_mk_model(tree = tree,
                                        tip_states = NULL,
                                        Nstates = length(character_states),
                                        rate_model = model,
                                        Ntrials = 2,
                                        tip_priors = character)$ancestral_likelihoods
            colnames(est) <- character_states
            verboseplaceholder <- "silent"
            return(est)
        }

        if(verbose) body(castor.ace)[[4]] <- substitute(cat("."))
        if(verbose) cat("Running ancestral states estimations:\n")
        ancestral_estimations <- lapply(args_list, function(args) do.call(castor.ace, args))
        if(verbose) cat(" Done.\n")

        ##TODO: Improve model

        ## Select the threshold type function
        switch(threshold.type,
            relative = {select.states <- function(taxon, threshold) {
                                                  names(taxon[taxon >= (max(taxon) - 1/length(taxon))])
                                                 }},
            max      = {select.states <- function(taxon, threshold) {
                                                  names(taxon[taxon >= (max(taxon))])
                                                 }},
            absolute = {select.states <- function(taxon, threshold) {
                                                  names(taxon[taxon >= threshold])
                                                 }})
        
        ## Translating the likelihood table into a vector of characters
        translate.likelihood <- function(character, threshold, select.states, special.tokens) {
            ## Translate the likelihood table
            threshold.fun <- function(taxon, threshold, select.states, special.tokens) {
                return(paste(select.states(taxon, threshold), collapse = special.tokens$uncertainty))
            }
            translated_states <- apply(character, 1, threshold.fun, threshold, select.states, special.tokens)
            ## Replace empty states by uncertainties
            replace.empty.states <- function(x, character, special.tokens) {
                return(ifelse(x == "", paste(colnames(character), collapse = special.tokens$uncertainty), x))
            }
            translated_states <- replace.empty.states(translated_states, character, special.tokens) 
            ## Return translation
            return(translated_states)
        }

        ## Estimate the ancestral states
        ancestral_states <- lapply(ancestral_estimations, translate.likelihood, threshold, select.states, special.tokens)

        ## Add invariants characters back in place
        if(length(invariants) > 0) {
            ## Replicate the invariant characters
            invariant_ancestral <- lapply(invariant_characters_states, function(x, n) rep(ifelse(length(x == 0), special.tokens$missing, x), n), args_list[[1]]$tree$Nnode)

            ## Combine the final dataset
            output <- replicate(ncol(matrix), list())
            ## Fill the final dataset
            output[invariants] <- invariant_ancestral
            output[-invariants] <- ancestral_states
            ancestral_states <- output
        }

        ## Return the ancestral state estimated matrix
        if(class_matrix == "matrix") {
            return(do.call(cbind, ancestral_states))
        } else {
            return(ancestral_states)
        }
    }

    ## Running the ACE for each tree
    output <- lapply(tree_args_list, one.tree.ace, special.tokens, invariants, threshold.type, threshold, verbose)
    return(output)
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