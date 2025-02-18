## Set default arguments for continuous models
set.continuous.args.ace <- function(method, model, scaled, kappa, corStruct) {
    continuous_args <- list(type = "continuous")
    continuous_args$model <- ifelse(missing(model), "BM", model)
    continuous_args$scaled <- ifelse(missing(scaled), TRUE, scaled)
    continuous_args$kappa <- ifelse(missing(kappa), 1, kappa)
    if(missing(corStruct)) {
        continuous_args$corStruct <- NULL
    } else {
        continuous_args$corStruct <- corStruct
    }
    return(continuous_args)
}
## Set default arguments for continuous models with "models" as input (= method or model)
set.continuous.args.ace.models <- function(models, n) {
    if(models == "BM" || models == "REML") {
        ## Set everything default
        return(replicate(n, set.continuous.args.ace(), simplify = FALSE))
    } else {
        return(replicate(n, set.continuous.args.ace(method = models), simplify = FALSE))
    }
}


## Finding or adding node labels
get.node.labels <- function(tree) {
    if(is.null(tree$node.label)) {
        tree <- makeNodeLabel(tree, prefix = "n")
        return(list(tree, tree$node.label))
    } else {
        return(list(tree, tree$node.label))
    }
}

## Checking model classes
check.model.class <- function(one_model, available_models) {
    if(class(one_model)[[1]] == "character") {
        check.method(one_model, available_models, msg = "models")
    } else {
        check.class(c(one_model), c("numeric", "integer"), msg = "models must be numerical matrices")
    }
}

## Convert the characters into probability tables
convert.char.table <- function(character, character_states) {
    convert.one.taxon <- function(taxon, character_states) {
        ## Number of states
        n_states <- length(character_states)
        if(all(is.na(taxon))) {
            ## If all NA, return only NAs
            return(rep(NA, n_states))
        } else {
            ## Make the empty table
            table <- rep(0, n_states)
            ## Fill the table
            if(any(active_states <- character_states %in% taxon)) {
                ## Fill in the character states
                table[active_states] <- 1/length(which(active_states))
                ## Scale to be probabilities
                if((state_sum <- sum(table)) > 1) {
                    table <- table/state_sum
                }
            } else {
                ## Return only NAs (should normally never get ticked)
                return(rep(NA, n_states))
            }
            return(table)
        }
    }
    return(t(sapply(character, convert.one.taxon, character_states)))
}

## Set up the characters arguments for one tree
make.args <- function(character, character_states, model, castor.options = NULL, cores = NULL, estimation.details = NULL) {
    ## Get the list of arguments
    castor_args <- list(tip_states = NULL,
                        Nstates = length(character_states),
                        rate_model = model,
                        tip_priors = character,
                        check_input = FALSE,
                        Ntrials = 1)
    ## Add options
    if(!is.null(cores)) {
        castor_args$Nthreads <- cores
    } else {
        castor_args$Nthreads <- 1
    }
    if(!is.null(estimation.details)) {
        castor_args$details <- estimation.details
    }
    if(!is.null(castor.options)) {
        castor.options <- c(castor_args, castor.options)
    }
    return(castor_args)
}

## Update the tree and data
tree.data.update <- function(castor_args) {
    ## Find if any of the tips have NAs
    if(any(dropped <- apply(castor_args$tip_priors, 1, FUN = function(x) any(is.na(x))))) {
        ## Update the tip_priors
        castor_args$tip_priors <- castor_args$tip_priors[!dropped,]
        ## Get the subtree
        subtree <- castor::get_subtree_with_tips(castor_args$tree, omit_tips = names(which(dropped)))
        ## Get the dropped nodes (missing ones are original order)
        dropped <- list("missing" = castor_args$tree$node.label[-subtree$new2old_node], "order" = castor_args$tree$node.label)
        ## Update the tree
        castor_args$tree <- subtree$subtree
    } else {
        dropped <- NULL
    }
    return(list(castor_args, dropped = dropped))
}

## Estimate the ancestral states
castor.ace <- function(castor_args) {
    verboseplaceholder <- "silent"

    ## Drop the tips with no data if needed
    dropped <- tree.data.update(castor_args)
    castor_args <- dropped[[1]]
    dropped <- dropped[[2]]

    ## Get whether to extract details or not
    extract_details <- castor_args$details
    castor_args$details <- NULL

    ## Run the ace
    estimation <- do.call(castor::asr_mk_model, castor_args)

    ## Return details if needed
    if(!is.null(extract_details)) {
        details <- estimation[extract_details]
    } else {
        details <- NULL
    }

    # stop("DEBUG multi.ace_fun::castor.ace")

    # asr_mk_model( tree = castor_args$tree, 
    #               tip_states = castor_args$tip_states, 
    #               Nstates = castor_args$Nstates, 
    #               tip_priors = castor_args$tip_priors, 
    #               rate_model = castor_args$rate_model, 
    #               Ntrials = castor_args$Ntrials, 
    #               check_input =castor_args$check_input, 
    #               Nthreads = castor_args$Nthreads)


    ## Increase the number of trials if unsuccessful
    while(!estimation$success && castor_args$Ntrials < 100) {
        castor_args$Ntrials <- castor_args$Ntrials + castor_args$Ntrials * 2
        estimation <- do.call(castor::asr_mk_model, castor_args)
    }

    ## Get the likelihoods
    if(!estimation$success) {
        estimation <- matrix(1/castor_args$Nstates, ncol = castor_args$Nstates, nrow = Nnode(castor_args$tree))
        success <- FALSE
    } else {
        estimation <- estimation$ancestral_likelihoods
        success <- TRUE
    }

    ## Add back the missing nodes
    if(!is.null(dropped)) {
        ## Add the missing nodes
        missing_estimations <- matrix(NA, ncol = castor_args$Nstates, nrow = length(dropped$missing),
                                      dimnames = list(dropped$missing))
        rownames(estimation) <- castor_args$tree$node.label
        ## Order them following the normal tip order
        estimation <- rbind(estimation, missing_estimations)[dropped$order,]
    } else {
        ## Add the node labels
        rownames(estimation) <- castor_args$tree$node.label
    }

    return(list(results = estimation, success = success, dropped = dropped, details = details))
}

## Update names and trees a posteriori
add.state.names <- function(estimation, character_states, tree) {
    ## Update the estimation names
    colnames(estimation$results) <- character_states

    if(!is.null(estimation$details$transition_matrix)) {
        ## Update the estimations table
        rownames(estimation$details$transition_matrix) <- colnames(estimation$details$transition_matrix) <- character_states
    }
    if(!is.null(estimation$details$ancestral_likelihoods)) {
        ## Update the estimation names
        colnames(estimation$details$ancestral_likelihoods) <- character_states
    }

    return(estimation)
}

## Translating the likelihood table into a vector of characters
translate.likelihood <- function(character, threshold, select.states, special.tokens) {
    ## Translate the likelihood table
    threshold.fun <- function(taxon, threshold, select.states, special.tokens) {
        return(paste(select.states(taxon, threshold), collapse = sub("\\\\", "", special.tokens["uncertainty"])))
    }
    translated_states <- apply(character, 1, threshold.fun, threshold, select.states, special.tokens)
    ## Replace empty states by uncertainties
    replace.empty.states <- function(x, character, special.tokens) {
        return(ifelse(x == "", paste(colnames(character), collapse = sub("\\\\", "", special.tokens["uncertainty"])), x))
    }
    translated_states <- replace.empty.states(translated_states, character, special.tokens) 
    ## Return translation
    return(translated_states)
}

## Function for running ace on a single tree.
one.tree.ace <- function(args_list, special.tokens, invariants, characters_states, threshold.type, threshold, invariant_characters_states, verbose) {

    if(verbose) body(castor.ace)[[2]] <- substitute(cat("."))
    # if(verbose) cat("Running ancestral states estimations:\n")
    ancestral_estimations <- lapply(args_list, castor.ace)
    ancestral_estimations <- mapply(add.state.names, ancestral_estimations, characters_states, SIMPLIFY = FALSE)

    ## Separating the estimations
    success <- unlist(lapply(ancestral_estimations, function(estimation) return(estimation$success)))
    estimations_details <- lapply(ancestral_estimations, function(estimation) return(estimation$details))
    ancestral_estimations <- lapply(ancestral_estimations, function(estimation) return(estimation$results))
    if(any(!success)) {
        warning(paste0("Impossible to fit the model for the following character(s): ", paste(which(!success), collapse = ", "), ".\nThe ancestral estimated values are set to uncertain (all states equiprobable)."))
    }

    ## Select the threshold type function
    switch(threshold.type,
        relative = {select.states <- function(taxon, threshold) {
                                              if(all(is.na(taxon))) {
                                                return(NA)
                                              } else {
                                                return(names(taxon[taxon >= (max(taxon) - 1/length(taxon))]))
                                              }
                                             }},
        max      = {select.states <- function(taxon, threshold) {
                                              if(all(is.na(taxon))) {
                                                return(NA)
                                              } else {
                                                return(names(taxon[taxon >= (max(taxon))]))
                                              }
                                             }},
        absolute = {select.states <- function(taxon, threshold) {
                                              if(all(is.na(taxon))) {
                                                return(NA)
                                              } else {
                                                return(names(taxon[taxon >= threshold]))
                                              }
                                             }})
    
    ## Estimate the ancestral states
    ancestral_states <- lapply(ancestral_estimations, translate.likelihood, threshold, select.states, special.tokens)

    ## Add invariants characters back in place
    if(length(invariants) > 0) {
        ## Replicate the invariant characters
        invariant_ancestral <- lapply(invariant_characters_states, function(x, n) rep(ifelse(length(x == 0), special.tokens["missing"], x), n), args_list[[1]]$tree$Nnode)

        ## Combine the final dataset
        output <- replicate(length(args_list)+length(invariants), list())
        ## Fill the final dataset
        output[invariants] <- invariant_ancestral
        output[-invariants] <- ancestral_states
        ancestral_states <- output
    }

    ## Replace NAs
    replace.NA <- function(character, characters_states, special.tokens) {
        return(sapply(character, function(x) ifelse(x[[1]] == "NA", paste0(characters_states, collapse = sub("\\\\", "", special.tokens["uncertainty"])), x)))
    }
    ancestral_states[-invariants] <- mapply(replace.NA, ancestral_states[-invariants], characters_states, MoreArgs = list(special.tokens = special.tokens), SIMPLIFY = FALSE)

    ## Sort the details list
    if(!is.null(args_list[[1]]$details)) {
        ## Extract the details into multiple lists
        extracted_details <- names(estimations_details[[1]])
        estimations_details_out <- list()
        for(one_detail in 1:length(extracted_details)) {
            estimations_details_out[[one_detail]] <- lapply(estimations_details, `[[`, one_detail)
        }
        names(estimations_details_out) <- extracted_details
    } else {
        estimations_details_out <- NULL
    }

    # if(verbose) cat(" Done.\n")
    return(list(results = ancestral_states, details = estimations_details_out))
}

## Bind the continuous and discrete characters and reorder them
bind.characters <- function(continuous, discrete, order) {
    bound <- cbind(as.data.frame(continuous), as.data.frame(discrete))
    ## Get the new character IDs
    cont_names <- colnames(bound)[1:ncol(continuous)]
    disc_names <- colnames(bound)[-c(1:ncol(continuous))]
    ## Rename discrete if they have the names in common
    if(any(disc_names %in% cont_names)) {
        disc_names <- paste0("c", disc_names)
        colnames(bound)[-c(1:ncol(continuous))] <- disc_names
    }
    ## Reorder the characters to match the input order
    ordering <- matrix(c(1:ncol(bound), c(order$continuous, order$discrete)), ncol = 2, byrow = FALSE, dimnames = list(c(cont_names, disc_names), c("out", "in")))
    return(bound[, names(sort(ordering[, 2, drop = TRUE]))])
}
## Bind the continuous and discrete details and reorder them
bind.details <- function(continuous, discrete, order) {
    ## Reorder the details out per characters
    if(length(length(discrete[[1]])) > 1) {
        discrete_details <- list()
        for(one_char in 1:length(discrete[[1]])) {
            discrete_details[[one_char]] <- lapply(discrete, `[[`, 1)    
        }
        if(!is.null(discrete[[1]])) {
            names(discrete_details) <- names(discrete[[1]])
        }
        discrete <- discrete_details
    }
    ## Bind the two lists
    return(c(continuous, discrete)[c(order$continuous, order$discrete)])
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
## Make the dispRity object out of the results (only for continuous)





# Samples a single estimate from an ace output
#' @param ace the ace output
#' @param sample.fun a list with two elements: fun the sampling function and param a named list of parameters and how to sample them
#' @param samples the number of samples (default is 1)
sample.ace <- function(ace, sample.fun, samples = 1) {
    ## Generate the parameters list
    make.param <- function(one_node, sample.fun, samples) {
        params <- lapply(sample.fun$param, function(fun, data) return(fun(data)), data = one_node)
        return(list(fun = sample.fun$fun, param = c(n = samples, params)))
    }
    fun_list <- apply(ace$CI95, 1, make.param, sample.fun, samples)

    ## Sample all the values
    return(lapply(fun_list, function(x) do.call(what = x$fun, args = x$param)))
}

