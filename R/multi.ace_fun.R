## Finding or adding node labels
get.node.labels <- function(tree) {
    if(is.null(tree$node.label)) {
        tree$node.label <- seq((Ntip(tree)+1):(Ntip(tree)+Nnode(tree)))
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
make.args <- function(character, character_states, model, castor.options, cores) {
    ## Get the list of arguments
    castor_args <- list(tip_states = NULL,
                        Nstates = length(character_states),
                        rate_model = model,
                        tip_priors = character,
                        check_input = FALSE,
                        Ntrials = 1,
                        Nthreads = cores)
    ## Add options
    if(!is.null(castor.options)) {
        castor.options <- c(castor_args, castor.options)
    }
    return(castor_args)
}


## Update the tree and data
update.tree.data <- function(castor_args) {
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
    dropped <- update.tree.data(castor_args)
    castor_args <- dropped[[1]]
    dropped <- dropped[[2]]

    ## Run the ace
    estimation <- do.call(castor::asr_mk_model, castor_args)

    ## Increase the number of trials if unsuccessful
    while(!estimation$success && trials < 100) {
        castor_args$Ntrials <- castor_args$Ntrials + castor_args$Ntrials * 2
        estimation <- do.call(castor::asr_mk_model, castor_args)
    }

    ## Get the likelihoods
    if(!estimation$success) {
        estimation <- matrix(1/castor_args$Nstates, ncol = castor_args$Nstates, nrow = Nnode(tree))
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

    return(list(results = estimation, success = success, dropped = dropped))
}

## Update names and trees a posteriori
add.state.names <- function(estimation, character_states, tree) {
    ## Update the estimation names
    colnames(estimation$results) <- character_states

    if(!is.null(estimation$dropped)) {
        ## Update the estimations table
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
one.tree.ace <- function(args_list, special.tokens, invariants, threshold.type, threshold, verbose) {

    if(verbose) body(castor.ace)[[2]] <- substitute(cat("."))
    if(verbose) cat("Running ancestral states estimations:\n")
    ancestral_estimations <- lapply(args_list, castor.ace)
    ancestral_estimations <- mapply(add.state.names, ancestral_estimations, characters_states, SIMPLIFY = FALSE)
    if(verbose) cat(" Done.\n")

    ## Separating the estimations
    success <- unlist(lapply(ancestral_estimations, function(estimation) return(estimation$success)))
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
        output <- replicate(ncol(matrix), list())
        ## Fill the final dataset
        output[invariants] <- invariant_ancestral
        output[-invariants] <- ancestral_states
        ancestral_states <- output
    }
    return(ancestral_states)
}
