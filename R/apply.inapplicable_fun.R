## Apply inapplicable from character #NC: I don't understand what you mean here
inap.character <- function(target_character, pattern_character, inapplicable_state = "0") {
    ## Setting state 0 of the pattern character as inapplicable (e.g. 0 is absence of feature)
    inapplicables <- which(pattern_character == inapplicable_state)
    ## Replace the 0s (e.g. absence) in the target character by inapplicable tokens
    target_character[inapplicables] <- "-"
    output <- target_character
    return(output)
}

## Select specific states
state.selector <- function(character) {
    sort(unique(character))
}

# NC: Lines should be <85 characters - I tried to fix but can't completely, also if you use two spaces instead of indents then you save some space
## mapply function for inap.character
mapply.inap.character <- function(target_character, pattern_character, matrix, invariant, ...) {

    ## invariant
    if(invariant == FALSE) {
        ## Loop through all the states and exit asap
        for(state in 1:length(state.selector)) {
            new_character <- inap.character(matrix[,target_character], matrix[,pattern_character], 
                                            inapplicable_state = state.selector(matrix[, pattern_character])[state])
            ## Exit if more than two characters
            if (length(unique(new_character)) > 2) break
        }

        ## If none, go back to original and stay invariant
        if (length(unique(new_character)) == 2) {
            new_character <- inap.character(matrix[, target_character], matrix[, pattern_character], 
                                            inapplicable_state = state.selector(matrix[, pattern_character])[1])
        } 
    } else {
        new_character <- inap.character(matrix[, target_character], matrix[, pattern_character], 
                                        inapplicable_state = state.selector(matrix[, pattern_character])[1])
    }
    return(new_character)
}

## Selects a random clade from a tree
select.clade <- function(tree) {
    return(extract.clade(tree, node = sample(1:Nnode(tree), 1) + Ntip(tree))$tip.label)
}

## Apply inapplicable from clade #NC: I don't understand what you mean here, explain more clearly
inap.clade <- function(target_character, tree) {

    ## Select between a grade and a clade
    if(sample(c("grade", "clade"), 1) == "clade") {
        target_character[match(select.clade(tree), names(target_character))] <- "-"
    } else {
        target_character[-match(select.clade(tree), names(target_character))] <- "-"
    }
    return(target_character)
}

## lapply function for inap.clade # NC: function seems under commented...
lapply.inap.clade <- function(target_character, matrix, tree, invariant, ...) {
    ## invariant
    if(invariant == FALSE) {
        new_character <- inap.clade(matrix[, target_character], tree)
        while(length(unique(new_character)) <= 2) {
            new_character <- inap.clade(matrix[, target_character], tree)
        }
    } else {
        new_character <- inap.clade(matrix[, target_character], tree)
    }
    return(new_character)
}
