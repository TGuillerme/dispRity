## Translate a character to Felsenstein's xyz notation (to do in C?)
translate.xyz <- function(one_character, special.tokens) {
    ## Get the symbols in order of appearance
    symbols <- grep("\\d+", unique(unlist(strsplit(unique(one_character), split = paste0("[", paste(special.tokens, collapse = ""),"]")))), value = TRUE)

    ## Replace them with an extra special character
    n_symbols <- length(symbols)
    if(n_symbols < 26) {
        replace <- paste0(LETTERS[1:n_symbols], "@")
    } else {
        replace <- paste0(apply(combn(LETTERS[1:26], 2), 2, FUN = function(x) paste(x, collapse = ""))[1:n_symbols], "@")
    }

    ## Replace the symbols by a replacement (recursively)
    while(n_symbols > 0) {
        one_character <- gsub(symbols[n_symbols], replace[n_symbols], one_character)
        n_symbols <- n_symbols - 1
    }

    ## Re-replace by numbers (recursively too)
    while(n_symbols < length(symbols)) {
        n_symbols <- n_symbols + 1
        one_character <- gsub(replace[n_symbols], n_symbols, one_character)
    }
    return(one_character)
}

## Get all states
recursive.sub <- function(patterns, character) {
    if(length(patterns) == 0) {
        return(character)
    } else {
        character <- gsub(patterns[[1]], "", character)
        patterns <- patterns[-1]
        return(recursive.sub(patterns, character))
    }
}

## Recursive list conversion
convert.list <- function(behaviours, special_token, character_list, all_states) {
    if(length(behaviours) > 0) {
        ## Convert the characters for the first behaviour
        if(any(special_token[[1]])) {
            character_list[special_token[[1]]] <- lapply(character_list[special_token[[1]]], behaviours[[1]], all_states)
        }
        ## Remove the used behaviour
        behaviours[[1]] <- NULL
        special_token[[1]] <- NULL
    } else {
        return(character_list)
    }
    return(convert.list(behaviours, special_token, character_list, all_states))
}

## Getting binary numbers
binary <- function(token) {
    return(sum(2^token))
}

## Binary bit converter for a whole character
convert.bitwise <- function(character, special.tokens, special.behaviours) {
    ## Get all states
    all_states <- as.integer(sort(unique(strsplit(paste0(recursive.sub(special.tokens, unique(character)), collapse = ""), split = "")[[1]])))

    ## Get all the special characters
    special_characters <- lapply(special.tokens, grepl, character)

    ## Convert the list of integers
    character_list <- lapply(convert.list(special.behaviours, special_characters, as.list(character), all_states), as.integer)

    ## Convert into binary
    return(unlist(lapply(character_list, binary)))
}
