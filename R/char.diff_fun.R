## Translate a character to Felsenstein's xyz notation
translate.xyz <- function(one_character, special.tokens) {
    ## Get the symbols in order of appearance
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

## Binary bit converter for a single character (token)
convert.bitwise <- function(token, special.tokens = NULL, special.behaviours = NULL, all_states = NULL) {
    binary <- function(token) {
        return(round(sum(2^token)))
    }

    ## Convert character as bitwise:
    options(warn = -1)
    converted_token <- as.integer(token)
    options(warn = 0)
    if(!is.na(converted_token)) {
        ## Convert the token into a bitwise
        return(binary(converted_token))
    } else {
        ## Convert the token according to its behaviour
        behaviour <- names(which(sapply(special.tokens, grepl, token)))

        ## Convert to a bitwise convertible token
        return(binary(as.integer(special.behaviours[behaviour][[1]](token, all_states))))
    }
}

## Binary bit converter for a whole character
convert.character <- function(character, special.tokens, special.behaviours) {
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
    all_states <- as.integer(sort(unique(strsplit(paste0(recursive.sub(special.tokens, unique(character)), collapse = ""), split = "")[[1]])))

    ## Convert all characters
    return(sapply(character, convert.bitwise, special.tokens, special.behaviours,all_states))
}






## Functions for getting the the density plot limits
get.max.x <- function(density) return(max(density$x))
get.max.y <- function(density) return(max(density$y))
get.min.x <- function(density) return(min(density$x))
get.min.y <- function(density) return(min(density$y))

## Removing columns with only NAs
select.nas <- function(column) {
    if((length(column) - length(which(is.na(column)))) <= 2) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

plot.char.diff.density <- function(matrix, main, legend, col, xlim, ylim, legend.pos, xlab, ylab) {
    ## Removing columns with NAs
    NA_columns <- which(apply(matrix, 2, select.nas) == TRUE)
    
    if(length(NA_columns) != 0) {
        matrix <- matrix[,-NA_columns]
    }


    ## Measuring the densities
    densities <- apply(matrix, 2, stats::density, na.rm = TRUE)

    ## Getting the plot limits
    if(missing(xlim)) {
        xlim = c(min(unlist(lapply(densities, get.min.x))), max(unlist(lapply(densities, get.max.x))))
    }
    if(missing(ylim)) {
        ylim = c(min(unlist(lapply(densities, get.min.y))), max(unlist(lapply(densities, get.max.y))))
    }

    ## Measuring the cumulated density
    cum_density <- stats::density(as.numeric(matrix), na.rm = TRUE)

    ## Empty plot
    plot(1,1, col = "white", xlim = xlim, ylim = ylim, main = main, xlab = xlab, ylab = ylab, bty = "n")

    ## Adding the densities
    silent <- lapply(densities, lines, col = col[2])

    ## Adding the cumulative density
    lines(cum_density, col = col[1])

    ## Adding the legend
    if(legend[1] != FALSE) {
        legend(legend.pos, legend = legend, lty = 1, col = col)
    }
}
