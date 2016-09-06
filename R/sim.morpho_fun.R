#sampling from a distribution
## n is the number of samples
## distribution is the distribution as in c(fun, arg1, arg...)
sample.distribution <- function(n, distribution) {
    fun <- distribution[[1]]
    distribution[[1]] <- n
    return(do.call(fun, distribution))
}

#scaling the results from a distribution sample (to be equal to 1)
## n is the number of samples
## distribution is the distribution as in c(fun, arg1, arg...)
## pass.to.gen.seq.HKY  makes sure the output is correct for gen.seq.HKY.binary
proportional.distribution <- function(n, distribution, ..., pass.to.gen.seq.HKY = FALSE) {
    freq <- distribution(n, ...)
    output <- freq/sum(freq)
    #Disabling 0 frequencies
    if(pass.to.gen.seq.HKY == TRUE) {
        while(length(output) != 4 || sum(output) != 1 || any(output <= 0) || any(output >= 1)) {
            freq <- distribution(n, ...)
            output <- freq/sum(freq)
        }
    }
    
    return(output)
}

#seqgen HKY binary
gen.seq.HKY.binary <- function(tree, substitution, rates, states = 1, ...) {

    #States is fixed to 2 (O and 1)
    states <- 1
    
    #The character generator function
    HKY.seq.generator <- function(tree, substitution, rate, ...) {
        return(phyclust::gen.seq.HKY(tree, pi = proportional.distribution(4, runif, pass.to.gen.seq.HKY = TRUE), kappa = sample.distribution(1, substitution), L = 1, rate.scale = sample.distribution(1, rates), ...))
    }

    #The character selector (isolating the characters) function
    character.selector <- function(generated_character) {
        return(rapply(unlist(apply(matrix(as.matrix(generated_character)[-1,]), 1, strsplit, split = " "), recursive = FALSE), function(x) tail(x, 1)))
    }

    #Generating the matrix (with a different parameter for each character)
    character <- character.selector(HKY.seq.generator(tree, substitution, rate, ...))

    #Transforming the base pairs
    character <- gsub("A", "0", character)
    character <- gsub("G", "0", character)
    character <- gsub("C", "1", character)
    character <- gsub("T", "1", character)

    return(character)
}

#sampling the number of characters states
k.sampler <- function(states) {
    if(length(states) == 1) {
        #Only binary characters
        return(2)
    } else {
        return(sample(2:(length(states)+1), 1, prob = states))
    }
}

#Geneterating a character using the mk model
rTraitDisc.mk <- function(tree, substitution, rates, states, ...) {
    #Use the rTraitDisc function with ER model
    return(as.character(rTraitDisc(tree, k = k.sampler(states), rate = sample.distribution(1, rates), model = "ER", states = seq(from=0, to=(length(states))), ...) ))
}

#Invariant characters detector
is.invariant <- function(character) {
    if(length(unique(character)) == 1) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
