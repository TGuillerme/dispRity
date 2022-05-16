## Get one covar matrix
make.covar <- function(level, VCV, levels, n_traits) {
    matrix(VCV[(1:n_traits^2) + (level-1) * n_traits^2], ncol = n_traits)
}

## Get the estimated solutions
make.sol <- function(level, Sol, levels, n_traits) {
    if(names(levels[level]) == "random") {
        return(rep(0, n_traits))
    } else {
        return(unname(Sol[1:n_traits + (level - (sum(names(levels) == "random") + 1)) * n_traits]))
    }
}

## Sapply wrapper
make.matrix <- function(level, sample_estimates, levels, traits) {
    list(VCV = make.covar(level, sample_estimates$VCV, levels, length(traits)),
         loc =   make.sol(level, sample_estimates$Sol, levels, length(traits)))
}

## Internal function for MCMCglmm.covars
get.sample.covar <- function(one_sample, data, levels, traits) {
    ## Select a specific sample
    sample_estimates <- list(VCV = data$VCV[one_sample, ],
                             Sol = data$Sol[one_sample, ])

    ## Make the matrix
    levels_covar <- sapply(1:length(levels), make.matrix, sample_estimates, levels, traits, simplify = FALSE)
    names(levels_covar) <- levels

    ## Return the covariance matrices and origins
    return(levels_covar)
}

