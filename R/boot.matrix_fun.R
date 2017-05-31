## Full bootstrap replacement 
boot.full <- function(elements, rarefaction) {
    return(sample(elements, rarefaction, replace = TRUE))
}

## Single bootstrap: for each bootstrap, select one row and replace it by a 
## randomly chosen remaining row (for n rows, only one row can be present twice).
boot.single <- function(elements, rarefaction) {
    ## Rarefy the data
    rarefied_sample <- sample(elements, rarefaction, replace = FALSE)
    ## Select the row to remove
    row_in_out <- sample(1:length(rarefied_sample), 2)
    ## Replace the row
    rarefied_sample[row_in_out[1]] <- rarefied_sample[row_in_out[2]]
    return(rarefied_sample)
}

## Performs bootstrap on one subsamples and all rarefaction levels
replicate.bootstraps.verbose <- function(rarefaction, bootstraps, subsamples, boot.type.fun, verbose) {
    message(".", appendLF = FALSE)
    return(replicate(bootstraps, boot.type.fun(subsamples$elements, rarefaction)))
}
replicate.bootstraps.silent <- function(rarefaction, bootstraps, subsamples, boot.type.fun) {
    return(replicate(bootstraps, boot.type.fun(subsamples$elements, rarefaction)))
}

## Performs bootstrap on multiple subsamples and all rarefaction levels
bootstrap.wrapper <- function(subsamples, bootstraps, rarefaction, boot.type.fun, verbose) {

    ## Verbose?
    if(verbose == TRUE){
        replicate.bootstraps <- replicate.bootstraps.verbose
    } else {
        replicate.bootstraps <- replicate.bootstraps.silent
    }

    return(lapply(select.rarefaction(subsamples, rarefaction), replicate.bootstraps, bootstraps, subsamples, boot.type.fun))
}

## Rarefaction levels selection
select.rarefaction <- function(subsamples, rarefaction) {
    return(as.list(unique(c(nrow(subsamples$elements), rarefaction[which(rarefaction <= nrow(subsamples$elements))]))))
}

## Combine bootstrap results into a dispRity object
combine.bootstraps <- function(one_bs_result, one_subsamples) {
    return(c(one_subsamples, one_bs_result))
}