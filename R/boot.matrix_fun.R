## Full bootstrap replacement 
boot.full <- function(elements, rarefaction) {
    return( sample(elements, rarefaction, replace = TRUE) )
}

## Single bootstrap: for each bootstrap, select one row and replace it by a randomly chosen left one (for n rows, only one row can be present two times).
boot.single <- function(elements, rarefaction) {
    ## Rarefy the data
    rarefied_sample <- sample(elements, rarefaction, replace = FALSE)
    ## Select the row to remove
    row_in_out <- sample(1:length(rarefied_sample), 2)
    ## Replace the row
    rarefied_sample[row_in_out[1]] <- rarefied_sample[row_in_out[2]]
    return(rarefied_sample)
}

## Performs bootstrap on one series and all rarefaction levels
replicate.bootstraps <- function(rarefaction, bootstraps, series, boot.type.fun, verbose) {
    if(verbose) message(".", appendLF = FALSE)
    return(replicate(bootstraps, boot.type.fun(series$elements, rarefaction)))
}

## Performs bootstrap on multiple series and all rarefaction levels
bootstrap.wrapper <- function(series, bootstraps, rarefaction, boot.type.fun, verbose) {
    return(lapply(select.rarefaction(series, rarefaction), replicate.bootstraps, bootstraps, series, boot.type.fun, verbose))
}

## Rarefaction levels selection
select.rarefaction <- function(series, rarefaction) {
    return(as.list(unique(c(nrow(series$elements),rarefaction[which(rarefaction <= nrow(series$elements))]))))
}

## Combine bootstrap results to a dispRity object
combine.bootstraps <- function(one_bs_result, one_series) {
    return(c(one_series, one_bs_result))
}