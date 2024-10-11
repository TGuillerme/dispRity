## Function for selecting the elements for each bootstrap replicate (returns a collapsed matrix)
elements.sampler <- function(elements) {
    ## Sampling function
    sampler <- function(row) {
        if(all(is.na(row))) {
            return(NA)
        } else {
            return(sample(row[1:2], 1, prob = c(row[3], 1-row[3])))
        }
    }

    ## Set of samples to go through
    set_samples <- unname(split(1:ncol(elements), rep(1:(ncol(elements)/3), each = 3)))

    ## Returning the sampled matrix
    return(do.call(cbind, lapply(set_samples, function(x) apply(elements[,x, drop = FALSE] , 1, sampler))))
}

## Null bootstrap replacement
boot.null <- function(elements, rarefaction, all.elements) {
    return(sample(all.elements, rarefaction, replace = TRUE))
}

## Full bootstrap replacement 
boot.full <- function(elements, rarefaction, all.elements) {
    return(sample(na.omit(elements), rarefaction, replace = TRUE))
}

## Proba version
boot.full.proba <- function(elements, rarefaction, all.elements) {
    if(is.na(elements[1,2])) {
        ## Simple sampling
        return(sample(elements[,1], rarefaction, prob = elements[,3], replace = TRUE))
    } else {
        ## Dual sampling
        return(sample(na.omit(elements.sampler(elements)), rarefaction, replace = TRUE))
    }
}

## Single bootstrap: for each bootstrap, select one row and replace it by a 
## randomly chosen remaining row (for n rows, only one row can be present twice).
boot.single <- function(elements, rarefaction, all.elements, ...) {
    ## Rarefy the data
    rarefied_sample <- sample(elements, rarefaction, replace = FALSE)
    ## Select the row to remove
    row_in_out <- sample(1:length(rarefied_sample), 2)
    ## Replace the row
    rarefied_sample[row_in_out[1]] <- rarefied_sample[row_in_out[2]]

    return(rarefied_sample)
}

## Proba version
boot.single.proba <- function(elements, rarefaction, all.elements) {
    if(is.na(elements[1,2])) {
        ## Simple sampling (rarefy)
        rarefied_sample <- sample(elements[,1], rarefaction, replace = FALSE)
        ## Select the row to remove
        row_in_out <- sample(1:length(rarefied_sample), 2, prob = elements[,3])
    } else {
        ## Dual sampling (rarefy)
        rarefied_sample <- sample(na.omit(elements.sampler(elements)), rarefaction, replace = FALSE)
        ## Select the row to remove
        row_in_out <- sample(1:length(rarefied_sample), 2)
    }
    ## Replace the row
    rarefied_sample[row_in_out[1]] <- rarefied_sample[row_in_out[2]]
    return(rarefied_sample)
}

## Performs bootstrap on one subsets and all rarefaction levels
replicate.bootstraps <- function(rarefaction, bootstraps, subsets, boot.type.fun, all.elements, boot.by = "rows") {
    verbose_place_holder <- FALSE

    if(boot.by != "columns") {
        sub_elements <- subsets$elements
    } else {
        ## Change to something like that if the bootstrapping is on the rows
        stop("DEBUG: boot.matrix_fun replicate.bootstraps")
        sub_elements <- subsets$dimensions
    }

    if(nrow(sub_elements) == 1) {
        if(length(sub_elements) > 1) {
            ## Bootstrap with element sampler
            return(matrix(replicate(bootstraps, elements.sampler(matrix(sub_elements[1,], nrow = 1))), nrow = 1))
        } else {
            ## Empty subset (or containing a single element)
            return(matrix(rep(sub_elements[[1]], bootstraps), nrow = 1))
        }
    } else {
        ## Normal bootstrap (sample through the elements matrix)
        return(replicate(bootstraps, boot.type.fun(sub_elements, rarefaction, all.elements)))
    }
}

## Performs bootstrap on multiple subsets and all rarefaction levels
bootstrap.wrapper <- function(subsets, bootstraps, rarefaction, boot.type.fun, verbose, all.elements, boot.by = "rows") {
    if(verbose) {
        ## Making the verbose version of disparity.bootstraps
        body(replicate.bootstraps)[[2]] <- substitute(message(".", appendLF = FALSE))
    }
    return(lapply(select.rarefaction(subsets, rarefaction, all.elements, boot.by), replicate.bootstraps, bootstraps, subsets, boot.type.fun, all.elements, boot.by))
}

## Rarefaction levels selection
select.rarefaction <- function(subsets, rarefaction, all.elements, boot.by) {
    if(boot.by != "columns") {
        return(as.list(unique(c(nrow(subsets$elements), rarefaction[which(rarefaction <= nrow(subsets$elements))]))))
    } else {
        return(as.list(unique(c(length(all.elements), rarefaction[which(rarefaction <= length(all.elements))]))))
    }
}

## Combine bootstrap results into a dispRity object
combine.bootstraps <- function(one_bs_result, one_subsets) {
    return(c(one_subsets, one_bs_result))
}

## Split the subsets
do.split.subsets <- function(one_subset, n_trees) {
    ## split the whole dataset
    ncol_out <- ncol(one_subset$elements)/n_trees
    splitted <- lapply(
        split(one_subset$elements, rep(1:n_trees, each = ncol_out * nrow(one_subset$elements))), 
        function(X, ncol) return(list("elements" = matrix(X, ncol = ncol))),
        ncol = ncol_out)
    return(splitted)
}

## Merge the splitted data back into a list
merge.to.list <- function(subset_bs) {
    lapply(as.list(seq_along(subset_bs[[1]])), function(rare, subset_bs) do.call(cbind, lapply(subset_bs, `[[`, rare)), subset_bs)
}