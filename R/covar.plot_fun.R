## Internal: adjusting the centre
adjust.centre <- function(centres, dim) {
    if(length(centres) < length(dim)) {
        return(rep(centres, length(dim))[dim])
    } else {
        return(centres[dim])
    }
}

## Internal: recentring the covar matrices (changing their loc)
recentre <- function(one_group, one_centre, dimensions) {
    recentre.loc <- function(covar, centre, dim) {
        covar$loc[dim] <- centre[dim]
        return(covar)
    }
    return(lapply(one_group, recentre.loc, centre = one_centre, dim = dimensions))
}

## Internal: making one ellipse
make.ellipse <- function(one_sample, dimensions, npoints){
    return(ellipse::ellipse(x       = one_sample$VCV[dimensions, dimensions],
                            centre  = one_sample$loc[dimensions],
                            npoints = npoints))
}

## Internal: making a list of ellipses for the level
level.ellipses <- function(level_sample, dimensions, npoints, centre) {

    ## Recentreing the levels
    level_sample <- recentre.levels(level_sample, centre, dimensions)

    ## Get the ellipses for the level
    return(lapply(level_sample, make.ellipse, dimensions, npoints))
}

## Internal: changing the intercept ($loc)
replace.intercept <- function(level_sample, value, dimensions) {
    lapply(level_sample, function(X) {X$loc[dimensions] <- value[dimensions]; return(X)})
}


## Internal: changing the intercept wrapper
recentre.levels <- function(level_sample, centre, dimensions) {
    ## Centre the ellipse
    if(is(centre, "function")) {
        ## Get the central tendency (as a function)
        centre_values <- apply(do.call(rbind, lapply(level_sample, `[[`, "loc")), 2, centre)
        ## Recentre the intercepts
        level_sample <- replace.intercept(level_sample, value = centre_values, dimensions)
    }
    if(is(centre, "numeric") || is(centre, "integer")) {
        if((diff <- length(level_sample[[1]]$loc) - length(centre)) > 0) {
            centre <- c(centre, rep(centre, diff))
        }
        ## Manually recentre the intercepts
        level_sample <- replace.intercept(level_sample, value = centre, dimensions)
    }
    return(level_sample)
}