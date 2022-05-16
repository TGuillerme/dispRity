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
make.ellipse <- function(one_sample, dimensions, npoints, level){
    return(ellipse::ellipse(x       = one_sample$VCV[dimensions, dimensions],
                            centre  = one_sample$loc[dimensions],
                            npoints = npoints,
                            level   = level))
}

## Internal: making a list of ellipses for the level
level.ellipses <- function(level_sample, dimensions, npoints, centre, level) {

    ## Recentreing the levels
    level_sample <- recentre.levels(level_sample, centre, dimensions)

    ## Get the ellipses for the level
    return(lapply(level_sample, make.ellipse, dimensions, npoints, level))
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

## Get the VCV central tendency
VCV.cent.tend <- function(one_covar, fun) {
    VCVs <- lapply(one_covar, `[[`, "VCV")
    locs <- lapply(one_covar, `[[`, "loc")
    return(list(VCV = apply(array(do.call(cbind, VCVs), dim = c(dim(VCVs[[1]]), length(VCVs))), c(1,2), fun),
                loc = apply(do.call(rbind, locs), 2, fun)))
}

## Scale a VCV matrix to another one
scale.VCV <- function(VCV1, VCV2) {
    ## Dividing both VCVs
    
    ## Getting the off diagonal (the scaling ratio)
    # VCV <- VCV1$VCV/VCV2$VCV
    # ratio <- abs(VCV[(nrow(VCV))^2-(1:nrow(VCV))*(nrow(VCV)-1)])

    ## Getting the scaling ratio based on the major axis length
    ratio <- dist(get.one.axis(VCV1))[1]/dist(get.one.axis(VCV2))[1]

    ## Scaling VCV1
    VCV1$VCV <- VCV1$VCV/ratio^2
    return(VCV1)
}
