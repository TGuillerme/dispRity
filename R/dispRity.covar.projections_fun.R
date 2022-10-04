## Projection of elements on an axis
projections.fast <- function(matrix, point1 = 0, point2 = colMeans(matrix), measure = "position", scale = TRUE, centre = TRUE, abs = TRUE) {

    ## Get the projection
    proj_results <- linear.algebra.projection(matrix, point1, point2, do_angle = any(measure %in% c("degree", "radian", "orthogonality")), scale = scale)
    projections <- proj_results$projections
    if(any(measure %in% c("degree", "radian", "orthogonality"))) {
        angles <- proj_results$angles
    }

    # "position" #distance on
    # "distance" #distance from
    # "angle"    #angle between

    ## Measure the thingies
    values <- list()
    if("position" %in% measure) {
        values[["position"]] <- correct.position(projections[, 1], centre, abs)
    }
    if("distance" %in% measure) {
        values[["distance"]] <- get.distance(proj_results$centred_matrix, projections)
    }
    if("degree" %in% measure) {
        values[["degree"]] <- angles[,1]
    }
    if("orthogonality" %in% measure) {
        values[["orthogonality"]] <- sapply(angles, orthogonise)
    }

    ## DEBUG
    # values_out <<- values
    ## Rearrange the values in the input order
    return(values[measure])
}

# stop("DEBUG")

# values_out <- x_fast <- x_nrom <- NULL
# x_fast <- projections.fast(matrix, point1 = point1, point2 = point2, measure = "position", scale = scale, centre = centre, abs = abs)
# x_norm <- projections(matrix, point1 = point1, point2 = point2, measure = "position", scale = scale, centre = centre, abs = abs)

# all(c(x_fast[[1]]) == x_norm)
# all(unname(values_out[["position"]]) == x_norm)
# x_values <- values_out[["position"]]

# values_out <- y_fast <- y_nrom <- NULL
# y_fast <- projections.fast(matrix, point1 = point1, point2 = point2, measure = "distance", scale = scale, centre = centre, abs = abs)
# y_norm <- projections(matrix, point1 = point1, point2 = point2, measure = "distance", scale = scale, centre = centre, abs = abs)

# all(c(y_fast[[1]]) == y_norm)
# all(unname(values_out[["distance"]]) == y_norm)
# y_values <- values_out[["distance"]]

# plot(x_fast[[1]], y_fast[[1]])
# plot(x_values, y_values)


# values_out <- xy_fast <- NULL
# xy_fast <- projections.fast(matrix, point1 = point1, point2 = point2, measure = c("position", "distance"), scale = scale, centre = centre, abs = abs)
# all(c(xy_fast[[1]]) == x_norm)
# all(c(xy_fast[[2]]) == y_norm)
# plot(xy_fast[[1]], xy_fast[[2]])

# plot(values_out[["position"]], values_out[["distance"]])



## Reorder projections.fast arguments
fun.proj <- function(axis, group, data, measure, dots) {
    return(projections.fast(data$matrix[[1]][group, data$call$dimensions, drop = FALSE], point1 = axis[1,], point2 = axis[2,], measure = measure, centre = dots$centre, scale = dots$scale, abs = dots$abs))
}

## Apply the projection per group and axes
apply.proj <- function(axes, group, measure, data, verbose, dots) {
    if(verbose) message(".", appendLF = FALSE)
    return(lapply(axes, fun.proj, group, data, measure, dots))
}

