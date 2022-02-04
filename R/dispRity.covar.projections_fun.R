## Projection of elements on an axis
projections.fast <- function(matrix, point1 = 0, point2 = colMeans(matrix), measure = "position", scale = TRUE, centre = TRUE, abs = TRUE) {

    ## Get the point1 and point2
    if(length(point1) != ncol(matrix)) {
        point1 <- rep(point1, ncol(matrix))[1:ncol(matrix)]
    }
    if(length(point2) != ncol(matrix)) {
        point2 <- rep(point2, ncol(matrix))[1:ncol(matrix)]
    }

    ## Get the base vector
    base_vector <- rbind(point1, point2)

    ## Get all the space (with the two last rows being the base vectors)
    space <- rbind(matrix, base_vector)

    ## Centre the matrix on point1
    if(sum(point1) != 0) {
        ## Centre all the space
        space <- space - rep(point1, rep.int(nrow(space), ncol(space)))
        ## Re-attribute the centred variables
        matrix <- space[1:nrow(matrix), , drop = FALSE]
        base_vector <- space[-c(1:nrow(matrix)), , drop = FALSE]
    }

    ## Scale the space
    if(scale) {
        ## The scaled space
        space <- space/dist(space[-c(1:nrow(matrix)), , drop = FALSE])
    }

    ## Get the base vector axis (x) and the projection vector (former unit vector; y)
    x <- base_vector[2, ]
    y <- c(sqrt(sum(base_vector[2,]^2)), rep(0, (ncol(matrix)-1)))
    ## If the base vector and the unit vector are different...
    if(any(x != y)) {
        ## ...rotate the matrix on the x-axis
        space <- space %*% get.rotation.matrix(x, y)
    }
    
    ## Re-attributing the matrix and the vector
    matrix <- space[1:nrow(matrix), , drop = FALSE]
    base_vector <- space[-c(1:nrow(matrix)), , drop = FALSE]

    ## Project the vectors
    projections <- t(apply(matrix, 1, geometry::dot, y = base_vector[2,], d = 2))
    ## Calculate the angles
    if("degree" %in% measure) {
        angles <- t(t(apply(matrix, 1, vector.angle, base_vector[2,])))
        angles <- ifelse(is.nan(angles), 0, angles)
    }

    # "position" #distance on
    # "distance" #distance from
    # "angle"    #angle between

    ## Measure the thingy
    values <- list()
    if("position" %in% measure) {
        if(centre && abs) {
            values[["position"]] <- abs(projections[,1] - 0.5)/0.5
        }
        if(centre && !abs) {
            values[["position"]] <- (projections[,1] - 0.5)/0.5
        }
        if(!centre && abs) {
            values[["position"]] <- abs(projections[,1])
        }
        if(!centre && !abs) {
            values[["position"]] <- projections[,1]
        }
    }
    if("distance" %in% measure) {
        values[["distance"]] <- apply(matrix - projections, 1, function(row) sqrt(sum(row^2)))
        if(centre) {
            values[["distance"]] <- values[["distance"]]/2
        }
    }
    if("degree" %in% measure) {
        values[["degree"]] <- angles[,1]
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

