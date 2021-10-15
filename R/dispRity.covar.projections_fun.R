# dispRitize <- function(one_result, data, name, fun, type, cent.tend = median) {
#     ## Copying the data
#     output <- data
#     ## Adding the disparity results
#     output$disparity <- lapply(one_result, function(X) list(elements = X, X))
#     # ## Summarising the results for the "elements" part and adding rownames
#     # for(i in 1:n.subsets(data)) {
#     #     output$disparity[[i]] <- matrix(apply(output$disparity[[i]]$elements, 1, cent.tend), ncol = 1, dimnames = list(c(rownames(data$matrix[[1]])[c(data$subsets[[i]]$elements)])))    
#     # }
#         ## Adding the call
#     output$call$bootstrap <- list(ncol(one_result[[1]]), "covar", NULL)
#     output$call$disparity$metrics$name[[1]] <- name
#     output$call$disparity$metrics$fun[[1]]  <- fun
#     output$call$disparity$metrics$between.groups <- (type == "between")
#     return(output)
# }

# ## Between type
# # dispRity.covar(data, metric = projections.covar, between.groups = list_of_pairs, measure = measure)$disparity

## Projection of elements on an axis
projections.fast <- function(matrix, point1 = 0, point2 = colMeans(matrix), measure = "position", scaled = TRUE) {


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
    if(scaled) {
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
        values[["position"]] <- projections[,1]
    }
    if("distance" %in% measure) {
        values[["distance"]] <- apply(matrix - projections, 1, function(row) sqrt(sum(row^2)))
    }
    if("degree" %in% measure) {
        values[["degree"]] <- angles[,1]
    }
    return(values)
}


