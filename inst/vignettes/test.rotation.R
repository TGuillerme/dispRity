plot.base <- function(matrix, base_vector, lim) {

    if(missing(lim)) {
        lim <- ceiling(max(abs(range(matrix))))
        lim <- c(-lim, lim)
    }

    if(ncol(matrix) == 3) {
        plot3d(NULL, xlim = lim, ylim =lim, zlim = lim, xlab = "", ylab = "", zlab = "")
        segments3d(x = lim, y = c(0, 0), z = c(0, 0), col = "grey")
        text3d(x = min(lim)*0.9, y = 0, z = 0, texts = "X", col = "grey")
        segments3d(x = c(0,0), y = lim, z = c(0, 0), col = "grey")
        text3d(x = 0, y = min(lim)*0.9, z = 0, texts = "Y", col = "grey")
        segments3d(x = c(0,0), y = c(0,0), z = lim, col = "grey")
        text3d(x = 0, y = 0, z = min(lim)*0.9, texts = "Z", col = "grey")
        ## Add the original points
        text3d(matrix, texts = rownames(matrix), col = "grey")
        segments3d(base_vector, col = "grey")
    } else {
        par(bty = "n")
        plot(NULL, xlim = lim, ylim = lim, xlab = "x", ylab = "y")
        abline(v = 0, col = "grey", lwd = 0.5)
        abline(h = 0, col = "grey", lwd = 0.5)
        text(matrix[,1:2], labels = rownames(matrix), col = "grey", cex = 1.5)
        for(i in 1:nrow(matrix)) {
            lines(rbind(base_vector[1,], matrix[i,]), col = "grey", lwd = 1, lty = 2)
        }
        lines(base_vector, col = "grey", lwd = 1.5)
    }
}
plot.recentred <- function(matrix, base_vector, col = c("orange", "blue", "darkgreen", "black", "black")) {
    if(ncol(matrix) == 3) {
        text3d(matrix, texts = rownames(matrix), col = "black")
        for(i in 1:nrow(matrix)) {
            segments3d(rbind(c(0,0,0), matrix[i,]), col ="grey")
        }
        segments3d(base_vector, col = "black", lwd = 2)
    } else {
        for(i in 1:nrow(matrix)) {
            lines(rbind(c(0,0), matrix[i,]), col = "grey", lwd = 1,)
        }
        lines(base_vector, col = "grey", lwd = 4)
        text(matrix, labels = rownames(matrix), col = col, cex = 1.5)
    }
}
plot.projections <- function(matrix, projections, rejections, col = c("orange", "blue", "darkgreen", "black", "black")) {
    if(ncol(matrix) == 3) {
        for(i in 1:nrow(matrix)) {
            ## Plot the projections
            segments3d(rbind(c(0,0,0), projections[i,]), col = "blue", lwd = 2)
            ## Plot the rejection
            segments3d(rbind(projections[i, ], rejections[i,]+projections[i, ]), col = "orange", lwd = 2)
        }

    } else {
        ## Sort projections in increasing orders
        order_proj <- projections[-c(nrow(projections)-1, nrow(projections)), ]
        order_proj <- sort(abs(order_proj[, 1]), decreasing = TRUE)
        new_order <- match(names(order_proj), letters[1:length(order_proj)])
        order_col <- col[new_order]
        order_proj <- projections[new_order, ]
        order_rej <- rejections[new_order, ]

        for(i in 1:length(new_order)) {
            ## Plot the projections
            lines(rbind(c(0,0), order_proj[i, ]), col = order_col[i], lwd = 1)
            ## Plot the rejection
            lines(rbind(order_proj[i, ], order_rej[i,] + order_proj[i, ]), col = order_col[i], lty = 3)
        }
    }
}
## Angle between two vectors
vector.angle <- function(v1, v2, degree = TRUE) {
    angle <- acos(geometry::dot(v1, v2, d = 1) / (sqrt(sum(v1^2))*sqrt(sum(v2^2))))
    if(degree) {
        return(angle *180/pi)
    } else {
        angle
    }
}
## Rotate a matrix along one axis (y)
get.rotation.matrix <- function(x, y){
    ## This magic comes from https://stackoverflow.com/questions/42520301/find-rotation-matrix-of-one-vector-to-another-using-r/42542385#42542385
    ## following: https://math.stackexchange.com/questions/598750/finding-the-rotation-matrix-in-n-dimensions
    u <- x/sqrt(sum(x^2))

    v <- y-sum(u*y)*u
    v <- v/sqrt(sum(v^2))

    cost <- sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))
    sint <- sqrt(1-cost^2);

    return(diag(length(x)) - u %*% t(u) - v %*% t(v) + cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v)))
}

## Projection of elements on an axis
projections.debug <- function(matrix, point1 = 0, point2 = colMeans(matrix), measure = "position", scaled = TRUE) {

    ## Get the point1
    if(length(point1) == 1) {
        point1 <- rep(point1, ncol(matrix))
    }

    ## Get the base vector
    base_vector <- rbind(point1, point2)

    ## DEBUG
    plot.base(matrix, base_vector)

    ## Get all the space (with the two last rows being the base vectors)
    space <- rbind(matrix, base_vector)

    ## Centre the matrix on point1
    if(sum(point1) != 0) {
        ## Centre all the space
        space <- space - rep(point1, rep.int(nrow(space), ncol(space)))
        ## Re-attribute the centred variables
        matrix <- space[1:nrow(matrix), ]
        base_vector <- space[-c(1:nrow(matrix)), ]
    }

    ## Scale the space
    if(scaled) {
        ## The scaled space
        space <- space/dist(space[-c(1:nrow(matrix)),])
    }

    ## Rotate the matrix on the x-axis
    space <- space %*% get.rotation.matrix(base_vector[2, ], c(dist(space[-c(1:nrow(matrix)),]), rep(0, (ncol(matrix)-1))))
    
    ## Re-attributing the matrix and the vector
    matrix <- space[1:nrow(matrix),]
    base_vector <- space[-c(1:nrow(matrix)),]

    ## DEBUG
    plot.recentred(matrix, base_vector)

    ## Project the vectors
    projections <- t(apply(matrix, 1, geometry::dot, y = base_vector[2,], d = 2))
    angles <- t(t(apply(matrix, 1, vector.angle, base_vector[2,])))
    angles <- ifelse(is.nan(angles), 0, angles)

    ## DEBUG
    rejections <- matrix - projections
    plot.projections(matrix, projections, rejections) 

    # "position" #distance on
    # "distance" #distance from
    # "angle"    #angle between

    ## Measure the thingy
    values <- switch(measure,
        "position" = { #distance on
            ## Measure the position on the vectors and their orientation
            projections[,1]
        },
        "distance" = { #distance from
            ## Get the rejection distance
            apply(matrix - projections, 1, function(row) sqrt(sum(row^2)))
        },
        "degree"  = {
            c(angles)
        },
        "radian"  = {
            c(angles/180*pi)
        })

    return(values)
}
test.fun <- function(seed, n) {
    set.seed(seed)
    if(n == 3) {
        matrix <- matrix(rnorm(15), 5, 3)
    } else {
        matrix <- matrix(rnorm(10), 5, 2)
    }
    rownames(matrix) <- letters[1:5]
    point1 <- matrix["d",]
    point2 <- matrix["e",]

    ## Test the algorithm visualy
    projections.debug(matrix, point1 = point1, point2 = point2, measure = "position", scaled = TRUE)
}

# library(rgl)
matrix <- point1 <- point2 <- base_vector <- projections <- rejections <- base_angl <- NULL
set.seed(2) #1,4
# for(i in 1:10) {
i = 39 #16, 27, 30, 34
    test.fun(seed = i, n = 2) #2, 4, 7

# }
