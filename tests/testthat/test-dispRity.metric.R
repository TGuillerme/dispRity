#TESTING boot.matrix

context("dispRity.metric")

test_that("dimension generic", {
    expect_equal(capture_output(dimension.level3.fun()), "No implemented Dimension level 3 functions implemented in dispRity!\nYou can create your own by using: ?make.metric")
    expect_equal(capture_output(dimension.level2.fun()), "Dimension level 2 functions implemented in dispRity:\n?ancestral.dist\n?angles\n?centroids\n?deviations\n?displacements\n?neighbours\n?pairwise.dist\n?ranges\n?radius\n?variances\n?span.tree.length")
    expect_equal(capture_output(dimension.level1.fun()), "Dimension level 1 functions implemented in dispRity:\n?convhull.surface\n?convhull.volume\n?diagonal\n?ellipse.volume\n?func.div\n?func.eve\n?group.dist\n?mode.val\n?n.ball.volume")
})



#Testing the metrics
test_that("k.root", {
    mat <- matrix(abs(rnorm(25)), 5, 5)
    test <- k.root(mat, 5)
    ## Right format
    expect_is(test, "matrix")
    expect_equal(dim(test), dim(mat))
    ## Repeatable
    expect_true(all(test == k.root(mat, 5)))
    ## Number of dimensions matters
    expect_true(all(mat == k.root(mat, 1)))
})

test_that("variances metric", {
    #Create a dummy matrix
    matrix <- replicate(50, rnorm(100))
    #Calculate the variances
    vars1 <- variances(matrix)
    expect_equal(
    	length(vars1), ncol(matrix)
    	)
    #Calculate the variances "manually"
    vars2 <- apply(matrix, 2, var)
    #test
    expect_equal(
    	vars1, vars2
    	)
    #Applying the k.root
    vars3 <- variances(matrix, k.root = TRUE)
    vars4 <- variances(matrix)^(1/ncol(matrix))
    expect_equal(
        vars3, vars4
        )
})

test_that("ranges metric", {
    #Create a dummy matrix
    matrix <- replicate(50, rnorm(100))
    #Calculate the ranges
    ran1 <- ranges(matrix)
    expect_equal(
    	length(ran1), ncol(matrix))
    #Calculate the variances "manually"
    ran2 <- apply(matrix, 2, function(X) abs(min(X)-max(X)))
    #test
    expect_equal(
    	ran1, ran2
    	)
    #Applying the k.root
    ran3 <- ranges(matrix, k.root = TRUE)
    ran4 <- ranges(matrix)^(1/ncol(matrix))
    expect_equal(
        ran3, ran4
        )
})

test_that("centroids metric", {
    #Create a dummy matrix
    matrix <- replicate(50, rnorm(100))
    #Calculate the centroids
    cent1 <- centroids(matrix)
    expect_equal(
    	length(cent1), nrow(matrix)
    	)
    #Calculate the centroids "manually"
    centroid <- apply(matrix, 2, mean)
    cent2 <- NULL
    for (j in 1:nrow(matrix)){
        cent2[j] <- dist(rbind(matrix[j,], centroid), method="euclidean")
    }
    #test
    expect_equal(
    	cent1, cent2
    	)

    #Checking the centroid argument
    #Wrong argument
    # expect_error(
    #     centroids(matrix, centroid = "a")
    #     )
    #Using the real centroid
    expect_equal(
        cent1, centroids(matrix, centroid = centroid)
        )
    expect_equal(
        sum(centroids(matrix, centroid = 0))
        , sum(centroids(matrix, centroid = rep(0, ncol(matrix)) ))
        )
    #Using a 0 origin centroid
    expect_lt(
        sum(centroids(matrix, centroid = rep(0, ncol(matrix)))), sum(centroids(matrix, centroid = rep(1, ncol(matrix))))
        )
    #Using a 0 origin centroid
    expect_lt(
        sum(centroids(matrix, centroid = 0)), sum(centroids(matrix, centroid = 1))
        )
})

test_that("mode.val metric", {
    #Create a dummy vector
    vector <- rnorm(100)
    #Calculate the mode
    mode1 <- mode.val(vector)
    #Calculate the variances "manually"
    mode2 <- as.numeric(names(sort(-table(vector))[1]))
    #test
    expect_equal(
    	mode1, mode2
    	)
})

test_that("ellipse.volume metric", {
    # Calculate the proper volume (using the eigen values)
    volume.true <- function(matrix, eigen.val) {
        #Correct calculation of the volume (using the eigen values)
        #remove the eigen values for the eigen vectors not present in matrix
        eigen.val<-eigen.val[1:ncol(matrix)]
        #dimensionality (where k (or n in Donohue et al 2013) is the size of the covariance matrix but if corrected is the size of the covariance matrix - 2 (n=k-2))
        n<-ncol(matrix)
        #volume
        vol<-pi^(n/2)/gamma((n/2)+1)*prod(eigen.val^(0.5))
        return(vol)
		#For volume through time use the eigenvectors??
    }

    # Dummy data
    dummy_cla <- replicate(50, sample(c(0,1,2), 20, replace = TRUE, prob = c(0.45,0.45,0.1)))
    rownames(dummy_cla) <- LETTERS[1:20]

    # Dummy ordination
    dummy_dis <- as.matrix(dist(dummy_cla, method="euclidean"))
    dummy_ord <- cmdscale(dummy_dis, k=19, eig=TRUE)
    dummy_eig <- dummy_ord$eig
    dummy_ord <- dummy_ord$points
    # Calculate the true volume (with eigen values)
    true_vol <- volume.true(dummy_ord, dummy_eig/(nrow(dummy_dis)-1))
    # Calculate the volume without the eigen values
    test_vol <- ellipse.volume(dummy_ord)
    # test
    expect_equal(
    	true_vol, test_vol
    	)
    # test with the eigen val estimation
    expect_equal(
        true_vol, ellipse.volume(dummy_ord, eigen.value = dummy_eig/(nrow(dummy_dis)-1))
        )

    # Now testing for PCOA
    dummy_ord <- pcoa(dummy_dis)
    dummy_eig <- dummy_ord$values[,1]
    dummy_ord <- dummy_ord$vectors
    # Calculate the true volume (with eigen values)
    true_vol <- volume.true(dummy_ord, dummy_eig/(nrow(dummy_dis)-1))
    # Calculate the volume without the eigen values
    test_vol <- ellipse.volume(dummy_ord)
    # test
    expect_equal(
    	true_vol, test_vol
    	)
    # test with the eigen val estimation
    expect_equal(
        true_vol, ellipse.volume(dummy_ord, eigen.value = dummy_eig/(nrow(dummy_dis)-1))
        )


    ## Eigen values functions
    eigen.val.var <- function(x) return(eigen(var(x))$values)
    eigen.val.cov <- function(x) return(eigen(cov(x))$values)
    eigen.val.pca <- function(x) return(prcomp(x)$sdev^2)
    eigen.val.pco <- function(x) return(abs(apply(var(x), 2, sum)))
    # plot(microbenchmark(eigen.val.var(pca$x),eigen.val.cov(pca$x), eigen.val.pca(pca$x), eigen.val.pco(pca$x)))
    # ## The pco one is fastest by a margin


    ## Checking for equality
    check.equal <- function(x,y, round = 7) {
        expect_equal(unname(round(x, digits = round)), unname(round(y, digits = round)))
    }

    ## Matrices
    pco <- cmdscale(dist(dummy_cla), k=19, eig=TRUE)
    pcoa <- ape::pcoa(dist(dummy_cla))
    pca <- prcomp(space.maker(20, 50, rnorm))
    whatever <- space.maker(20, 50, rnorm)

    ## True eigen values
    true_eigen_pco <- pco$eig
    true_eigen_pcoa <- pcoa$values$Eigenvalues
    true_eigen_pca <- pca$sdev^2

    true_eigen_pco[1]/true_eigen_pco[length(true_eigen_pco)-1]
    true_eigen_pca[1]/true_eigen_pca[length(true_eigen_pca)-1]

    ## The true eigen value is eigen.val.var(matrix); if pco, it is eigen.val.var(matrix)*(nrow-1)
    check.equal(true_eigen_pco[1:19], true_eigen_pcoa)
    check.equal(true_eigen_pco[1:19], eigen.val.var(pco$points)*(nrow(pcoa$vectors)-1))
    check.equal(true_eigen_pco[1:19], eigen.val.cov(pco$points)*(nrow(pcoa$vectors)-1))
    check.equal(true_eigen_pco[1:19], eigen.val.pca(pco$points)*(nrow(pcoa$vectors)-1))
    check.equal(true_eigen_pco[1:19], eigen.val.pco(pco$points)*(nrow(pco$points)-1))

    check.equal(true_eigen_pcoa, eigen.val.var(pcoa$vectors)*(nrow(pcoa$vectors)-1))
    check.equal(true_eigen_pcoa, eigen.val.cov(pcoa$vectors)*(nrow(pcoa$vectors)-1))
    check.equal(true_eigen_pcoa, eigen.val.pca(pcoa$vectors)*(nrow(pcoa$vectors)-1))
    check.equal(true_eigen_pcoa, eigen.val.pco(pcoa$vectors)*(nrow(pco$points)-1))

    check.equal(true_eigen_pca, eigen.val.var(pca$x))
    check.equal(true_eigen_pca, eigen.val.cov(pca$x))
    check.equal(true_eigen_pca, eigen.val.pca(pca$x))
    check.equal(true_eigen_pca, eigen.val.pco(pca$x))

    check.equal(eigen.val.var(t(whatever)), eigen.val.cov(t(whatever)))
    # check.equal(eigen.val.var(t(whatever)), eigen.val.pca(whatever))/(nrow(whatever)-1))
    # check.equal(eigen.val.var(t(whatever)), eigen.val.pco(t(whatever)))
})

test_that("convhull.surface metric", {

    #Create a small dummy matrix
    set.seed(1)
    matrix <- space.maker(5, 3, rnorm)

    #errors
    expect_error(
        convhull.surface(1)
        )
    expect_warning(
        expect_error(
            convhull.surface("a")
            )
        )
    expect_error(
        convhull.surface(list(matrix))
        )

    #Works fine!
    expect_is(
        convhull.surface(matrix)
        ,"numeric")
    expect_equal(
        length(convhull.surface(matrix))
        ,1)
    expect_equal(
        round(convhull.surface(matrix), 3)
        ,10.483)
})

test_that("convhull.volume metric", {

    #Create a small dummy matrix
    set.seed(1)
    matrix <- space.maker(5, 3, rnorm)

    #errors
    expect_error(
        convhull.volume(1)
        )
    expect_warning(
        expect_error(
            convhull.volume("a")
            )
        )
    expect_error(
        convhull.volume(list(matrix))
        )


    #Works fine!
    expect_is(
        convhull.volume(matrix)
        ,"numeric")
    expect_equal(
        length(convhull.volume(matrix))
        ,1)
    expect_equal(
        round(convhull.volume(matrix), 3)
        ,1.25)
})


# test_that("hyper.volume metric", {

#     #Create a small dummy matrix
#     set.seed(1)
#     matrix <- space.maker(5, 3, rnorm)

#     #errors
#     expect_warning(
#     expect_error(
#         hyper.volume(1)
#         ))
#     expect_error(
#         hyper.volume("a")
#         )
#     expect_error(
#         hyper.volume(list(matrix))
#         )

#     #Works fine!
#     output <- capture.output(
#     expect_warning(
#         volume <- hyper.volume(matrix, verbose = FALSE)
#         )
#     )

#     expect_is(
#         volume
#         ,"numeric")
#     expect_equal(
#         round(volume, 3)
#         ,91.863)
# })

test_that("diagonal", {
    matrix <- matrix(seq(1:25), 5, 5)
    expect_equal(diagonal(matrix), sqrt(20))
})



test_that("ancestral.dist", {

    set.seed(1)
    tree <- rtree(6)
    expect_equal(
        get.ancestors(7, tree),
        7)
    expect_equal(
        get.ancestors(4, tree),
        c(11,10,9,8,7))
    expect_equal(
        get.ancestors(4, tree, full = FALSE),
        c(11))

    set.seed(1)
    matrix <- matrix(rnorm(90), 9, 10)
    tree <- rtree(5)
    tree$node.label <- paste0("n", 1:4)
    rownames(matrix) <- c(tree$tip.label, tree$node.label)

    test <- nodes.coordinates(matrix, tree, full = FALSE)
    expect_is(test, "matrix")
    expect_equal(rownames(test), c("n2", "n2", "n3", "n4", "n4", "n1", "n1", "n1", "n3"))

    test <- nodes.coordinates(matrix, tree, full = TRUE)
    expect_is(test[[1]], "matrix")
    expect_is(test[[2]], "matrix")
    expect_is(test[[3]], "matrix")

    expect_equal(rownames(test[[1]]), c("n2", "n2", "n3", "n4", "n4", "n1", "n1", "n1", "n3"))
    expect_equal(rownames(test[[2]]), c("n1", "n1", "n1", "n3", "n3", NA, NA, NA, "n1"))
    expect_equal(rownames(test[[3]]), c(NA, NA, NA, "n1", "n1", NA, NA, NA, NA))

    set.seed(1)
    matrix <- matrix(rnorm(90), 9, 10)
    tree <- rtree(5) ; tree$node.label <- paste0("n", 1:4)
    rownames(matrix) <- c(tree$tip.label, tree$node.label)

    direct_anc_centroids <- nodes.coordinates(matrix, tree, full = FALSE)
    all_anc_centroids <- nodes.coordinates(matrix, tree, full = TRUE)

    test1 <- ancestral.dist(matrix, nodes.coords = direct_anc_centroids)
    test2 <- ancestral.dist(matrix, nodes.coords = all_anc_centroids)

    expect_equal(test1[6], c("n1" = 0))
    expect_equal(test2[6], c("n1" = 0))
    expect_lt(test1[1], test2[1])
    expect_equal(test1[7], test2[7])


    ## Ancestral dist without node coordinates
    test3 <- ancestral.dist(matrix, tree = tree, full = TRUE)
    expect_warning(test4 <- ancestral.dist(matrix))
    expect_equal(names(test3), c(tree$tip.label, tree$node.label))
    expect_equal(names(test4), c(tree$tip.label, tree$node.label))
    expect_equal(test4, centroids(matrix))
    
    ## Ancestral dist with fixed node coordinates
    test5 <- ancestral.dist(matrix, nodes.coords = rep(0, ncol(matrix)))
    expect_equal(test5, centroids(matrix, 0))

})


test_that("span.tree.length", {

    set.seed(1)
    matrix <- matrix(rnorm(50), 10, 5)

    test <- round(span.tree.length(matrix), digit = 1)
    expect_equal(test, c(1.6, 1.4, 3.1, 1.5, 1.5, 1.3, 1.7, 1.7, 1.6))

    test2 <- round(sum(span.tree.length(matrix)), digit = 5)
    expect_equal(test2, 15.40008)

    dist <- as.matrix(dist(matrix))
    expect_equal(test, round(span.tree.length(dist), digit = 1))

    ## Working for a non-distance matrix
    set.seed(1)
    matrix <- matrix(rnorm(25), 5, 5)
    test1 <- round(span.tree.length(matrix), digit = 5)
    test2 <- round(span.tree.length(matrix, method = "euclidean"), digit = 5)
    expect_error(span.tree.length(matrix, method = "wooops"))
    expect_equal(test1, test2)

})

test_that("pairwise.dist", {

    set.seed(1)
    matrix <- matrix(rnorm(30), 3, 10)

    distances <- pairwise.dist(matrix)

    expect_equal(distances[1], as.vector(dist(matrix[c(1,2),])))
    expect_equal(distances[2], as.vector(dist(matrix[c(1,3),])))
    expect_equal(distances[3], as.vector(dist(matrix[c(2,3),])))
})

test_that("radius", {

    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)

    radiuses <- radius(matrix)

    expect_equal(length(radiuses), ncol(matrix))
    expect_equal(round(radiuses, digit = 5), round(c(1.4660109,0.9556041,2.2528229,0.5045006,2.0705822,1.1221754,1.4195993,0.9011047,0.7181528,0.9463714), digit = 5))
})


test_that("n.ball.volume", {

    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)

    volume_sphere <- n.ball.volume(matrix)
    volume_spheroid <- n.ball.volume(matrix, sphere = FALSE)

    expect_equal(round(volume_sphere, digit = 5), round(1.286559, digit = 5))
    expect_equal(round(volume_spheroid, digit = 5), round(8.202472, digit = 5))
})


test_that("quantiles", {
    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)
    ## Gives the same results as range if quantile = 100
    expect_equal(quantiles(matrix, quantile = 100), ranges(matrix))
    expect_equal(quantiles(matrix, quantile = 100, k.root = TRUE), ranges(matrix, k.root = TRUE))
    ## Default 95%
    expect_equal(round(quantiles(matrix), digit = 5),
                 round(c(2.28341, 1.49103, 3.52845, 0.97363, 2.68825, 1.74203, 2.51121, 1.47926, 1.32815, 1.51783), digit = 5))
})

test_that("displacements", {
    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)
    
    ## Default behaviour
    expect_equal(
        round(displacements(matrix), digits = 5),
        round(c(0.9336158, 0.9823367, 1.1963676, 1.0412577, 1.0469445), digits = 5)
        )
    expect_equal(
        round(displacements(matrix, method = "manhattan"), digits = 5),
        round(c(0.8395959, 0.9557916, 1.1694120, 1.1046963, 1.0906642), digits = 5)
        )
    expect_equal(
        round(displacements(matrix, reference = 100), digits = 5),
        round(c(113.55976, 260.10424, 155.41071, 87.47385, 133.64356), digits = 5)
        )
})

test_that("neighbours", {
    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)
    
    ## Default behaviour
    expect_equal(
        round(neighbours(matrix), digits = 5),
        round(c(2.63603, 2.31036, 2.58740, 4.00868, 2.31036), digits = 5)
        )
    expect_equal(
        round(neighbours(matrix, method = "manhattan"), digits = 5),
        round(c(6.14827, 6.14827, 7.44352, 9.98804, 6.40357), digits = 5)
        )
    expect_equal(
        round(neighbours(matrix, which = max), digits = 5),
        round(c(5.943374, 4.515470, 4.008678, 5.943374, 5.059321), digits = 5)
        )
})

test_that("neighbours", {
    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)
    
    ## Default behaviour
    expect_equal(
        round(neighbours(matrix), digits = 5),
        round(c(2.63603, 2.31036, 2.58740, 4.00868, 2.31036), digits = 5)
        )
    expect_equal(
        round(neighbours(matrix, method = "manhattan"), digits = 5),
        round(c(6.14827, 6.14827, 7.44352, 9.98804, 6.40357), digits = 5)
        )
    expect_equal(
        round(neighbours(matrix, which = max), digits = 5),
        round(c(5.943374, 4.515470, 4.008678, 5.943374, 5.059321), digits = 5)
        )
})

test_that("func.eve", {
    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)
    
    ## Default behaviour
    expect_equal(
        test1 <- round(func.eve(matrix), digits = 5),
        round(c(0.87027), digits = 5)
        )

    expect_equal(
        round(func.eve(matrix, method = "manhattan"), digits = 5),
        round(c(0.88917), digits = 5)
        )

    distance <- as.matrix(dist(matrix))
    expect_equal(
        round(func.eve(distance), digits = 5),
        test1
    ) 
})

test_that("func.div", {
    set.seed(1)
    matrix <- matrix(rnorm(50), 5, 10)
    
    ## Default behaviour
    expect_equal(
        test1 <- round(func.div(matrix), digits = 5),
        round(c(0.79003), digits = 5)
        )
})

test_that("angles", {
    set.seed(1)
    matrix <- matrix(rnorm(90), 9, 10)
    matrix[,1] <- 1:9
    matrix[,2] <- matrix[,1]*2
    matrix[,3] <- matrix[,1]/2

    ## Angles in degrees
    test_angles <- angles(matrix)
    expect_equal(length(test_angles), ncol(matrix))
    expect_equal(test_angles[1], 45)
    expect_equal(round(test_angles[2], 1), 26.6)
    expect_equal(round(test_angles[3], 1), 63.4)

    ## Angles in degrees with base shift (90)
    test_angles2 <- angles(matrix, base = 90)
    expect_equal(test_angles2, test_angles + 90)

    ## Angles in radian
    test_angles2 <- angles(matrix, unit = "radian")
    expect_equal(test_angles2 * 180/pi, test_angles)

    ## Angles in slopes
    test_angles2 <- angles(matrix, unit = "slope")
    expect_equal(test_angles2[1], 1)
    expect_equal(test_angles2[2], 0.5)
    expect_equal(test_angles2[3], 2)

    ## Angles only significant
    expect_warning(test_angles1 <- angles(matrix, significant = TRUE))
    test_angles2 <- angles(matrix, significant = FALSE)
    expect_equal(test_angles1[c(1,2,3)], test_angles2[c(1,2,3)])
    expect_true(all(test_angles1[-c(1,2,3)] == 0))
    expect_false(all(test_angles2[-c(1,2,3)] == 0))
})

test_that("deviation", {
    test <- matrix(0, ncol = 2, nrow = 4)
    test[1, ] <- c(3, 3)

    ## Input the hyperplane for a 2d matrix (line is the x axis)
    expect_equal(deviations(test, hyperplane = c(0, 1, 0)), c(3, 0, 0, 0))
    expect_equal(deviations(test, hyperplane = c(3, 1, 0)), c(6, 3, 3, 3))
    
    ## 2d matrix (line is x = y) 
    test <- cbind(1:4, 1:4)
    expect_equal(deviations(test, hyperplane = c(0, -1, 1)), c(0, 0, 0, 0))

    ## Works with 3d (compare to surfaces)
    test <- matrix(0, ncol = 3, nrow = 4)
    test[1,  ] <- c(5, 5, 5)
    expect_equal(deviations(test, hyperplane = c(0, 1, 0, 0)), c(5, 0, 0, 0))
    expect_equal(deviations(test, hyperplane = c(5, 1, 0, 0)), c(10, 5, 5, 5))

    ## Works with 3d x=y=z planes
    test <- cbind(1:4, 1:4, 1:4)
    expect_equal(deviations(test, hyperplane = c(0, 1, -1, 0)), c(0, 0, 0, 0))

    ## Works with estimating the plane
    test <- cbind(1:4, 1:4)
    expect_equal(deviations(test), c(0, 0, 0, 0))
    
    test <- cbind(test, test, test, test)
    expect_equal(deviations(test), c(0, 0, 0, 0))

    set.seed(1)
    matrix <- matrix(rnorm(90), 9, 10)
    expect_equal(
        round(deviations(matrix, significant = TRUE)),
        rep(0, 9))
    expect_equal(
        round(deviations(matrix, hyperplane = c(100, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0))),
        c(71, 72, 72, 69, 69, 72, 70, 70, 71))
})

test_that("min.distance", {

    ## Predictable matrix
    set.seed(1)
    matrix <- matrix(NA, 5, 2)
    matrix2 <- matrix(runif(8), 4, 2)
    matrix2[,1] <- scale(matrix2[,1])
    matrix2[,2] <- scale(matrix2[,2])
    matrix[1,] <- c(-2, 1)
    matrix[2,] <- c(-1.5, -1)
    matrix[3,] <- c(1, 0)
    matrix[4,] <- c(0.5, 1)
    matrix[5,] <- c(0.25, -1)  ## Recentring with a shift of -0.35 on the x axis

    #projected_distances should be matrix[,1]+0.35
    ## Make the combined matrix
    combined_matrix <- as.matrix(rbind(matrix, matrix2))
    ## Get the groups IDs
    groups <- list(1:nrow(matrix), (1:nrow(matrix2)+nrow(matrix)))
    ## Centre that matrix onto the centroid of group 1
    centred_matrix <- centre.matrix(combined_matrix, groups[[1]])
    expect_equal_round(colMeans(centred_matrix[1:5, ]), c(0,0))
    ## Get the centroid vector in the centred matrix (the centroid of group 2)
    centroid <- colMeans(centred_matrix[groups[[2]],])
    ## Get the length of each projected points on the centroid vector
    projected_lengths <- apply(centred_matrix, 1, get.proj.length, centroid = centroid, length = sqrt(sum(centroid^2)))
    expect_equal_round(projected_lengths[1:5], matrix[,1]+0.35, 3)

    ## Super simple example (works both directions)
    matrix1 <- matrix2 <- matrix(c(1,2,3), 3, 2)
    matrix1[, 1] <- 1
    matrix2[, 1] <- 2
    expect_equal(group.dist(matrix1, matrix2), 1)
    expect_equal(group.dist(matrix2, matrix1), 1)
    matrix1[, 1] <- 8.5
    expect_equal(group.dist(matrix1, matrix2), 6.5)
    expect_equal(group.dist(matrix2, matrix1), 6.5)

    ## Function works with 2D data
    ## Overlap version
    set.seed(1)
    matrix <- matrix(rnorm(20), 10, 2)
    matrix2 <- matrix(rnorm(30), 15, 2)
    expect_equal(group.dist(matrix, matrix2), 0)
    expect_equal(group.dist(matrix2, matrix), 0)
    ## Working with probabilities
    expect_equal(group.dist(matrix, matrix2, probs = c(0.1, 0.9)), 0)
    expect_equal_round(group.dist(matrix, matrix2, probs = c(0.5)), 0.339, digits = 3)

    ## No overlap version
    set.seed(1)
    matrix <- matrix(rnorm(20, mean = 3), 10, 2)
    matrix2 <- matrix(rnorm(30), 15, 2)  
    expect_equal_round(group.dist(matrix, matrix2), 2.233, digits = 3) 
    expect_equal_round(group.dist(matrix2, matrix), 2.233, digits = 3)
    ## Distance gets bigger when lowering the quantiles
    expect_equal_round(group.dist(matrix2, matrix, probs = c(.025, .975)), 2.383, digits = 3)
    expect_equal_round(group.dist(matrix2, matrix, probs = c(.1, .9)), 2.897, digits = 3)
    expect_equal_round(group.dist(matrix2, matrix, probs = c(.3, .7)), 3.993, digits = 3)

    ## Function works with 3D data
    ## Non-overlap version
    set.seed(1)
    disc1 <- space.maker(100, 3, c(runif, rnorm, rnorm), arguments =  list(list(min = 0, max = 0), NULL, NULL))
    disc2 <- space.maker(100, 3, c(runif, rnorm, rnorm), arguments =  list(list(min = 1, max = 1), NULL, NULL))
    ## Visualise
    # plot3d <- scatterplot3d(rbind(disc1, disc2), color = c(rep("orange", 100), rep("blue", 100)), pch = 20)
    # plot3d$points3d(rbind(colMeans(disc1), colMeans(disc2)),
    #   col= c("orange", "blue"), pch=13, cex = 2)
    # plot3d$points3d(rbind(colMeans(disc1), colMeans(disc2)), type = "l", col = "darkgreen")

    ## Close to 1
    expect_equal_round(group.dist(disc1, disc2, probs = 0.5), 1.018, digits = 3)
    expect_equal_round(group.dist(disc1, disc2), 0.46, digits = 3)

    ## Overlap version
    set.seed(1)
    disc <- space.maker(100, 3, c(runif, rnorm, rnorm), arguments =  list(list(min = -2, max = -2), NULL, NULL))
    sphere <- space.maker(100, 3, c(rnorm, rnorm, rnorm))
    ## Visualise
    # plot3d <- scatterplot3d(rbind(disc, sphere), color = c(rep("orange", 100), rep("blue", 100)), pch = 20)
    # plot3d$points3d(rbind(colMeans(disc), colMeans(sphere)),
    #   col= c("orange", "blue"), pch=13, cex = 2)
    # plot3d$points3d(rbind(colMeans(disc), colMeans(sphere)), type = "l", col = "darkgreen")

    expect_equal_round(group.dist(disc, sphere, probs = 0.5), 2.015, digits = 3)
    expect_equal_round(group.dist(disc, sphere), 0)

    ## Function works with nD data
    ## non-Overlap
    set.seed(1)
    matrix <- matrix(rnorm(150), 15, 10)
    matrix2 <- matrix(rnorm(100, mean = 6), 10, 10)
    expect_equal_round(group.dist(matrix, matrix2, probs = 0.5), 18.803, digits = 3)
    expect_equal_round(group.dist(matrix, matrix2), 16.35804, digits = 5)

    ## Overlap
    set.seed(1)
    matrix <- matrix(rnorm(150), 15, 10)
    matrix2 <- matrix(rnorm(100), 10, 10)
    expect_equal_round(group.dist(matrix, matrix2), 0)
    expect_equal_round(group.dist(matrix, matrix2, probs = 0.5), 1.472447, digits = 3)

    ## Function gives the same results as hypervolume?
    matrix <- subset(iris, Species=="setosa")[,1:3]
    matrix2 <- subset(iris, Species=="virginica")[,1:3]

    ## Creating the hypervolume objects
    # set.seed(1)
    # hv1 = hypervolume::hypervolume_gaussian(matrix, verbose = FALSE)
    # hv2 = hypervolume::hypervolume_gaussian(matrix2, verbose = FALSE)

    # ## Giving similar results than hypervolume?
    # min_distance_est <- hypervolume::hypervolume_distance(hv1, hv2, type = "minimum", num.points.max = 1000, check.memory = FALSE)
    # cent_distance_est <- hypervolume::hypervolume_distance(hv1, hv2, type = "centroid", num.points.max = 1000, check.memory = FALSE)
    # expect_equal_round(min_distance_est, 2.298591, digits = 5)
    # expect_equal_round(cent_distance_est, 4.527476, digits = 5)

    expect_equal_round(group.dist(matrix, matrix2), 2.444375, digits = 5)
    expect_equal_round(group.dist(matrix, matrix2, probs = 0.5), 4.311056, digits = 5)
    ## Pretty close!

})