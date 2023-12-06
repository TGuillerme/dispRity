#TESTING boot.matrix

#context("dispRity.metric")

nocov <- TRUE

test_that("dimension generic", {
    expect_equal(capture_output(dimension.level3.fun()), "No implemented Dimension level 3 functions implemented in dispRity!\nYou can create your own by using: ?make.metric")
    expect_equal(capture_output(dimension.level2.fun()), "Dimension level 2 functions implemented in dispRity:\n?ancestral.dist\n?angles\n?centroids\n?deviations\n?displacements\n?edge.length.tree\n?neighbours\n?pairwise.dist\n?point.dist\n?projections\n?projections.tree\n?ranges\n?radius\n?variances\n?span.tree.length")
    expect_equal(capture_output(dimension.level1.fun()), "Dimension level 1 functions implemented in dispRity:\n?convhull.surface\n?convhull.volume\n?diagonal\n?ellipsoid.volume\n?func.div\n?func.eve\n?group.dist\n?mode.val\n?n.ball.volume\n?roundness")
    expect_equal(capture_output(between.groups.fun()), "Between groups functions implemented in dispRity:\n?disalignment # level 1\n?group.dist # level 1\n?point.dist # level 2\n?projections.between # level 2")
})

test_that("select.method sanitizing works", {
    test <- select.method("euclidean")
    expect_is(test, "function")
    expect_error(select.method(NULL))
    error <- capture_error(select.method("NULL"))
    expect_equal(error[[1]], "method argument can only be \"euclidean\" or \"manhattan\".")
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

test_that("ellipsoid.volume metric", {
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
    test_vol <- ellipsoid.volume(dummy_ord)
    # test
    expect_equal(
    	true_vol, test_vol
    	)
    # test with the eigen val estimation
    expect_equal(
        true_vol, ellipsoid.volume(dummy_ord, eigen.value = dummy_eig/(nrow(dummy_dis)-1))
        )

    # Now testing for PCOA
    dummy_ord <- pcoa(dummy_dis)
    dummy_eig <- dummy_ord$values[,1]
    dummy_ord <- dummy_ord$vectors
    # Calculate the true volume (with eigen values)
    true_vol <- volume.true(dummy_ord, dummy_eig/(nrow(dummy_dis)-1))
    # Calculate the volume without the eigen values
    test_vol <- ellipsoid.volume(dummy_ord)
    # test
    expect_equal(
    	true_vol, test_vol
    	)
    # test with the eigen val estimation
    expect_equal(
        true_vol, ellipsoid.volume(dummy_ord, eigen.value = dummy_eig/(nrow(dummy_dis)-1))
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
    matrix <- matrix(rnorm(90), 9, 10)
    tree <- rtree(5) ; tree$node.label <- paste0("n", 1:4)
    rownames(matrix) <- c(tree$tip.label, tree$node.label)
    
    ## New version
    expect_equal_round(ancestral.dist(matrix, tree), c(t3 = 3.226549, t5 = 4.014473, t4 = 3.392561, t2 = 3.963446, t1 = 4.389945, n1 = 0.000000, n2 = 4.938898, n3 = 3.490736, n4 = 2.746763), 6)
    expect_equal_round(ancestral.dist(matrix, tree, to.root = TRUE), c(t3 = 4.354400, t5 = 3.845289, t4 = 4.391939, t2 = 4.875274, t1 = 4.490714, n1 = 0.000000, n2 = 4.938898, n3 = 3.490736, n4 = 3.300742), 6)
    ## Test with a dispRity object
    test <- dispRity(matrix, metric = ancestral.dist, tree = tree)
    expect_equal(c(test$disparity[[1]][[1]]), unname(ancestral.dist(matrix, tree)))

    ## Works with time slices!
    data(BeckLee_mat99)
    data(BeckLee_tree)
    data <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "acctran", time = 5)
    test <- dispRity(data, metric = ancestral.dist)
    expect_equal(summary(test)$obs.median, c(2.457, 2.538, 2.677, 2.746, 2.741))
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
    data(iris)
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

test_that("point.dist", {
    ## Simple tests
    matrix1 <- matrix(1, 5, 4)
    matrix2 <- matrix(2, 10, 4)
    expect_equal(point.dist(matrix1, matrix1), rep(0, 5))
    expect_equal(point.dist(matrix2, matrix2), rep(0, 10))
    expect_equal(point.dist(matrix1, matrix2), rep(2, 5))
    expect_equal(point.dist(matrix2, matrix1), rep(2, 10))
    expect_equal(point.dist(matrix2, matrix1, point = sd), rep(4, 10))
    expect_equal(point.dist(matrix1, matrix2, point = sd), rep(2, 5))
    expect_equal(point.dist(matrix2, matrix1, point = sd, method = "manhattan"), rep(8, 10))
    expect_equal(point.dist(matrix1, matrix2, point = sd, method = "manhattan"), rep(4, 5))

    expect_warning(test <- custom.subsets(rbind(matrix1, matrix2), group = list(1:5, 6:15)))
    test2 <- dispRity(test, metric = point.dist, between.groups = TRUE)
    expect_equal(summary(test2)$obs.median, 2)
    test3 <- dispRity(test, metric = c(mean, point.dist), between.groups = TRUE)
    expect_equal(summary(test3)$obs, 2)

    set.seed(1)
    data(BeckLee_tree)
    data(BeckLee_mat99)
    test <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "equal.split", time = 10)
    test2 <- dispRity(test, metric = point.dist, between.groups = TRUE)
    expect_equal(summary(test2)$obs.median, c(1.594, 1.838, 1.843, 1.969, 1.828, 1.977, 1.934, 1.892, 1.950))
})

test_that("projections", {

    ## Simple 1D test
    matrix <- matrix(c(0,1,2,0,0,0), 3, 2)
    # plot(matrix, pch = 19)
    # lines(matrix(c(0, 0, colMeans(matrix)), 2,2, byrow = TRUE), lty = 2)
    # lines(matrix(c(2, 0, c(0, 0)), 2,2, byrow = TRUE), lty = 2, col = "red")
    rownames(matrix) <- LETTERS[1:3]

    ## Position default (from 0 to centroid)
    expect_equal(projections(matrix, centre = FALSE, abs = FALSE), c(0, 1, 2))
    expect_equal(projections(matrix, centre = TRUE, abs = TRUE), c(1,1,3))
    ## Distance default (from 0 to centroid)
    expect_equal(projections(matrix, measure = "distance", centre = FALSE), c(0, 0, 0))
    ## Position from 0 to 1)
    expect_equal(projections(matrix, point2 = c(2, 0), centre = FALSE, abs = FALSE), c(0, 0.5, 1))
    ## Distance default (from 0 to centroid)
    expect_equal(projections(matrix, point2 = c(2, 0), measure = "distance", centre = TRUE, abs = TRUE), c(0, 0, 0))
    ## Same
    expect_equal(projections(matrix, point2 = c(2, 0), measure = "distance", centre = FALSE, abs = FALSE), c(0, 0, 0))
    ## Position from -1 to 1)
    expect_equal(projections(matrix, point1 = c(-1, 0), point2 = c(1, 0), centre = FALSE, abs = FALSE), c(0.5, 1, 1.5))

    ## Simple 2D test
    matrix <- matrix(c(1,2,3,1,2,3), 3, 2)
    # plot(matrix)

    ## Position default (from 0 to centroid)
    expect_equal(projections(matrix, centre = FALSE, abs = FALSE), c(0.5, 1, 1.5))
    ## Distance default (from 0 to centroid)
    expect_equal(projections(matrix, measure = "distance"), c(0, 0, 0))
    ## Position from 0 to 1)
    expect_equal(projections(matrix, point2 = c(1, 0), centre = TRUE, abs = TRUE), c(1, 3, 5))
    ## Distance default (from 0 to centroid)
    expect_equal(projections(matrix, point2 = c(0.5, 0), measure = "distance"), c(2, 4, 6))
    ## Position from -1 to 1)
    expect_equal(projections(matrix, point1 = c(-1, 0), point2 = c(1, 0), centre = FALSE, abs = FALSE), c(1, 1.5, 2))  


    ## 400D matrix (showing off)
    set.seed(1)
    test <- matrix(rnorm(400*5), 5, 400)
    rownames(test) <- letters[1:5]
    test_res <- projections(test, measure = "position", point1 = colMeans(test), point2 = test["a",], centre = FALSE, abs = TRUE)
    expect_equal_round(test_res, c(1, 0.324, 0.141, 0.269, 0.265), 3)
    test_res <- projections(test, measure = "position", point1 = 1, point2 = 2, centre = TRUE, abs = FALSE)
    expect_equal_round(test_res, c(-3.148523, -2.934695, -3.019256, -3.181841, -2.855235), 3)
    test_res <- projections(test, measure = "distance", point1 = colMeans(test), point2 = test["c",], centre = FALSE)
    expect_equal_round(test_res, c(1.023, 1.007, 0, 1.005, 0.953), 3)
    test_res <- projections(test, measure = "degree", point1 = colMeans(test), point2 = test["e",])
    expect_equal_round(test_res, c(106.11636, 99.19498,105.32585, 104.94005, 0), 3)
    test_res <- projections(test, measure = "radian")
    expect_equal_round(test_res, c(1.100621, 1.177726, 1.069398, 1.146989, 1.079798), 3)

    ## Orthogonality
    angles <-     c(0, 1, 89, 90, 91, 179, 180, 181, 269, 270, 271, 359, 360)
    orthogonal <- c(0, 1, 89, 90, 89,   1,   0,   1,   89, 90,  89,   1,   0)
    expect_equal(sapply(angles, orthogonise), orthogonal/90)
})

test_that("projections.tree ", {
    
    set.seed(1)
    matrix <- matrix(rnorm(90), 9, 10)
    tree <- rtree(5)
    tree <- makeNodeLabel(tree, prefix = "n")
    tree2 <- root(tree, 1)
    rownames(matrix) <- c(tree$tip.label, tree$node.label)
    ## Test get.root
    expect_equal(get.root(row = 1, matrix = matrix, tree = tree), matrix["n1", ])
    expect_equal(get.root(row = 6, matrix = matrix, tree = tree), matrix["n1", ])
    expect_equal(get.root(row = 6, matrix = matrix, tree = tree2), matrix["n2", ])
    ## Test get.anc
    expect_equal(get.ancestor(row = 1, matrix = matrix, tree = tree), matrix["n2", ])
    expect_equal(get.ancestor(row = 2, matrix = matrix, tree = tree), matrix["n2", ])
    expect_equal(get.ancestor(row = 3, matrix = matrix, tree = tree), matrix["n3", ])
    ## Root is root
    expect_equal(get.ancestor(row = 6, matrix = matrix, tree = tree), matrix["n1", ])
    ## Test get.tips
    expect_equal(get.tips(row = 1, matrix = matrix, tree = tree), colMeans(matrix[1:5, ]))
    expect_equal(get.tips(row = 2, matrix = matrix, tree = tree), colMeans(matrix[1:5, ]))
    ## Test get.nodes
    expect_equal(get.nodes(row = 1, matrix = matrix, tree = tree), colMeans(matrix[-c(1:5), ]))
    expect_equal(get.nodes(row = 2, matrix = matrix, tree = tree), colMeans(matrix[-c(1:5), ]))
    ## Test get.livings
    expect_equal(get.livings(row = 1, matrix = matrix, tree = tree), matrix["t1", ])
    expect_equal(get.livings(row = 2, matrix = matrix, tree = tree), matrix["t1", ])
    ## Test get.fossils
    expect_equal(get.fossils(row = 1, matrix = matrix, tree = tree), colMeans(matrix[-5, ]))
    expect_equal(get.fossils(row = 2, matrix = matrix, tree = tree), colMeans(matrix[-5, ]))
    ## Test sapply.projections
    expect_equal(sapply.projections(row = 1, matrix = matrix, tree = tree, from = get.root, to = get.livings),
                projections(matrix[1, , drop = FALSE], point1 = get.root(matrix, tree), point2 = get.livings(matrix, tree)))
    expect_equal(sapply.projections(row = 5, matrix = matrix, tree = tree, from = get.root, to = get.livings), 1)
    ## Test fun

    ## Making a dummy tree with node labels
    set.seed(8)
    dummy_matrix <- matrix(rnorm(90), 9, 10)
    dummy_tree <- makeNodeLabel(rtree((nrow(dummy_matrix)/2)+1))
    named_matrix <- dummy_matrix
    rownames(named_matrix) <- c(dummy_tree$tip.label,
                                dummy_tree$node.label)
    error <- capture_error(projections.tree(named_matrix, dummy_tree, type = c("BOOT", "ancestor")))
    expect_equal(error[[1]], "The following type argument is not recognised in projections.tree: BOOT")
    expect_equal_round(projections.tree(named_matrix, dummy_tree, type = c("root", "ancestor")) , c(1.0250009, 0.3089018, 0.2359258, 0.5322932, 0.3297963, NaN, NaN, NaN, 0.4874452), 3)
    expect_equal_round(projections.tree (named_matrix, dummy_tree, type = c("nodes", "tips"), measure = "distance", centre = FALSE), c(1.383, 0.860, 1.656, 0.886, 1.391, 1.080, 1.466, 1.366, 1.674), 3)
    user.fun <- function(matrix, tree, row = NULL) {
         return(colMeans(matrix[tree$node.label[1:3], ]))
    }
    expect_equal_round(projections.tree(named_matrix, dummy_tree, type = c(0, user.fun)), c(0.2873904, -0.2930323, 0.7724387, 0.3042220, -2.6090564, 0.7318370, 1.7202007, 0.5479623, -0.1651890), 3)
})

test_that("edge.length.tree works", {
    ## A ladderized tree
    tree <- read.tree(text = "((((((A,B), C), D), E), F), G);")
    tree$edge.length <- rep(1, 7+6)
    tree$node.label <- letters[1:6]
    ## An empty matrix (with the right elements)
    matrix1 <- matrix(NA, nrow = 7, ncol = 2)
    rownames(matrix1) <- tree$tip.label
    matrix2 <- matrix(NA, nrow = 7+6, ncol = 2)
    rownames(matrix2) <- c(tree$tip.label, tree$node.label)

    expect_equal(edge.length.tree(matrix1, tree), c(6,6,5,4,3,2,1))
    expect_equal(edge.length.tree(matrix2, tree), c(6,6,5,4,3,2,1,0,1,2,3,4,5))
    expect_equal(edge.length.tree(matrix2[c(1,3,5,7,10),], tree), c(6,6,5,4,3,2,1,0,1,2,3,4,5)[c(1,3,5,7,10)])
    expect_equal(edge.length.tree(matrix1, tree, to.root = FALSE), rep(1, 7))
    expect_equal(edge.length.tree(matrix2, tree, to.root = FALSE), c(rep(1, 7), 0, rep(1,5)))
    expect_equal(edge.length.tree(matrix1[c(1,2), ], tree, to.root = FALSE), c(1,1))
    tree$edge.length[c(3,7)] <- 10
    expect_equal(edge.length.tree(matrix1[c(1,2), ], tree, to.root = FALSE), c(1,10))
})

test_that("projections.between works", {

    set.seed(1)
    ## Two dummy matrices
    matrix_1 <- matrix(rnorm(16), 4, 4)
    matrix_2 <- matrix(rnorm(16), 4, 4)
    ## Projecting the major axis of matrix_2 onto the one from matrix_1
    expect_equal_round(projections.between(matrix_1, matrix_2, centre = FALSE, abs = TRUE), 0.312014, 6)
    ## Projecting both second major 0.75 axes
    ## and getting the rejections (see projections() for option details)
    expect_equal_round(projections.between(matrix_1, matrix_2, measure = "distance", axis = 4, level = 0.75, centre = FALSE), 0.7356813, 6)

    ## Testing covarly
    data(charadriiformes)
    data <- MCMCglmm.subsets(data       = charadriiformes$data,
                             posteriors = charadriiformes$posteriors,
                             group = MCMCglmm.levels(charadriiformes$posteriors)[1:4],
                             rename.groups = c(levels(charadriiformes$data$clade), "phylogeny"))
    ## Testing the metric in the pipeline without covar option
    no_covar <- dispRity(data, metric = projections.between, between.groups = TRUE, centre = FALSE, abs = FALSE)
    ## Test the values out
    disparity <- get.disparity(no_covar)
    expect_equal(names(disparity), c("gulls:plovers", "gulls:sandpipers", "gulls:phylogeny", "plovers:sandpipers", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal_round(unname(unlist(disparity)), c(-0.1915237,-1.5257785,-1.5257785,0.2534359,0.2534359,1.0000000), 6)

if(!nocov) {
    ## Testing the metric in the pipeline with covar option
    proj_metric <- as.covar(projections.between)
    expect_equal(names(formals(proj_metric)), c("matrix", "matrix2", "..."))
    is_covar <- dispRity(data, metric = proj_metric, between.groups = TRUE, centre = FALSE, abs = FALSE)
    ## Test the values out
    disparity <- get.disparity(is_covar, concatenate = FALSE)
    expect_equal(names(disparity), c("gulls:plovers", "gulls:sandpipers", "gulls:phylogeny", "plovers:sandpipers", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal(unique(unlist(lapply(disparity, length))), 1000)
    disparity <- get.disparity(is_covar)
    #expect_equal_round(unname(unlist(disparity)), c(2.8460391, 1.5703472, 1.2262642, 0.3840770, 0.2397510, 0.7011024), 2)
    expect_equal_round(unname(unlist(disparity)), c(2.8175937, 1.5718191, 1.2262642, 0.3840770, 0.2389399, 0.7011024), 1)
}

    ## Same as above but with options
    no_covar <- dispRity(data, metric = projections.between, between.groups = TRUE, measure = "degree", level = 0.9, centre = FALSE, abs = FALSE)
    disparity <- get.disparity(no_covar)
    expect_equal(names(disparity), c("gulls:plovers", "gulls:sandpipers", "gulls:phylogeny", "plovers:sandpipers", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal_round(unname(unlist(disparity)), c(96.69595,148.31804,148.31804,76.57482,76.57482,0), 5)

if(!nocov) {
    is_covar <- dispRity(data, metric = as.covar(projections.between), between.groups = TRUE, measure = "degree", level = 0.9, centre = FALSE, abs = FALSE)
    disparity <- get.disparity(is_covar, concatenate = FALSE)
    expect_equal(names(disparity), c("gulls:plovers", "gulls:sandpipers", "gulls:phylogeny", "plovers:sandpipers", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal(unique(unlist(lapply(disparity, length))), 1000)
    disparity <- get.disparity(is_covar)
    #expect_equal_round(unname(unlist(disparity))[-c(4,5)], c(25.115014, 11.407162, 9.240426, 25.914558, 26.988654, 10.379432)[-c(4,5)], 3)
    expect_equal_round(unname(unlist(disparity))[-c(4,5)], c(25.115014, 11.407162, 9.240426, 25.986941, 27.336217, 10.353848)[-c(4,5)], 1)
}
})

test_that("disalignment works", {

    set.seed(1)
    ## Two dummy matrices
    matrix_1 <- matrix(rnorm(16), 4, 4)
    matrix_2 <- matrix(rnorm(16), 4, 4)
    ## Projecting the major axis of matrix_2 onto the one from matrix_1
    expect_equal_round(disalignment(matrix_1, matrix_2), 0.03887824, 6)
    expect_equal_round(disalignment(matrix_2, matrix_1), 0.06599623, 6)
    expect_equal_round(disalignment(matrix_1, matrix_2, axis = 4, level = 0.75, point.to.reject = 2), 0.1618571, 6)

    ## Testing covarly
    data(charadriiformes)
    data <- MCMCglmm.subsets(data       = charadriiformes$data,
                             posteriors = charadriiformes$posteriors,
                             group      = MCMCglmm.levels(charadriiformes$posteriors)[1:4],
                             rename.groups = c(levels(charadriiformes$data$clade), "phylogeny"),
                             n = 50)
    ## Testing the metric in the pipeline without covar option
    no_covar <- dispRity(data, metric = disalignment, between.groups = TRUE, centre = FALSE, abs = FALSE)
    ## Test the values out
    disparity <- get.disparity(no_covar)
    expect_equal(names(disparity), c("gulls:plovers", "gulls:sandpipers", "gulls:phylogeny", "plovers:sandpipers", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal_round(unname(unlist(disparity)), c(0.02345475, 0.03010739, 0.03010739, 0.03055703, 0.03055703, 0.01782711), 6)

if(!nocov) {
    ## Testing the metric in the pipeline with covar option
    cov_dis <- as.covar(disalignment, VCV = c(FALSE, TRUE), loc = c(TRUE, FALSE))
    expect_equal(names(formals(cov_dis)), c("matrix", "matrix2", "..."))
    is_covar <- dispRity(data, metric = cov_dis, between.groups = list(c(1,4), c(2,4), c(3, 4)))
    ## Test the values out
    disparity <- get.disparity(is_covar, concatenate = FALSE)
    expect_equal(names(disparity), c("gulls:phylogeny", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal(unique(unlist(lapply(disparity, length))), 50)
    #expect_equal_round(unname(unlist(disparity)), c(2.8460391, 1.5703472, 1.2262642, 0.3840770, 0.2397510, 0.7011024), 2)
    expect_equal_round(unname(unlist(lapply(disparity, median))), c(0.06060223, 0.02611046, 0.06848407), 5)
}
})

test_that("roudness works", {
    set.seed(1)
    dummy_matrix <- matrix(rnorm(50), 5, 10)
    test <- roundness(dummy_matrix, vcv = TRUE)
    expect_equal_round(test, 0.1776007)
    test <- roundness(var(dummy_matrix), vcv = FALSE)
    expect_equal_round(test, 0.1776007)
})
