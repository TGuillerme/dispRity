#TESTING boot.matrix

context("dispRity.metric")

test_that("dimension generic", {
    expect_equal(capture_output(dimension.level3.fun()), "No implemented Dimension level 3 functions implemented in dispRity!\nYou can create your own by using: ?make.metric")
    expect_equal(capture_output(dimension.level2.fun()), "Dimension level 2 functions implemented in dispRity:\n?ranges\n?variances\n?centroids\n?ancestral.dist\n?pairwise.dist\n?radius")
    expect_equal(capture_output(dimension.level1.fun()), "Dimension level 1 functions implemented in dispRity:\n?ellipse.volume\n?convhull.surface\n?convhull.volume\n?diagonal\n?mode.val\n?span.tree.length\n?n.ball.volume")


})



#Testing the metrics
test_that("k.root", {
    mat <- matrix(abs(rnorm(25)), 5, 5)
    test <- k.root(mat, 5)
    ## Right format
    expect_is(test, "matrix")
    expect_equal(dim(test), dim(mat))
    ## Repeatable
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
    dummy_cla <- replicate(50, sample(c(0,1,2), 20, replace=T, prob=c(0.45,0.45,0.1)))
    rownames(dummy_cla) <- LETTERS[1:20]
    dummy_tre <- rtree(20, tip.label=LETTERS[1:20])

    # Dummy ordination
    dummy_dis <- as.matrix(dist(dummy_cla, method="euclidean"))
    dummy_ord <- cmdscale(dummy_dis, k=19, eig=TRUE)
    dummy_eig <- dummy_ord$eig
    dummy_ord <- dummy_ord$points
    # Calculate the true volume (with eigen values)
    true_vol <- volume.true(dummy_ord, dummy_eig)
    # Calculate the volume without the eigen values
    test_vol <- ellipse.volume(dummy_ord)
    # test
    expect_equal(
    	true_vol, test_vol
    	)
    # test with the eigen val estimation
    expect_equal(
        true_vol, ellipse.volume(dummy_ord, eigen.value = dummy_eig)
        )

    # Now testing for PCOA
    dummy_ord <- pcoa(dummy_dis)
    dummy_eig <- dummy_ord$values[,1]
    dummy_ord <- dummy_ord$vectors
    # Calculate the true volume (with eigen values)
    true_vol <- volume.true(dummy_ord, dummy_eig)
    # Calculate the volume without the eigen values
    test_vol <- ellipse.volume(dummy_ord)
    # test
    expect_equal(
    	true_vol, test_vol
    	)
    # test with the eigen val estimation
    expect_equal(
        true_vol, ellipse.volume(dummy_ord, eigen.value = dummy_eig)
        )


    # # Testing with eigen
    # dummy_ord <- eigen(dummy_dis, symmetric=TRUE)
    # dummy_eig <- dummy_ord$values
    # dummy_ord <- dummy_ord$vectors
    # # Calculate the true volume (with eigen values)
    # true_vol <- volume.true(dummy_ord, dummy_eig)
    # # Calculate the volume without the eigen values
    # test_vol <- ellipse.volume(dummy_ord)
    # # test
    # expect_equal(
    #	true_vol, test_vol
    #	)

    # # Now testing with PCA (from cladistic data)
    # dummy_ord <- prcomp(dummy_cla)
    # dummy_eig <- dummy_ord$sdev^2 # Squared since the sdev is sqrt(eig)
    # dummy_ord <- dummy_ord$x
    # # Calculate the true volume (with eigen values)
    # true_vol <- volume.true(dummy_ord, dummy_eig)
    # # Calculate the volume without the eigen values
    # test_vol <- ellipse.volume(dummy_ord)
    # # test
    # expect_equal(
    #	true_vol, test_vol
    #	)

    # ## DOES NOT WORK FOR PCA!

    # # Now testing with PCA (from distance data)
    # dummy_ord <- prcomp(dummy_dis)
    # dummy_eig <- dummy_ord$sdev^2 # Squared since the sdev is sqrt(eig)
    # dummy_ord <- dummy_ord$x
    # # Calculate the true volume (with eigen values)
    # true_vol <- volume.true(dummy_ord, dummy_eig)
    # # Calculate the volume without the eigen values
    # test_vol <- ellipse.volume(dummy_ord)
    # # test
    # expect_equal(
    #	true_vol, test_vol
    #	)

    # ## DOES NOT WORK FOR PCA!

    # dummy_ord <- prcomp(dummy_dis, center=FALSE, scale=TRUE)
    # dummy_eig <- dummy_ord$sdev^2 # Squared since the sdev is sqrt(eig)
    # dummy_ord <- dummy_ord$x
    # # Calculate the true volume (with eigen values)
    # true_vol <- volume.true(dummy_ord, dummy_eig)
    # # Calculate the volume without the eigen values
    # test_vol <- ellipse.volume(dummy_ord)
    # # test
    # expect_equal(
    #	true_vol, test_vol
    #) ## Getting closer...

    # #And with svd?

    # svd(dummy_dis)
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
        c(10,9,7))
    expect_equal(
        get.ancestors(4, tree, full = FALSE),
        c(10))

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

    test <- round(span.tree.length(matrix), digit = 5)

    expect_equal(test, 15.40008)

    dist <- as.matrix(dist(matrix))

    expect_equal(test, round(span.tree.length(dist), digit = 5))

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