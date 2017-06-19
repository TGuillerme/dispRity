#TESTING boot.matrix

context("dispRity.metric")

#Testing the metrics
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
    expect_error(
        centroids(matrix, centroid = "a")
        )
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
    expect_message(
        convhull.surface(space.maker(22, 21, rnorm))
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
    expect_message(
        convhull.volume(space.maker(22, 21, rnorm))
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


test_that("hyper.volume metric", {

    #Create a small dummy matrix
    set.seed(1)
    matrix <- space.maker(5, 3, rnorm)

    #errors
    expect_error(
        hyper.volume(1)
        )
    expect_error(
        hyper.volume("a")
        )
    expect_error(
        hyper.volume(list(matrix))
        )

    #Works fine!
    expect_is(
        hyper.volume(matrix)
        ,"numeric")
    expect_equal(
        length(hyper.volume(matrix))
        ,1)
    expect_equal(
        round(hyper.volume(matrix), 3)
        ,10.83)
})