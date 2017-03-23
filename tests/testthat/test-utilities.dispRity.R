context("dispRity.utilities")

## make.dispRity
test_that("make.matrix", {
    test1 <- make.dispRity()

    expect_is(
        test1
        ,"dispRity")
    expect_equal(
        test1$matrix
        ,NULL)
    expect_is(
        test1$call
        ,"list")
    expect_is(
        test1$subsamples
        ,"list")

    test2 <- make.dispRity(data = matrix(rnorm(12), ncol = 3))

    expect_is(
        test2
        ,"dispRity")
    expect_is(
        test2$matrix
        ,"matrix")
    expect_is(
        test2$call
        ,"list")
    expect_is(
        test2$subsamples
        ,"list")
    expect_equal(
        length(unlist(test2))
        , 12)
})


## fill.dispRity
test_that("fill.dispRity", {
    expect_error(
        fill.dispRity(make.dispRity())
        )
    test <- fill.dispRity(make.dispRity(data = matrix(rnorm(12), ncol = 3)))

    expect_is(
        test
        ,"dispRity")
    expect_is(
        test$matrix
        ,"matrix")
    expect_is(
        test$call
        ,"list")
    expect_is(
        test$subsamples
        ,"list")


    expect_equal(
        length(unlist(test$matrix))
        , 12)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        as.vector(test$subsamples[[1]]$elements)
        , 1:nrow(test$matrix))
})


## matrix.dispRity
test_that("matrix.dispRity", {
    ## Load the Beck & Lee 2014 data
    data(BeckLee_mat50)

    ## Calculating the disparity from a customised subsamples
    ## Generating the subsamples
    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsamples <- custom.subsamples(BeckLee_mat50, factors)
    ## Bootstrapping and rarefying the data
    set.seed(1)
    dispRity_data <- boot.matrix(customised_subsamples, bootstraps = 100,rarefaction = c(15, 10))


    expect_error(
        matrix.dispRity(matrix(rnorm(12), ncol = 3))
        )
    expect_true(
        all(matrix.dispRity(dispRity_data) == BeckLee_mat50)
        )
    expect_equal(
        dim(matrix.dispRity(dispRity_data, subsamples = 2))
        , c(25, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, subsamples = 2))
        , c("Rhombomylus","Gomphos","Mimotona","Soricidae","Solenodon","Eoryctes","Potamogalinae","Rhynchocyon","Procavia","Moeritherium","Dasypodidae","Bradypus","Myrmecophagidae","Dilambdogale","Widanelfarasia","Todralestes","unnamed_zalambdalestid","unnamed_cimolestid","Oxyclaenus","Protictis","Icaronycteris","Patriomanis","Cynocephalus","Pezosiren","Trichechus"))
    expect_equal(
        dim(matrix.dispRity(dispRity_data, subsamples = 2, rarefaction = 2, bootstrap = 52))
        , c(15, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, subsamples = 2, rarefaction = 2, bootstrap = 52))
        , c("Patriomanis","Patriomanis","Procavia","Oxyclaenus","Pezosiren","Solenodon","Potamogalinae","Procavia","Gomphos","Cynocephalus","Solenodon","Todralestes","Gomphos","Widanelfarasia","Pezosiren"))
})


## get.subsamples.dispRity
test_that("get.subsamples.dispRity", {
    subsamples_full <- time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsamples_full, bootstraps = 10, rarefaction = c(3, 5))
    disparity_data <- dispRity(bootstrapped_data, variances)

    expect_error(
        get.subsamples.dispRity(disparity_data)
        )
    expect_error(
        get.subsamples.dispRity(disparity_data, matrix(1))
        )
    expect_error(
        get.subsamples.dispRity(disparity_data, "blabalbal")
        )
    expect_error(
        get.subsamples.dispRity(disparity_data, 1:10)
        )

    test <- get.subsamples.dispRity(subsamples_full, subsamples = c(1,2))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$subsamples)
        ,2)
    expect_equal(
        names(test$subsamples)
        ,names(subsamples_full$subsamples)[1:2])

    test <- get.subsamples.dispRity(bootstrapped_data, subsamples = "66.75552")
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$subsamples)
        ,1)
    expect_equal(
        test$call$bootstrap[[1]]
        ,10)

    test <- get.subsamples.dispRity(disparity_data, subsamples = c(1:3))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,4)
    expect_equal(
        length(test$subsamples)
        ,3)
    expect_equal(
        length(test$disparity)
        ,3)
    expect_equal(
        as.character(test$call$disparity$metric[[1]])
        ,"variances")
})



## extract.dispRity
test_that("extract.dispRity", {
    data(BeckLee_mat99) ; data(BeckLee_tree) 
    subsamples_full <- time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsamples_full, bootstraps = 10, rarefaction = c(3, 5))
    data <- dispRity(bootstrapped_data, c(sum,variances))

    expect_error(
        extract.dispRity(subsamples_full)
        )
    test <- extract.dispRity(data)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsamples))
    expect_equal(
        names(test)
        ,names(data$subsamples))
    expect_equal(
        round(test[[5]], digit = 5)
        ,4.05457)

    test <- extract.dispRity(data, observed = FALSE)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsamples))
    expect_equal(
        names(test)
        ,names(data$subsamples))
    expect_equal(
        length(test[[5]][[1]])
        ,data$call$bootstrap[[1]])

    test <- extract.dispRity(data, observed = FALSE, rarefaction = 5)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsamples))
    expect_equal(
        names(test)
        ,names(data$subsamples))
    expect_null(
        test[[1]])

    test <- extract.dispRity(data, observed = FALSE, subsamples = c(1,5))
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,2)
    expect_equal(
        names(test)
        ,names(data$subsamples)[c(1,5)])
})

test_that("scale.dispRity", {
    data(BeckLee_mat50)
    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsamples <- custom.subsamples(BeckLee_mat50, factors)
    bootstrapped_data <- boot.matrix(customised_subsamples, bootstraps = 7, rarefaction = c(10, 25))
    data <- dispRity(bootstrapped_data, metric = c(sum, centroids))

    expect_error(
        scale.dispRity(data, scale = "yes")
        )
    expect_error(
        scale.dispRity(data, center = "yes")
        )
    expect_is(
        scale.dispRity(data, scale = TRUE)
        ,"dispRity")
    expect_is(
        scale.dispRity(data, scale = FALSE)
        ,"dispRity")
    expect_is(
        scale.dispRity(data, scale = TRUE, center = TRUE)
        ,"dispRity")

    base <- summary(data)
    scaled_down <- summary(scale.dispRity(data, scale = TRUE))
    scaled_up <- summary(scale.dispRity(data, scale = 0.1))
    expect_lt(
        scaled_down[1,3]
        ,base[1,3])
    expect_gt(
        scaled_up[1,3]
        ,base[1,3])
})

test_that("sort.dispRity", {
    data(BeckLee_mat99) ; data(BeckLee_tree) 
    subsamples <- time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", time = 5, model = "acctran")
    data <- dispRity(subsamples, metric = mean)

    expect_error(
        sort.dispRity("yes")
        )

    sorted <- sort(data, decreasing = TRUE)
    expect_true(
        all(summary(sorted) == summary(data)[5:1,])
        )

    sorted <- sort(data, sort = c(1,3,4,5,2))
    expect_true(
        all(summary(sorted) == summary(data)[c(1,3,4,5,2),])
        )
})