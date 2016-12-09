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
        test1$series
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
        test2$series
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
        test$series
        ,"list")


    expect_equal(
        length(unlist(test$matrix))
        , 12)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        as.vector(test$series[[1]]$elements)
        , 1:nrow(test$matrix))
})


## matrix.dispRity
test_that("matrix.dispRity", {
    ## Load the Beck & Lee 2014 data
    data(BeckLee_mat50)

    ## Calculating the disparity from a customised series
    ## Generating the series
    factors <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_series <- cust.series(BeckLee_mat50, factors)
    ## Bootstrapping and rarefying the data
    set.seed(1)
    dispRity_data <- boot.matrix(customised_series, bootstraps = 100,rarefaction = c(15, 10))


    expect_error(
        matrix.dispRity(matrix(rnorm(12), ncol = 3))
        )
    expect_true(
        all(matrix.dispRity(dispRity_data) == BeckLee_mat50)
        )
    expect_equal(
        dim(matrix.dispRity(dispRity_data, series = 2))
        , c(25, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, series = 2))
        , c("Rhombomylus","Gomphos","Mimotona","Soricidae","Solenodon","Eoryctes","Potamogalinae","Rhynchocyon","Procavia","Moeritherium","Dasypodidae","Bradypus","Myrmecophagidae","Dilambdogale","Widanelfarasia","Todralestes","unnamed_zalambdalestid","unnamed_cimolestid","Oxyclaenus","Protictis","Icaronycteris","Patriomanis","Cynocephalus","Pezosiren","Trichechus"))
    expect_equal(
        dim(matrix.dispRity(dispRity_data, series = 2, rarefaction = 2, bootstrap = 52))
        , c(15, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, series = 2, rarefaction = 2, bootstrap = 52))
        , c("Patriomanis","Patriomanis","Procavia","Oxyclaenus","Pezosiren","Solenodon","Potamogalinae","Procavia","Gomphos","Cynocephalus","Solenodon","Todralestes","Gomphos","Widanelfarasia","Pezosiren"))
})


## get.series.dispRity
test_that("get.series.dispRity", {
    series_full <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(series_full, bootstraps = 10, rarefaction = c(3, 5))
    disparity_data <- dispRity(bootstrapped_data, variances)

    expect_error(
        get.series.dispRity(disparity_data)
        )
    expect_error(
        get.series.dispRity(disparity_data, matrix(1))
        )
    expect_error(
        get.series.dispRity(disparity_data, "blabalbal")
        )
    expect_error(
        get.series.dispRity(disparity_data, 1:10)
        )

    test <- get.series.dispRity(series_full, series = c(1,2))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$series)
        ,2)
    expect_equal(
        names(test$series)
        ,names(series_full$series)[1:2])

    test <- get.series.dispRity(bootstrapped_data, series = "66.75552")
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$series)
        ,1)
    expect_equal(
        test$call$bootstrap[[1]]
        ,10)

    test <- get.series.dispRity(disparity_data, series = c(1:3))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,4)
    expect_equal(
        length(test$series)
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
    series_full <- time.series(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(series_full, bootstraps = 10, rarefaction = c(3, 5))
    data <- dispRity(bootstrapped_data, c(sum,variances))

    expect_error(
        extract.dispRity(series_full)
        )
    test <- extract.dispRity(data)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$series))
    expect_equal(
        names(test)
        ,names(data$series))
    expect_equal(
        round(test[[5]], digit = 5)
        ,4.05457)

    test <- extract.dispRity(data, observed = FALSE)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$series))
    expect_equal(
        names(test)
        ,names(data$series))
    expect_equal(
        length(test[[5]])
        ,data$call$bootstrap[[1]])

    test <- extract.dispRity(data, observed = FALSE, rarefaction = 5)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$series))
    expect_equal(
        names(test)
        ,names(data$series))
    expect_null(
        test[[1]])

    test <- extract.dispRity(data, observed = FALSE, series = c(1,5))
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,2)
    expect_equal(
        names(test)
        ,names(data$series)[c(1,5)])
})