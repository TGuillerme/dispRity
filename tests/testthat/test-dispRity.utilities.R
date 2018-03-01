context("dispRity.utilities")

## utilities internals
test_that("utilities internal: extract.disparity.values", {
    data(disparity)
    data <- disparity
    test_con <- extract.disparity.values(1, data, rarefaction = FALSE, concatenate = TRUE)
    ## extract list of 100 (bs) numeric elements
    expect_is(test_con, "list")
    expect_is(test_con[[1]], "numeric")
    expect_equal(length(test_con[[1]]), data$call$bootstrap[[1]])

    test_uncon <- extract.disparity.values(1, data, rarefaction = FALSE, concatenate = FALSE)
    ## extract list of 100 (bs) numeric elements
    expect_is(test_uncon, "list")
    expect_is(test_uncon[[1]], "numeric")
    expect_equal(length(test_uncon[[1]]), 1)
    expect_equal(length(test_uncon), data$call$bootstrap[[1]])
})

test_that("utilities internal: clean.list", {
    dummy_list <- list("a" = NULL, "b" = 1, "c" = list(NULL, 1))
    test <- clean.list(dummy_list)
    expect_is(test, "list")
    expect_equal(length(test), length(dummy_list)-1)
})

test_that("utilities internal: recursive.sort", {
    expect_equal(recursive.sort(LETTERS[1:5], 5:1), rev(LETTERS[1:5]))
})

test_that("utilities internal: merge.two.subsets", {
    data(disparity)
    data <- disparity

    ## Merging two first subsets
    test <- merge.two.subsets(1,2, data)

    expect_is(test, "dispRity")
    expect_equal(length(test), 4)
    expect_is(test$subsets, "list")
    expect_equal(length(test$subsets),length(data$subsets)-1)
    expect_equal(names(test$subsets)[1], paste(names(data$subsets)[1:2], collapse = "-"))
})

test_that("utilities internal: check.subsets", {
    data(disparity)
    data <- disparity

    ## Testing if subsets work
    expect_error(
        check.subsets(1:20, data)
        )
    expect_error(
        check.subsets(8, data)
        )
    expect_error(
        check.subsets(c(1,2,8), data)
        )
    expect_error(
        check.subsets(c("8", "0"), data)
        )
    expect_error(
        check.subsets(matrix(NA), data)
        )
})


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
        test1$subsets
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
        test2$subsets
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
        test$subsets
        ,"list")


    expect_equal(
        length(unlist(test$matrix))
        , 12)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        as.vector(test$subsets[[1]]$elements)
        , 1:nrow(test$matrix))
})


## matrix.dispRity
test_that("matrix.dispRity", {
    ## Load the Beck & Lee 2014 data
    data(BeckLee_mat50)

    ## Calculating the disparity from a customised subsets
    ## Generating the subsets
    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsets <- custom.subsets(BeckLee_mat50, groups)
    ## Bootstrapping and rarefying the data
    set.seed(1)
    dispRity_data <- boot.matrix(customised_subsets, bootstraps = 100,rarefaction = c(15, 10))


    expect_error(
        matrix.dispRity(matrix(rnorm(12), ncol = 3))
        )
    expect_true(
        all(matrix.dispRity(dispRity_data) == BeckLee_mat50)
        )
    expect_equal(
        dim(matrix.dispRity(dispRity_data, subsets = 2))
        , c(25, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, subsets = 2))
        , c("Rhombomylus","Gomphos","Mimotona","Soricidae","Solenodon","Eoryctes","Potamogalinae","Rhynchocyon","Procavia","Moeritherium","Dasypodidae","Bradypus","Myrmecophagidae","Dilambdogale","Widanelfarasia","Todralestes","unnamed_zalambdalestid","unnamed_cimolestid","Oxyclaenus","Protictis","Icaronycteris","Patriomanis","Cynocephalus","Pezosiren","Trichechus"))
    expect_equal(
        dim(matrix.dispRity(dispRity_data, subsets = 2, rarefaction = 2, bootstrap = 52))
        , c(15, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, subsets = 2, rarefaction = 2, bootstrap = 52))
        , c("Patriomanis","Patriomanis","Procavia","Oxyclaenus","Pezosiren","Solenodon","Potamogalinae","Procavia","Gomphos","Cynocephalus","Solenodon","Todralestes","Gomphos","Widanelfarasia","Pezosiren"))
})


## get.subsets
test_that("get.subsets", {
    data(BeckLee_mat99)
    data(BeckLee_tree)
    subsets_full <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsets_full, bootstraps = 10, rarefaction = c(3, 5))
    disparity_data <- dispRity(bootstrapped_data, variances)

    expect_error(
        get.subsets(disparity_data)
        )
    expect_error(
        get.subsets(disparity_data, matrix(1))
        )
    expect_error(
        get.subsets(disparity_data, "blabalbal")
        )
    expect_error(
        get.subsets(disparity_data, 1:10)
        )
    expect_error(
        get.subsets(disparity_data, 6)
        )

    test <- get.subsets(subsets_full, subsets = c(1,2))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$subsets)
        ,2)
    expect_equal(
        names(test$subsets)
        ,names(subsets_full$subsets)[1:2])

    test <- get.subsets(bootstrapped_data, subsets = "66.75552")
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$subsets)
        ,1)
    expect_equal(
        test$call$bootstrap[[1]]
        ,10)

    test <- get.subsets(disparity_data, subsets = c(1:3))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,4)
    expect_equal(
        length(test$subsets)
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
    subsets_full <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsets_full, bootstraps = 10, rarefaction = c(3, 5))
    data <- dispRity(bootstrapped_data, c(sum,variances))

    expect_error(
        extract.dispRity(subsets_full)
        )

    expect_error(
        extract.dispRity(data, 1, rarefaction = 4, observed = FALSE)
        )


    test <- extract.dispRity(data)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsets))
    expect_equal(
        names(test)
        ,names(data$subsets))
    expect_equal(
        round(test[[5]], digit = 5)
        ,4.05457)

    test <- extract.dispRity(data, observed = FALSE)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsets))
    expect_equal(
        names(test)
        ,names(data$subsets))
    expect_equal(
        length(test[[5]][[1]])
        ,data$call$bootstrap[[1]])

    test <- extract.dispRity(data, observed = FALSE, rarefaction = 5)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsets))
    expect_equal(
        names(test)
        ,names(data$subsets))
    expect_null(
        test[[1]])

    test <- extract.dispRity(data, observed = FALSE, subsets = c(1,5))
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,2)
    expect_equal(
        names(test)
        ,names(data$subsets)[c(1,5)])


})

test_that("rescale.dispRity", {
    data(BeckLee_mat50)
    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsets <- custom.subsets(BeckLee_mat50, groups)
    bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 7, rarefaction = c(10, 25))
    data <- dispRity(bootstrapped_data, metric = c(sum, centroids))

    expect_error(
        rescale.dispRity(bootstrapped_data)
        )
    expect_error(
        rescale.dispRity(data, scale = "yes")
        )
    expect_error(
        rescale.dispRity(data, center = "yes")
        )
    expect_error(
        rescale.dispRity(data, center = c(1,2))
        )

    expect_is(
        rescale.dispRity(data, scale = TRUE)
        ,"dispRity")
    expect_is(
        rescale.dispRity(data, scale = FALSE)
        ,"dispRity")
    expect_is(
        rescale.dispRity(data, scale = TRUE, center = TRUE)
        ,"dispRity")

    base <- summary(data)
    scaled_down <- summary(rescale.dispRity(data, scale = TRUE))
    scaled_up <- summary(rescale.dispRity(data, scale = 0.1))
    expect_lt(
        scaled_down[1,3]
        ,base[1,3])
    expect_gt(
        scaled_up[1,3]
        ,base[1,3])
})

test_that("sort.dispRity", {
    data(BeckLee_mat99) ; data(BeckLee_tree) 
    subsets <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", time = 5, model = "acctran")
    data <- dispRity(subsets, metric = mean)

    expect_error(
        sort.dispRity("yes")
        )

    expect_error(
        sort.dispRity(data, sort = c(6,5,1,2,3))
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



## combine.subsets
test_that("combine.subsets", {
    data(disparity)
    data_test1 <- disparity
    expect_warning(data_test2 <- custom.subsets(matrix(rnorm(120), 40), group = list("a" = c(1:5), "b" = c(6:10), "c" = c(11:20), "d" = c(21:24), "e" = c(25:30), "f" = c(31:40))))
    tests <- list()
    expected_names <- list(c("70", "60", "90-80-50", "40", "30"),
                           c("70", "60", "80-90-50", "40", "30"),
                           c("90-80", "70", "60", "30-50-40"),
                           c("b-a-c", "d", "e", "f"),
                           c("b-a-c", "d", "e", "f"),
                           c("a-b", "c", "d-e", "f"))
    expected_elements <- list(c(23, 21, 49, 15, 10),
                              c(23, 21, 49, 15, 10),
                              c(34, 23, 21, 23),
                              c(20, 4, 6, 10),
                              c(20, 4, 6, 10),
                              c(10, 10, 11, 10))

    ## Errors
    expect_error(
        combine.subsets("data_test1", c(1,2))
        )
    expect_error(
        combine.subsets(matrix(100,10), c(1,2))
        )
    expect_error(
        combine.subsets(data_test2, "c(1,2)")
        )
    expect_error(
        combine.subsets(data_test2, c(13,14))
        )
    expect_error(
        combine.subsets(data_test2, c("a", "x"))
        )

    dummy_data1 <- dummy_data2 <- data_test1
    dummy_data1$call$bootstrap <- NULL
    test1 <- capture_warnings(garbage <-combine.subsets(dummy_data1, c(1,2)))

    expect_equal(test1, "dummy_data1 contained disparity data that has been discarded in the output.")


    ## Warnings
    expect_warning(
        tests[[1]] <- combine.subsets(data_test1, c(2,1,5))
        )
    expect_warning(
        tests[[2]] <- combine.subsets(data_test1, c("90", "80", "50"))
        )
    expect_warning(
        tests[[3]] <- combine.subsets(data_test1, 20)
        )

    ## Working fine!
    tests[[4]] <- combine.subsets(data_test2, c(1,2,3))
    tests[[5]] <- combine.subsets(data_test2, c("a", "b", "c"))
    tests[[6]] <- combine.subsets(data_test2, 10)

    for(test in 1:length(tests)) {
        ## Class
        expect_is(tests[[test]]
            , "dispRity")
        ## Number of subsets
        expect_equal(
            names(tests[[test]]$subsets)
            ,expected_names[[test]])
        ## Number of elements per subsets
        expect_equal(
            as.vector(unlist(lapply(tests[[test]]$subsets, lapply, length)))
            ,expected_elements[[test]])
    }
})


test_that("size.subsets works", {
    data(disparity)
    expect_equal(
        size.subsets(disparity)
        , c("90"=18, "80"=22, "70"=23, "60"=21, "50"=18, "40"=15, "30"=10))

})

test_that("extinction.subsets works", {

    data(disparity)
    data(BeckLee_mat99)
    data(BeckLee_tree)

    ## detect.bin.age (internal)
    data <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "discrete", time = c(100, 66, 40), inc.nodes = TRUE)
    expect_equal(detect.bin.age(data, 66, greater = TRUE), c("100 - 66" = TRUE, "66 - 40" = FALSE))
    expect_equal(detect.bin.age(data, 66, greater = FALSE), c("100 - 66" = FALSE, "66 - 40" = TRUE))
    expect_equal(detect.bin.age(data, 60, greater = TRUE), c("100 - 66" = TRUE, "66 - 40" = FALSE))

    ## Sanitising
    expect_error(extinction.subsets("disparity", 66, names = TRUE, as.list = TRUE))
    expect_error(extinction.subsets(disparity, c(1,2), names = TRUE, as.list = TRUE))
    expect_error(extinction.subsets(disparity, 66, names = "TRUE", as.list = TRUE))
    expect_error(extinction.subsets(disparity, 66, names = TRUE, as.list = "TRUE"))
    expect_error(extinction.subsets(disparity, 91, names = TRUE, as.list = TRUE))

    ## Normal behaviour
    expect_equal(extinction.subsets(disparity, 66, names = TRUE, as.list = TRUE), list("60" = c("70", "60")))
    expect_equal(extinction.subsets(disparity, 66, lag = 4), c(3:7))
    expect_warning(expect_equal(extinction.subsets(disparity, 66, lag = 12), c(3:7)))
    expect_equal(extinction.subsets(chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 6, inc.nodes = TRUE), 111.2592), c(1,2))
    expect_equal(extinction.subsets(chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 6, inc.nodes = TRUE), 111), c(1,2))
})
