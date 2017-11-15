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

test_that("utilities internal: merge.two.subsetss", {
    data(disparity)
    data <- disparity

    ## Merging two first subsetss
    test <- merge.two.subsetss(1,2, data)

    expect_is(test, "dispRity")
    expect_equal(length(test), 4)
    expect_is(test$subsetss, "list")
    expect_equal(length(test$subsetss),length(data$subsetss)-1)
    expect_equal(names(test$subsetss)[1], paste(names(data$subsetss)[1:2], collapse = "-"))
})

test_that("utilities internal: check.subsetss", {
    data(disparity)
    data <- disparity

    ## Testing if subsetss work
    expect_error(
        check.subsetss(1:20, data)
        )
    expect_error(
        check.subsetss(8, data)
        )
    expect_error(
        check.subsetss(c(1,2,8), data)
        )
    expect_error(
        check.subsetss(c("8", "0"), data)
        )
    expect_error(
        check.subsetss(matrix(NA), data)
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
        test1$subsetss
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
        test2$subsetss
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
        test$subsetss
        ,"list")


    expect_equal(
        length(unlist(test$matrix))
        , 12)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        as.vector(test$subsetss[[1]]$elements)
        , 1:nrow(test$matrix))
})


## matrix.dispRity
test_that("matrix.dispRity", {
    ## Load the Beck & Lee 2014 data
    data(BeckLee_mat50)

    ## Calculating the disparity from a customised subsetss
    ## Generating the subsetss
    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsetss <- custom.subsetss(BeckLee_mat50, groups)
    ## Bootstrapping and rarefying the data
    set.seed(1)
    dispRity_data <- boot.matrix(customised_subsetss, bootstraps = 100,rarefaction = c(15, 10))


    expect_error(
        matrix.dispRity(matrix(rnorm(12), ncol = 3))
        )
    expect_true(
        all(matrix.dispRity(dispRity_data) == BeckLee_mat50)
        )
    expect_equal(
        dim(matrix.dispRity(dispRity_data, subsetss = 2))
        , c(25, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, subsetss = 2))
        , c("Rhombomylus","Gomphos","Mimotona","Soricidae","Solenodon","Eoryctes","Potamogalinae","Rhynchocyon","Procavia","Moeritherium","Dasypodidae","Bradypus","Myrmecophagidae","Dilambdogale","Widanelfarasia","Todralestes","unnamed_zalambdalestid","unnamed_cimolestid","Oxyclaenus","Protictis","Icaronycteris","Patriomanis","Cynocephalus","Pezosiren","Trichechus"))
    expect_equal(
        dim(matrix.dispRity(dispRity_data, subsetss = 2, rarefaction = 2, bootstrap = 52))
        , c(15, 48))
    expect_equal(
        rownames(matrix.dispRity(dispRity_data, subsetss = 2, rarefaction = 2, bootstrap = 52))
        , c("Patriomanis","Patriomanis","Procavia","Oxyclaenus","Pezosiren","Solenodon","Potamogalinae","Procavia","Gomphos","Cynocephalus","Solenodon","Todralestes","Gomphos","Widanelfarasia","Pezosiren"))
})


## get.subsetss
test_that("get.subsetss", {
    data(BeckLee_mat99)
    data(BeckLee_tree)
    subsetss_full <- time.subsetss(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsetss_full, bootstraps = 10, rarefaction = c(3, 5))
    disparity_data <- dispRity(bootstrapped_data, variances)

    expect_error(
        get.subsetss(disparity_data)
        )
    expect_error(
        get.subsetss(disparity_data, matrix(1))
        )
    expect_error(
        get.subsetss(disparity_data, "blabalbal")
        )
    expect_error(
        get.subsetss(disparity_data, 1:10)
        )
    expect_error(
        get.subsetss(disparity_data, 6)
        )

    test <- get.subsetss(subsetss_full, subsetss = c(1,2))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$subsetss)
        ,2)
    expect_equal(
        names(test$subsetss)
        ,names(subsetss_full$subsetss)[1:2])

    test <- get.subsetss(bootstrapped_data, subsetss = "66.75552")
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,3)
    expect_equal(
        length(test$subsetss)
        ,1)
    expect_equal(
        test$call$bootstrap[[1]]
        ,10)

    test <- get.subsetss(disparity_data, subsetss = c(1:3))
    expect_is(
        test
        ,"dispRity")
    expect_equal(
        length(test)
        ,4)
    expect_equal(
        length(test$subsetss)
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
    subsetss_full <- time.subsetss(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsetss_full, bootstraps = 10, rarefaction = c(3, 5))
    data <- dispRity(bootstrapped_data, c(sum,variances))

    expect_error(
        extract.dispRity(subsetss_full)
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
        ,length(data$subsetss))
    expect_equal(
        names(test)
        ,names(data$subsetss))
    expect_equal(
        round(test[[5]], digit = 5)
        ,4.05457)

    test <- extract.dispRity(data, observed = FALSE)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsetss))
    expect_equal(
        names(test)
        ,names(data$subsetss))
    expect_equal(
        length(test[[5]][[1]])
        ,data$call$bootstrap[[1]])

    test <- extract.dispRity(data, observed = FALSE, rarefaction = 5)
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,length(data$subsetss))
    expect_equal(
        names(test)
        ,names(data$subsetss))
    expect_null(
        test[[1]])

    test <- extract.dispRity(data, observed = FALSE, subsetss = c(1,5))
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,2)
    expect_equal(
        names(test)
        ,names(data$subsetss)[c(1,5)])


})

test_that("scale.dispRity", {
    data(BeckLee_mat50)
    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsetss <- custom.subsetss(BeckLee_mat50, groups)
    bootstrapped_data <- boot.matrix(customised_subsetss, bootstraps = 7, rarefaction = c(10, 25))
    data <- dispRity(bootstrapped_data, metric = c(sum, centroids))

    expect_error(
        scale.dispRity(bootstrapped_data)
        )
    expect_error(
        scale.dispRity(data, scale = "yes")
        )
    expect_error(
        scale.dispRity(data, center = "yes")
        )
    expect_error(
        scale.dispRity(data, center = c(1,2))
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
    subsetss <- time.subsetss(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", time = 5, model = "acctran")
    data <- dispRity(subsetss, metric = mean)

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



## merge.subsetss
test_that("merge.subsetss", {
    data(disparity)
    data_test1 <- disparity
    expect_warning(data_test2 <- custom.subsetss(matrix(rnorm(120), 40), group = list("a" = c(1:5), "b" = c(6:10), "c" = c(11:20), "d" = c(21:24), "e" = c(25:30), "f" = c(31:40))))
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
        merge.subsetss("data_test1", c(1,2))
        )
    expect_error(
        merge.subsetss(matrix(100,10), c(1,2))
        )
    expect_error(
        merge.subsetss(data_test2, "c(1,2)")
        )
    expect_error(
        merge.subsetss(data_test2, c(13,14))
        )
    expect_error(
        merge.subsetss(data_test2, c("a", "x"))
        )

    dummy_data1 <- dummy_data2 <- data_test1
    dummy_data1$call$bootstrap <- NULL
    test1 <- capture_warnings(garbage <-merge.subsetss(dummy_data1, c(1,2)))

    expect_equal(test1, "dummy_data1 contained disparity data that has been discarded in the output.")


    ## Warnings
    expect_warning(
        tests[[1]] <- merge.subsetss(data_test1, c(2,1,5))
        )
    expect_warning(
        tests[[2]] <- merge.subsetss(data_test1, c("90", "80", "50"))
        )
    expect_warning(
        tests[[3]] <- merge.subsetss(data_test1, 20)
        )

    ## Working fine!
    tests[[4]] <- merge.subsetss(data_test2, c(1,2,3))
    tests[[5]] <- merge.subsetss(data_test2, c("a", "b", "c"))
    tests[[6]] <- merge.subsetss(data_test2, 10)

    for(test in 1:length(tests)) {
        ## Class
        expect_is(tests[[test]]
            , "dispRity")
        ## Number of subsetss
        expect_equal(
            names(tests[[test]]$subsetss)
            ,expected_names[[test]])
        ## Number of elements per subsets
        expect_equal(
            as.vector(unlist(lapply(tests[[test]]$subsetss, lapply, length)))
            ,expected_elements[[test]])
    }
})


test_that("size.subsetss works", {
    data(disparity)
    expect_equal(
        size.subsetss(disparity)
        , c("90"=18, "80"=22, "70"=23, "60"=21, "50"=18, "40"=15, "30"=10))

})