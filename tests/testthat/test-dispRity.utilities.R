#context("dispRity.utilities")

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
    expect_equal(length(test), 5)
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

    ## Between.groups check.subsets
    data(BeckLee_mat50)
    data(BeckLee_tree)
    group <- crown.stem(BeckLee_tree, inc.nodes = FALSE)
    group$all <- rownames(BeckLee_mat50)
    ## Get the disparity between groups
    disparity <- dispRity(boot.matrix(custom.subsets(BeckLee_mat50, group = group)), metric = group.dist, between.groups = TRUE)
    expect_null(check.subsets(names(disparity$subsets), disparity))
    expect_null(check.subsets(c("all", "crown"), disparity))
    expect_null(check.subsets(c(1,2), disparity))
    expect_null(check.subsets(c("all:crown"), disparity))
    expect_null(check.subsets(c("crown:all"), disparity))

    error <- capture_error(check.subsets(c(1,2,3,4), disparity))
    expect_equal(error[[1]], "Subset 4 not found.")

    error <- capture_error(check.subsets(c("ally", "crown"), disparity))
    expect_equal(error[[1]], "Subset ally not found.")

})

## make.dispRity
test_that("make.matrix", {
    test1 <- make.dispRity()

    expect_is(
        test1
        ,"dispRity")
    expect_is(
        test1$matrix
        ,"list")
    expect_equal(
        test1$matrix[[1]]
        ,NULL)
    expect_is(
        test1$call
        ,"list")
    expect_is(
        test1$subsets
        ,"list")
    expect_is(
        test1$tree
        ,"list")
    expect_equal(
        test1$tree[[1]]
        ,NULL)

    data_test <- matrix(rnorm(12), ncol = 3)

    test2 <- make.dispRity(data = data_test)

    expect_is(
        test2
        ,"dispRity")
    expect_is(
        test2$matrix
        ,"list")
    expect_is(
        test2$matrix[[1]]
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
    expect_is(
        test1$tree
        ,"list")
    expect_equal(
        test1$tree[[1]]
        ,NULL)

    test3 <- make.dispRity(data = list(data_test))

    expect_is(
        test3
        ,"dispRity")
    expect_is(
        test3$matrix
        ,"list")
    expect_is(
        test3$matrix[[1]]
        ,"matrix")
    expect_is(
        test3$call
        ,"list")
    expect_is(
        test3$subsets
        ,"list")
    expect_equal(
        length(unlist(test3))
        , 12)

    test4 <- make.dispRity(data = list(data_test, data_test))
    expect_equal(
        length(unlist(test4))
        , 24)

    test5 <- make.dispRity(data = data_test, tree = rtree(5))
    expect_is(
        test5$tree
        ,"multiPhylo")
    expect_is(
        test5$tree[[1]]
        ,"phylo")    
    test6 <- make.dispRity(data = data_test, tree = rmtree(5, 5))
    expect_is(
        test6$tree
        ,"multiPhylo")
    expect_is(
        test6$tree[[1]]
        ,"phylo")    
})

## fill.dispRity
test_that("fill.dispRity", {
    expect_error(
        fill.dispRity(make.dispRity())
        )
    expect_warning(test <- fill.dispRity(make.dispRity(data = matrix(rnorm(12), ncol = 3))))
    ## Warn is added dimnames

    expect_is(
        test
        ,"dispRity")
    expect_is(
        test$matrix
        ,"list")
    expect_is(
        test$matrix[[1]]
        ,"matrix")
    expect_is(
        test$call
        ,"list")
    expect_is(
        test$subsets
        ,"list")


    expect_equal(
        length(unlist(test$matrix[[1]]))
        , 12)
    expect_equal(
        test$call$dimensions
        , 1:ncol(test$matrix[[1]]))
    expect_equal(
        as.vector(test$subsets[[1]]$elements)
        , 1:nrow(test$matrix[[1]]))

    test <- make.dispRity(data = matrix(rnorm(12), ncol = 3))
    test$subsets <- c(list(), list())

    expect_warning(test <- fill.dispRity(test))
    expect_equal(
        as.vector(test$subsets[[1]]$elements)
        , 1:nrow(test$matrix[[1]]))

    ## Filling trees

    ## One tree one data
    tree <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    test <- fill.dispRity(make.dispRity(data = data, tree = tree))

    ## Basic works
    expect_is(test, "dispRity")
    expect_is(test$tree, "multiPhylo")
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 1)
    expect_equal(length(test$matrix), 1)

    ## Multiple tree one data
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    test <- fill.dispRity(make.dispRity(data = data, tree = tree))

    ## multiple trees works
    expect_is(test, "dispRity")
    expect_is(test$tree, "multiPhylo")
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 2)
    expect_equal(length(test$matrix), 1)

    ## One tree multiple data
    tree <- makeNodeLabel(rtree(5))
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    test <- fill.dispRity(make.dispRity(data = list(data, data), tree = tree))

    ## One tree multiple data works
    expect_is(test, "dispRity")
    expect_is(test$tree, "multiPhylo")
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 1)
    expect_equal(length(test$matrix), 2)

    ## multiple tree multiple data
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))

    ## multiple tree multiple data works
    test <- fill.dispRity(tree = tree, data = make.dispRity(data = list(data, data)))
    expect_is(test, "dispRity")
    expect_is(test$tree, "multiPhylo")
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 2)
    expect_equal(length(test$matrix), 2)

    ## Also works with just tips
    my_tree <- rtree(6)
    my_data <- matrix(0, ncol = 2, nrow = 6, dimnames = list(c(my_tree$tip.label)))
    test <- fill.dispRity(tree = my_tree, data = make.dispRity(my_data))
    expect_is(test, "dispRity")
    expect_is(test$tree, "multiPhylo")
    expect_is(test$tree[[1]], "phylo")
    expect_equal(length(test$tree), 1)
    expect_equal(length(test$matrix), 1)
})

## get.matrix
test_that("get.matrix", {
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
        get.matrix(matrix(rnorm(12), ncol = 3))
        )
    expect_true(
        all(get.matrix(dispRity_data) == BeckLee_mat50)
        )
    expect_equal(
        dim(get.matrix(dispRity_data, subsets = 2))
        , c(25, 48))
    expect_equal(
        rownames(get.matrix(dispRity_data, subsets = 2))
        , c("Rhombomylus","Gomphos","Mimotona","Soricidae","Solenodon","Eoryctes","Potamogalinae","Rhynchocyon","Procavia","Moeritherium","Dasypodidae","Bradypus","Myrmecophagidae","Dilambdogale","Widanelfarasia","Todralestes","unnamed_zalambdalestid","unnamed_cimolestid","Oxyclaenus","Protictis","Icaronycteris","Patriomanis","Cynocephalus","Pezosiren","Trichechus"))
    expect_equal(
        dim(get.matrix(dispRity_data, subsets = 2, rarefaction = 2, bootstrap = 52))
        , c(15, 48))
    expect_equal(
        rownames(get.matrix(dispRity_data, subsets = 2, rarefaction = 2, bootstrap = 52))
        , c("Eoryctes", "Rhynchocyon", "Pezosiren", "Potamogalinae", "Soricidae", "Myrmecophagidae", "Protictis", "Protictis", "unnamed_cimolestid", "Bradypus", "Mimotona", "Todralestes", "Moeritherium", "Dasypodidae", "Bradypus"))
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

    test <- get.subsets(bootstrapped_data, subsets = "66.76")
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

## get.disparity
test_that("get.disparity", {
    data(BeckLee_mat99) ; data(BeckLee_tree) 
    subsets_full <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous",time = 5, model = "acctran")
    bootstrapped_data <- boot.matrix(subsets_full, bootstraps = 10, rarefaction = c(3, 5))
    data <- dispRity(bootstrapped_data, c(sum,variances))

    expect_error(
        get.disparity(subsets_full)
        )

    expect_error(
        get.disparity(data, 1, rarefaction = 4, observed = FALSE)
        )

    expect_warning(
        test1 <- get.disparity(data, 1, rarefaction = 3, observed = TRUE)
        )
    expect_equal(test1,
        get.disparity(data, 1, observed = TRUE)
        )

    test <- get.disparity(data)
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
        ,4.09234)

    test <- get.disparity(data, observed = FALSE)
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

    test <- get.disparity(data, observed = FALSE, rarefaction = 5)
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

    test <- get.disparity(data, observed = FALSE, subsets = c(1,5))
    expect_is(
        test
        ,"list")
    expect_equal(
        length(test)
        ,2)
    expect_equal(
        names(test)
        ,names(data$subsets)[c(1,5)])

    ## Test whithout disparity but with distribution
    data <- dispRity(BeckLee_mat99, metric = centroids)
    expect_error(get.disparity(data, observed = FALSE))
    test <- get.disparity(data, observed = TRUE)
    expect_is(test, "list")
    expect_equal(length(test[[1]]), nrow(BeckLee_mat99))
})

## rescale.dispRity
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

## sort.dispRity
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

    sorted <- sort(data, decreasing = FALSE)
    expect_true(
        all(summary(sorted) == summary(data)[1:5,])
        )

    sorted <- sort(data, sort = c(1,3,4,5,2))
    expect_true(
        all(summary(sorted) == summary(data)[c(1,3,4,5,2),])
        )

    data2 <- dispRity(BeckLee_mat99, metric = mean)
    expect_error(sort(data2))
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

    ## Errors
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
    expect_warning(
        bs <- combine.subsets(boot.matrix(data_test2), c(1,2))
        )
    expect_error(
        combine.subsets(data_test2, (nrow(data_test2$matrix)+1))
        )
    expect_error(
        combine.subsets(data_test2, "a")
        )
    expect_error(
        combine.subsets(data_test2, c(1,2,3,4,5,6,7))
        )

    dummy_data1 <- dummy_data2 <- data_test1
    dummy_data1$call$bootstrap <- NULL
    test1 <- capture_warnings(garbage <- combine.subsets(dummy_data1, c(1,2)))
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

    error <- capture_error(expect_warning(combine.subsets(data_test1, c(1,1))))
    expect_equal(error[[1]], "subsets argument must not contain duplicates.")
})

## size.subsets
test_that("size.subsets works", {
    data(disparity)
    expect_equal(
        size.subsets(disparity)
        , c("90"=18, "80"=22, "70"=23, "60"=21, "50"=18, "40"=15, "30"=10))
})

## extinction.subsets
test_that("extinction.subsets works", {

    data(disparity)
    data(BeckLee_mat99)
    data(BeckLee_tree)

    ## Errors
    data <- dispRity(BeckLee_mat50, metric = mean)
    expect_error(extinction.subsets(data, 66))


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
    expect_error(extinction.subsets(disparity, 66, names = TRUE, as.list = TRUE, lag = 0))

    ## Normal behaviour
    expect_equal(extinction.subsets(disparity, 66, names = TRUE, as.list = TRUE), list("60" = c("70", "60")))
    expect_equal(extinction.subsets(disparity, 66, lag = 4), c(3:7))
    expect_warning(expect_equal(extinction.subsets(disparity, 66, lag = 12), c(3:7)))
    expect_equal(extinction.subsets(chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 6, inc.nodes = TRUE), 111.2592), c(1,2))
    expect_equal(extinction.subsets(chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 6, inc.nodes = TRUE), 111), c(1,2))
})

test_that("n.subset works", {
    data(disparity)
    expect_equal(n.subsets(disparity), 7)
})

test_that("tree utilities works", {
    data(BeckLee_mat99)
    disparity <- dispRity(BeckLee_mat99, metric = mean)
    data(BeckLee_tree)
    expect_null(disparity$tree[[1]])
    disparitree <- add.tree(tree = BeckLee_tree, data = disparity)
    expect_is(disparitree$tree, "multiPhylo")
    disparitree <- add.tree(tree = BeckLee_tree, data = disparity)
    expect_is(disparitree$tree, "multiPhylo")    
    disparitree2 <- add.tree(tree = c(BeckLee_tree, BeckLee_tree, BeckLee_tree), data = disparity)
    expect_is(disparitree2$tree, "multiPhylo")
    tree <- get.tree(disparitree)
    expect_is(tree, "phylo")
    tree <- get.tree(disparitree2)
    expect_is(tree, "multiPhylo")
    expect_null(remove.tree(disparitree)$tree[[1]])
    expect_null(remove.tree(disparitree2)$tree[[1]])

    ## Remove and replace trees
    disparity <- dispRity(BeckLee_mat99, metric = mean)
    expect_null(disparity$tree[[1]])
    disparitree <- add.tree(tree = BeckLee_tree, data = disparity)
    expect_equal(length(disparitree$tree), 1)
    disparitree <- add.tree(tree = BeckLee_tree, data = disparitree)
    expect_equal(length(disparitree$tree), 2)
    disparitree <- add.tree(tree = BeckLee_tree, data = disparitree, replace = TRUE)
    expect_equal(length(disparitree$tree), 1)
})

test_that("name.subsets(dispRity)", {
    data(disparity)
    expect_equal(name.subsets(disparity), names(disparity$subsets))
    expect_warning(test <- dispRity(matrix(rnorm(25), 5, 5), metric = mean))
    expect_null(name.subsets(test)) 
})

test_that("get.tree with subsets", {

    ## Testing detect edges and get new tree
    set.seed(1)
    simple_tree <- rtree(5)
    simple_tree$edge.length <- rep(1, Nedge(simple_tree))
    simple_tree$root.time <- 4
    simple_tree$node.label <- letters[1:4]
    plot(simple_tree, show.tip.label = FALSE); axisPhylo()
    nodelabels()
    nodelabels(simple_tree$node.label, adj = -1, col = "blue")
    edgelabels()
    tiplabels()
    tiplabels(simple_tree$tip.label, adj = -1, col = "blue")
    abline(v = c(0, 1, 2, 3, 3.8), col = "grey", lty = 2)
    dev.new()
    tree <- simple_tree

    ## Detect edges
    expect_equal(sort(detect.edges(tree, c(5, 3, 2, 8), to.root = FALSE)), sort(c(6, 5, 4, 3, 8)))
    expect_equal(sort(detect.edges(tree, c(5, 3, 2, 8), to.root = TRUE)), sort(c(6, 5, 4, 3, 8, 2)))
    expect_equal(sort(detect.edges(tree, c(9, 6, 1), to.root = FALSE)), sort(c(5,3,2,1)))
    expect_equal(sort(detect.edges(tree, c(9, 6, 1), to.root = TRUE)), sort(c(5,3,2,1)))

    ## Get new tree
    test <- get.new.tree(tree = tree, elements = c(5, 3, 2, 8), to.root = TRUE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 3)
    expect_equal(Nnode(test), 4)
    expect_equal(test$tip.label, c("t1", "t3", "t5"))
    expect_equal(test$node.label, c("a", "b", "c", "d"))
    # plot(test) ; nodelabels(test$node.label)

    test <- get.new.tree(tree = tree, elements = c(5, 3, 2, 8), to.root = FALSE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 3)
    expect_equal(Nnode(test), 3)    
    expect_equal(test$tip.label, c("t1", "t3", "t5"))
    expect_equal(test$node.label, c("b", "c", "d"))
    # plot(test) ; nodelabels(test$node.label)

    test <- get.new.tree(tree = tree, elements = c(5, 9, 2, 8), to.root = FALSE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 3)
    expect_equal(Nnode(test), 2)    
    expect_equal(test$tip.label, c("t1", "d", "t5"))
    expect_equal(test$node.label, c("b", "c"))
    # plot(test) ; nodelabels(test$node.label)

    test <- get.new.tree(tree = tree, elements = c(9, 6, 1), to.root = FALSE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 2)
    expect_equal(Nnode(test), 3)    
    expect_equal(test$tip.label, c("t2", "d"))
    expect_equal(test$node.label, c("a", "b", "c"))

    test <- get.new.tree(tree = tree, elements = c(9, 6, 1), to.root = TRUE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 2)
    expect_equal(Nnode(test), 3)    
    expect_equal(test$tip.label, c("t2", "d"))
    expect_equal(test$node.label, c("a", "b", "c"))

    test <- get.new.tree(tree = tree, elements = c(4,3), to.root = FALSE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 2)
    expect_equal(Nnode(test), 1)    
    expect_equal(test$tip.label, c("t3", "t4"))
    expect_equal(test$node.label, c("d"))

    test <- get.new.tree(tree = tree, elements = c(4,3), to.root = TRUE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 2)
    expect_equal(Nnode(test), 4)
    expect_equal(test$tip.label, c("t3", "t4"))
    expect_equal(test$node.label, c("a", "b", "c", "d"))

    ## Big test
    set.seed(2)
    big_tree <- rtree(20)
    big_tree$node.label <- letters[1:19]
    big_tree$edge.length <- rep(1, Nedge(big_tree))
    big_tree$root.time <- max(tree.age(big_tree)$ages)
    plot(big_tree, show.tip.label = FALSE); axisPhylo()
    nodelabels(cex = 0.8)
    nodelabels(big_tree$node.label, adj = -3, col = "blue", cex = 0.8)
    edgelabels(cex = 0.8)
    tiplabels(cex = 0.8)
    tiplabels(big_tree$tip.label, adj = -2, col = "blue", cex = 0.8)

    test <- get.new.tree(tree = big_tree, elements = c(6, 32, 28, 15, 35), to.root = FALSE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 4)
    expect_equal(Nnode(test), 10)
    expect_equal(test$tip.label, c("t6", "l", "o", "t12"))
    expect_equal(test$node.label, c("b", "c", "d", "e", "f", "h", "i", "j", "m", "n"))

    set.seed(1) ; elements <- sample(1:39, 10)
    test <- get.new.tree(tree = big_tree, elements = elements, to.root = FALSE)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 5)
    expect_equal(Nnode(test), 16)
    expect_equal(test$tip.label, c("t16", "t7", "t15", "t5", "t17"))
    expect_equal(test$node.label, c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "l", "m", "n", "o", "p", "q"))

    ## Basic subsets, return the tree per subsets
    set.seed(1)
    simple_tree <- rtree(10)
    simple_tree$edge.length <- rep(1, Nedge(simple_tree))
    simple_tree$node.label <- letters[1:9]
    matrix_dumb <- matrix(1, ncol = 1, nrow = 19, dimnames = list(c(simple_tree$tip.label, simple_tree$node.label)))
    plot(simple_tree)
    nodelabels(simple_tree$node.label)

    ## Groups
    groups <- list("clade1" = c("t3", "t2", "t7"),
                   "clade2" = c("c", "f", "t4"),
                   "clade3" = c("t10", "a", "t7"),
                   "clade4" = simple_tree$tip.label)

    data <- custom.subsets(data = matrix_dumb, tree = simple_tree, group = groups)

    ## Just get trees
    test_norm <- get.tree(data)
    expect_is(test_norm, "phylo")
    expect_equal(test_norm$tip.label, simple_tree$tip.label)

    ## Get subsets
    test <- get.tree(data, subsets = TRUE)
    expect_is(test, "list")
    expect_equal(length(test), 4)
    expect_equal(test$clade1$elements$tip.label, c("t7", "t2", "t3"))
    expect_equal(test$clade2$elements$tip.label, c("c", "f", "t4"))
    expect_equal(test$clade3$elements$tip.label, c("t7", "t10"))
    expect_equal(test$clade4$elements$tip.label, simple_tree$tip.label)

    ## Get subsets
    test <- get.tree(data, subsets = c(1,2))
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(names(test), c("clade1", "clade2"))

    ## Groups and bootstraps
    data <- boot.matrix(data, 3)
    test <- get.tree(data, subsets = TRUE)
    expect_is(test, "list")
    expect_equal(length(test), 4)
    expect_equal(test$clade1$elements$tip.label, c("t7", "t2", "t3"))
    expect_equal(test$clade2$elements$tip.label, c("c", "f", "t4"))
    expect_equal(test$clade3$elements$tip.label, c("t7", "t10"))
    expect_equal(test$clade4$elements$tip.label, simple_tree$tip.label)

    ## Time bin subsets
    set.seed(1)
    simple_tree <- rtree(5)
    simple_tree$edge.length <- rep(1, Nedge(simple_tree))
    simple_tree$root.time <- 4
    simple_tree$node.label <- letters[1:4]
    plot(simple_tree, show.tip.label = FALSE); axisPhylo()
    nodelabels()
    nodelabels(simple_tree$node.label, adj = -1, col = "blue")
    edgelabels()
    tiplabels()
    tiplabels(simple_tree$tip.label, adj = -1, col = "blue")
    abline(v = c(0, 1, 2, 3, 3.5), col = "grey", lty = 2)
    tree <- simple_tree
    matrix_dumb <- matrix(1, ncol = 1, nrow = 9, dimnames = list(c(simple_tree$tip.label, simple_tree$node.label)))
    data <- matrix_dumb
    tree <- simple_tree
    method = "discrete"
    time = c(0.5, 1, 2, 3, 3.5)
    data_bins <- chrono.subsets(matrix_dumb, tree = simple_tree, method = "discrete", time = time, inc.nodes = TRUE)



# chrono.subset.tree <- function(subset_n, data, to.root) {

#     slice.type <- data$call$subsets[[1]]
#     age <- as.numeric(strsplit(names(data$subsets[subset_n]), split = " - ")[[1]])

#     ## Time bin/slice behaviour
#     if(slice.type %in% c("discrete", "continuous")) {
#         if(to.root) {

#             slice.tree(data$tree[[1]], age = age[2], model = "acctran")

#             ## Maybe test this one?
#             # paleotree::timeSliceTree

#             slice.tree(tree, age, model, FAD, LAD)

#             ## cut the trees to the upper bound
#             return(output)
#         } else {
#             if(!to.root && slice.type == "discrete") {
#                 ## cut the trees to the upper and lower bounds
#                 return(output)




#             }
#         }
#     }

#     ## Out
#     return(output)

# }


# #     ## BUG when using a matrix with just one column! (empty subsets)

#     trees <- get.tree(data_bins, subsets = TRUE)

# #     dev.new()
# #     par(mfrow = c(2,2))
# #     plot(trees[[1]]$elements[[1]]) ; axisPhylo()
# #     plot(trees[[2]]$elements[[1]]) ; axisPhylo()
# #     plot(trees[[3]]$elements[[1]]) ; axisPhylo()
# #     plot(trees[[4]]$elements[[1]]) ; axisPhylo()

# tree <- trees[[1]]$elements[[1]]
# bin <- names(trees)[[1]]



    # test <- dispRity(times, metric = edge.length.tree, to.root = FALSE)
    # summary(test)

    # matrix <- times$matrix[[1]][times$subsets[[3]]$elements,, drop = FALSE]
    # matrix <- times$matrix[[1]][times$subsets[[4]]$elements,, drop = FALSE]

    # expect_equal(unique(c(summary(test)$obs)), 2)
    ## This should output the amount of branch length contained in each bin correctly


    ## Time bin subsets + multiPhylo


    ## Basic subsets + multiphylo
    
    ## Subsets with BS and multiphylo

})