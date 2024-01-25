## TESTING chrono.subsets

#context("chrono.subsets")

data(BeckLee_tree)
data(BeckLee_mat50)
data(BeckLee_ages)
data(BeckLee_mat99)

# load("test_data.rda")
tree <- BeckLee_tree
data <- BeckLee_mat50
FADLAD <- BeckLee_ages



# test_that("get.percent.age works", {
#     set.seed(42)
#     tree <- rtree(10)
#     tree$root.time <- 10
#     test <- get.percent.age(tree)
#     expect_is(test, "numeric")
#     expect_equal(test, 0.11)
# })


test_that("adjust.FADLAD works", {

    tree <- BeckLee_tree
    data <- BeckLee_mat50
    FADLAD <- BeckLee_ages


    ## Test FADLAD
    expect_equal(adjust.age(1, 1), 1)
    expect_equal(adjust.age(1, 12), 1)

    ages_tree <- adjust.FADLAD(FADLAD, tree, data)

    ## Class is list
    expect_is(
        ages_tree
        ,"list")
    ## Length is 2 (FAD and LAD)
    expect_equal(
        names(ages_tree)
        ,c("FAD", "LAD"))
    ## Length each is 50*2
    expect_equal(
        as.vector(unlist(lapply(ages_tree, dim)))
        ,c(50,2,50,2))
    ## Values match the FADLAD table
    for(taxon in 1:nrow(FADLAD)) {
        match_taxon <- match(rownames(FADLAD)[taxon], ages_tree$FAD$elements)
        expect_equal(
            c(ages_tree$FAD$ages[match_taxon], ages_tree$LAD$ages[match_taxon])
            , as.numeric(FADLAD[taxon,]))
    }
})

## chrono.subsets.discrete
time = c(120, 80, 40)
model = NULL
inc.nodes = FALSE
verbose = FALSE

time_subsets <- chrono.subsets.discrete(data, tree, time, model = NULL, FADLAD, inc.nodes, verbose)

## Test
test_that("chrono.subsets.discrete works properly without nodes", {
    # Test get.interval
    # expect_equal(
    #     as.vector(unlist(get.interval(1, time, adjust.FADLAD(FADLAD, tree, data), inc.nodes = FALSE, verbose = FALSE)))
    #     , c(5,4,6,8,43,10,11,42))

    ## class is list
    expect_is(
        time_subsets, "list"
        )
    ## length list is 2
    expect_equal(
        length(time_subsets)
        , 2)
    ## elements per subsets
    subsets_1 <- c("Daulestes","Bulaklestes","Uchkudukodon","Asioryctes","unnamed_cimolestid","Kulbeckia","Zhangolestes","unnamed_zalambdalestid")
    subsets_2 <- c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Oxyclaenus",
        "Protungulatum","Oxyprimus","Todralestes","Pezosiren","Tribosphenomys","Paramys","Rhombomylus","Gomphos","Mimotona","Purgatorius",
        "Plesiadapis","Notharctus","Protictis","Vulpavus","Miacis","Icaronycteris","Eoryctes")
    expect_equal(
        rownames(data[time_subsets[[1]]$elements,])
        , subsets_1)
    expect_equal(
        rownames(data[time_subsets[[2]]$elements,])
        , subsets_2)

    expect_message(chrono.subsets.discrete(data, tree, time, model = NULL, FADLAD, inc.nodes, verbose = TRUE))
})

## With nodes
inc.nodes = TRUE
data <- BeckLee_mat99

time_subsets <- chrono.subsets.discrete(data, tree, time, model = NULL, FADLAD, inc.nodes, verbose = FALSE)

## Test
test_that("chrono.subsets.discrete works properly with nodes", {
    ## class is list
    expect_is(
        time_subsets
        , "list")
    ## length list is 2
    expect_equal(
        length(time_subsets)
        , 2)
    ## elements per subsets
    expect_equal(
        length(time_subsets[[1]]$elements)
        , 32)
    expect_equal(
        length(time_subsets[[2]]$elements)
        , 47)
})


## chrono.subsets.continuous
data <- BeckLee_mat99
time = c(120, 80, 40)
verbose = FALSE

## DELTRAN
time_subsets <- chrono.subsets.continuous(data, tree, time, model = "deltran", FADLAD, inc.nodes = NULL, verbose)

## Test
test_that("chrono.subsets.continuous works properly with deltran model", {
    
    ## class is list
    expect_is(
        time_subsets
        , "list")
    ## length list is 3
    expect_equal(
        length(time_subsets)
        , 3)
    ## elements per subsets
    subsets_1 <- c("n1","n5","n11", "Zhangolestes")
    # c(11), sort(c(51, 55, 61))
    subsets_2 <- c("n6","Asioryctes","n7","n8","n10","n15","n17","n19","n22","n25","n34","n39","n44","n47")
    # c(8), sort(c(56,57,58,60,65,67,69,72,75,84,89,94,97))
    subsets_3 <- c("n17","n23","n27","n28","n29","n31","n32","n39","n42","n44","n48","n49")
    # sort(c(67,73,77,78,79,81,82,89,92,94,98,99))
    expect_equal(
        sort(rownames(data[time_subsets[[1]]$elements,]))
        , sort(subsets_1))
    expect_equal(
        sort(rownames(data[time_subsets[[2]]$elements,]))
        , sort(subsets_2))
    expect_equal(
        sort(rownames(data[time_subsets[[3]]$elements,]))
        , sort(subsets_3))
})

## ACCTRAN
time_subsets <- chrono.subsets.continuous(data, tree, time, model = "acctran", FADLAD, inc.nodes = NULL, verbose)

## Test
test_that("chrono.subsets.continuous works properly with acctran model", {
    subsets_1 <- c("n2","n6","n8","n12","n16", "Zhangolestes")
    subsets_2 <- c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Leptictis","Oxyclaenus","n20","n23","n26","n29","n35","Cynocephalus","n40","Patriomanis","n45","Icaronycteris","n48")
    subsets_3 <- c("Leptictis","Dasypodidae","n24","Potamogalinae","Dilambdogale","Widanelfarasia","Rhynchocyon","Procavia","Moeritherium","Trichechus","Cynocephalus","Adapis","Patriomanis","Soricidae","Solenodon")
    expect_equal(
        sort(rownames(data[time_subsets[[1]]$elements,]))
        , sort(subsets_1))
    expect_equal(
        sort(rownames(data[time_subsets[[2]]$elements,]))
        , sort(subsets_2))
    expect_equal(
        sort(rownames(data[time_subsets[[3]]$elements,]))
        , sort(subsets_3))
})

test_that("Sanitizing works for chrono.subsets (wrapper)", {

    ## chrono.subsets
    data = BeckLee_mat99
    tree = BeckLee_tree
    method = "continuous"
    model = "acctran"
    inc.nodes = TRUE
    FADLAD = BeckLee_ages
    verbose = FALSE

    ## Data
    error <- capture_error(chrono.subsets(data = "A", tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "data must be of class matrix or data.frame or list.")
    error <- capture_error(chrono.subsets(data = 1, tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "data must be of class matrix or data.frame or list.")
    expect_warning(error <- capture_error(chrono.subsets(data = matrix(NA, nrow = 2, ncol = 3), tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE)))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")

    ## tree
    error <- capture_error(chrono.subsets(data, tree = "A", method, time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    error <- capture_error(chrono.subsets(data, tree = 1, method, time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    error <- capture_error(chrono.subsets(data, tree = rtree(5), method, time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")
    ## method
    error <- capture_error(chrono.subsets(data, tree, method = 1, time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "method must be of class character.")
    error <- capture_error(chrono.subsets(data, tree, method = "a", time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "method argument must be one of the following: discrete, d, continuous, c.")
    error <- capture_error(chrono.subsets(data, tree, method = c("c","d"), time, model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "method argument must be one of the following: discrete, d, continuous, c.")
    ## time
    error <- capture_error(chrono.subsets(data, tree, method, time = "time", model, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "time must be of class numeric or integer.")
    ## model
    error <- capture_error(chrono.subsets(data, tree, method, time, model = 3, inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "model argument must be one of the following: acctran, deltran, random, proximity, equal.split, gradual.split.")
    error <- capture_error(chrono.subsets(data, tree, method, time, model = c("acctran","deltran"), inc.nodes, FADLAD, verbose = FALSE))
    expect_equal(error[[1]], "model argument must be one of the following: acctran, deltran, random, proximity, equal.split, gradual.split.")
    ## FADlAD
    error <- capture_error(chrono.subsets(data, tree, method, time, model, inc.nodes, FADLAD = data.frame(nrow = 2, ncol = 3), verbose = FALSE))
    expect_equal(error[[1]], "data.frame(nrow = 2, ncol = 3) must be a data.frame with two columns being called respectively:\n\"FAD\" (First Apparition Datum) and \"LAD\" (Last Apparition Datum).")

    ## t0
    error <- capture_error(chrono.subsets(data, tree, method, time, model, inc.nodes, FADLAD = data.frame(nrow = 2, ncol = 3), verbose = FALSE, t0 = "a"))
    expect_equal(error[[1]], "t0 must be logical or a single numeric value.")
    error <- capture_error(chrono.subsets(data, tree, method, time, model, inc.nodes, verbose = FALSE, t0 = c(1,2)))
    expect_equal(error[[1]], "t0 must be logical or a single numeric value.")

    data(BeckLee_mat99)
    data(BeckLee_mat50)
    data(BeckLee_ages)
    data(BeckLee_tree)

    ## Method shortcuts
    continous_shortcut <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "c", time = 3, model = "acctran")
    discrete_shortcut <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "d", time = 3, inc.nodes = TRUE)

    expect_is(continous_shortcut, "dispRity")
    expect_is(discrete_shortcut, "dispRity")
    expect_equal(unname(continous_shortcut$call$subsets[1]), "continuous")
    expect_equal(unname(discrete_shortcut$call$subsets[1]), "discrete")
    
    ## Error when no phy and no/wrong FADLAD
    expect_error(chrono.subsets(BeckLee_mat99, method = "continuous", time = 3, model = "acctran"))
    expect_error(chrono.subsets(BeckLee_mat99, method = "discrete", time = 3, FADLAD = BeckLee_ages))
    ## Error when only one time slice
    expect_error(chrono.subsets(BeckLee_mat99, method = "discrete", time = 1, tree = BeckLee_tree))
    
    ## 10 and tmax works with all FADLADs
    FADLAD_tmp <- tree.age(BeckLee_tree)
    FADLAD_tmp <- FADLAD_tmp[-c(51:99),]
    FADLAD_tmp <- data.frame("FAD" = FADLAD_tmp[,1], "LAD" = FADLAD_tmp[,1], row.names = FADLAD_tmp[,2])
    test <- chrono.subsets(BeckLee_mat50, method = "discrete", time = 3, FADLAD = FADLAD_tmp)
    expect_is(test, "dispRity")

    ## Wrong t0
    expect_error(chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "c", time = 3, model = "acctran", t0 = 10000))

    ## Inc nodes with no tree
    expect_error(chrono.subsets(BeckLee_mat50, method = "discrete", time = 3, FADLAD = FADLAD_tmp, inc.nodes = TRUE))

    ## Tree doesn't match
    wrong_tree <- rtree(50)
    wrong_tree$root.time <- 100
    error <- capture_error(chrono.subsets(BeckLee_mat99, wrong_tree, method = "c", time = 3, model = "acctran", inc.nodes = FALSE))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")
    error <- capture_error(chrono.subsets(BeckLee_mat99, wrong_tree, method = "c", time = 3, model = "acctran", inc.nodes = TRUE))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")


    ## FADLAD is inverse
    FADLAD_tmp <- FADLAD_tmp[,c(2,1)]
    test <- chrono.subsets(BeckLee_mat50, BeckLee_tree, method = "discrete", time = 3, FADLAD = FADLAD_tmp)
    expect_is(test, "dispRity")

    ## FADLAD contains too many taxa
    FADLAD_tmp2 <- rbind(BeckLee_ages, data.frame("FAD" = 1, "LAD" = 2, row.names = "Bob"))
    test <- chrono.subsets(BeckLee_mat50, BeckLee_tree, method = "discrete", time = 3, FADLAD = FADLAD_tmp2)
    expect_is(test, "dispRity")

    ## Verbose works
    expect_message(chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "c", time = 3, model = "acctran", verbose = TRUE))

    ## Multiple trees with wrong tip labels
    multitrees <- list(BeckLee_tree, BeckLee_tree)
    multitrees[[1]]$tip.label[1] <- "bob"
    class(multitrees) <- "multiPhylo"
    expect_warning(error <- capture_error(chrono.subsets(BeckLee_mat99, multitrees, method = "c", time = 3, model = "deltran")))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")

    ## No phylogeny provided!
    error <- capture_error(chrono.subsets(data = BeckLee_mat99, method = "c", time = 3, model = "acctran", FADLAD = BeckLee_ages))
    expect_equal(error[[1]], "If no phylogeny is provided, method must be \"discrete\".")

    ## Wrong label match between tree and data
    data_wrong <- BeckLee_mat99
    rownames(data_wrong)[1] <- "wrong!"
    error <- capture_error(chrono.subsets(data = data_wrong, tree = BeckLee_tree, method = "c", time = 3, model = "acctran"))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")
})

test_that("Output format is correct", {

    ## chrono.subsets
    data = BeckLee_mat99
    tree = BeckLee_tree
    method = "continuous"
    model = "acctran"
    inc.nodes = TRUE
    FADLAD = BeckLee_ages
    verbose = FALSE

    out_test <- chrono.subsets(data, tree, method, time, model, inc.nodes, FADLAD)
    ## Class
    expect_is(
        out_test, "dispRity"
        )
    ## Length
    expect_equal(
        length(out_test)
        , 4)
    ## Names
    expect_equal(
        names(out_test)
        , c("matrix", "tree", "call", "subsets")
        )
})

test_that("Output format is correct", {
    ## chrono.subsets
    data = BeckLee_mat99
    tree = BeckLee_tree
    method = "continuous"
    model = "acctran"
    inc.nodes = TRUE
    FADLAD = BeckLee_ages
    verbose = FALSE

    output_continuous <- capture_message(test <- chrono.subsets(data, tree, method = "continuous", time, model, inc.nodes, FADLAD, verbose = TRUE))
    expect_equal(strsplit(as.character(output_continuous), split = ", : ")[[1]][2], "Creating 3 time samples through one tree:\n")

    output_discrete <- capture_message(test <- chrono.subsets(data, tree, method = "discrete", time, model, inc.nodes, FADLAD, verbose = TRUE))
    expect_equal(strsplit(as.character(output_continuous), split = ", : ")[[1]][2], "Creating 3 time samples through one tree:\n")
})

test_that("Example works", {
    data(BeckLee_tree) ; data(BeckLee_mat50) ; data(BeckLee_mat99) ; data(BeckLee_ages)
    ex1 <- chrono.subsets(data = BeckLee_mat50, tree = BeckLee_tree, method = "discrete", time = c(120, 80, 40), inc.nodes = FALSE, FADLAD = BeckLee_ages)
    expect_equal(
        length(ex1)
        , 4)
    expect_is(
        ex1$matrix[[1]]
        ,"matrix")
    expect_equal(
        dim(ex1$matrix[[1]])
        ,c(50,48))
    expect_equal(
        nrow(ex1$subsets[[1]]$elements)
        ,8)
    expect_equal(
        nrow(ex1$subsets[[2]]$elements)
        ,27)

    ex2 <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = c(120, 80, 40), inc.nodes = TRUE, FADLAD = BeckLee_ages)
    expect_equal(
        length(ex2)
        , 4)
    expect_is(
        ex2$matrix[[1]]
        ,"matrix")
    expect_equal(
        dim(ex2$matrix[[1]])
        ,c(99,97))
    expect_equal(
        nrow(ex2$subsets[[1]]$elements)
        ,32)
    expect_equal(
        nrow(ex2$subsets[[2]]$elements)
        ,47)

    ex3 <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = 5, FADLAD = BeckLee_ages)
    expect_equal(
        length(ex3)
        , 4)
    expect_is(
        ex3$matrix[[1]]
        ,"matrix")
    expect_equal(
        dim(ex3$matrix[[1]])
        ,c(99,97))
    expect_equal(
        nrow(ex3$subsets[[1]]$elements)
        ,3)
    expect_equal(
        nrow(ex3$subsets[[2]]$elements)
        ,15)
    expect_equal(
        nrow(ex3$subsets[[3]]$elements)
        ,23)
})

test_that("make.origin.subsets works (internal fun)", {
    test_out <- make.origin.subsets(matrix(rnorm(25), 5, 5))
    expect_is(test_out, "list")
    expect_equal(names(test_out), "origin")
    expect_equal(names(test_out[[1]]), "elements")
    expect_equal(dim(test_out[[1]][[1]]), c(5,1))
})

test_that("chrono.subsets works without tree", {

    FAD_LAD_data <- tree.age(BeckLee_tree)[1:50,]
    rownames(FAD_LAD_data) <- FAD_LAD_data[,2]
    colnames(FAD_LAD_data) <- c("FAD", "LAD")
    FAD_LAD_data[, 2] <- FAD_LAD_data[, 1]

    ## Missing the FADLAD argument
    expect_error(
        chrono.subsets(BeckLee_mat50, method = "discrete", time = 5)
        )

    no_tree <- chrono.subsets(BeckLee_mat50, method = "discrete", time = c(130, 90, 45, 0), FADLAD = FAD_LAD_data)
    with_tree <- chrono.subsets(BeckLee_mat50, method = "discrete", time = c(130, 90, 45, 0), tree = BeckLee_tree)

    ## Right object
    expect_is(no_tree, "dispRity")
    ## Right subsets
    expect_equal(
        names(no_tree$subsets)
        ,names(with_tree$subsets))
    ## Right subsets values
    for(sub in 1:3) {
        expect_true(
            all(sort(unlist(no_tree$subsets[[sub]])) == sort(unlist(with_tree$subsets[[sub]])))
            )
    }

    ## Tree is saved accordingly
    expect_null(no_tree$tree[[1]])
    expect_is(with_tree$tree[[1]], "phylo")
})

test_that("t0 works", {
    data <- BeckLee_mat99
    test <- chrono.subsets(data, tree, method = "continuous", model = "acctran", inc.nodes = TRUE, FADLAD = FADLAD, t0 = 100, time = 11)
    expect_is(test, "dispRity")
    expect_equal(names(test$subsets), as.character(rev(seq(from = 0, to = 100, by = 10))))
})

test_that("chrono.subsets works for empty subsets", {
    data <- BeckLee_mat50
    tree <- BeckLee_tree
    time <- c(145, 140, 139, 0)

    ## Discrete
    warnings <- capture_warnings(test <- chrono.subsets(data, tree, method = "discrete", time = c(145, 140, 139, 0)))
    expect_equal(warnings, c("The interval 145 - 140 is empty.", "The interval 140 - 139 is empty."))
    expect_equal(test$subsets[[1]][[1]][,1], NA)
    expect_equal(test$subsets[[2]][[1]][,1], NA)
    expect_equal(test$subsets[[3]][[1]][,1], c(5, 4, 6, 7, 8, 9, 1, 43, 2, 3, 10, 11, 42, 12, 13, 14, 15, 44, 17, 18, 36, 37, 38, 41, 32, 39, 40, 33, 34, 35, 49, 50, 24, 25, 26, 27, 28, 48, 16, 21, 22, 23, 47, 45, 19, 20, 46, 29, 30, 31))

    ## Continuous
    data <- BeckLee_mat99
    warnings <- capture_warnings(test <- chrono.subsets(data, tree, model = "acctran", method = "continuous", time = c(145, 140, 139, 0)))
    expect_equal(warnings, c("The slice 145 is empty.", "The slice 140 is empty."))
    expect_equal(test$subsets[[1]][[1]][,1], NA)
    expect_equal(test$subsets[[2]][[1]][,1], NA)
    expect_equal(test$subsets[[3]][[1]][,1], c(52,54))
    expect_equal(test$subsets[[4]][[1]][,1], c(36, 37, 38, 32, 33, 34, 50, 48, 29, 30))
})

test_that("probability models work", {
    data(BeckLee_mat99)
    data(BeckLee_ages)
    data(BeckLee_tree)
    
    test1 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(120, 100, 80, 60, 40 , 20, 0), model = "gradual.split", inc.nodes = TRUE, FADLAD = BeckLee_ages, verbose = FALSE, t0 = FALSE)
    test2 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(120, 100, 80, 60, 40 , 20, 0), model = "equal.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)

    expect_is(test1, "dispRity")
    expect_is(test1$subsets[[1]][[1]], "matrix")
    expect_equal(dim(test1$subsets[[1]][[1]]), c(6,3))
    expect_true(all(test1$subsets[[1]][[1]][,1:2] >= 1))
    expect_true(all(test1$subsets[[1]][[1]][,3] <= 1))

    expect_is(test2, "dispRity")
    expect_is(test2$subsets[[1]][[1]], "matrix")
    expect_equal(dim(test2$subsets[[1]][[1]]), c(6,3))
    expect_true(all(test2$subsets[[1]][[1]][,1:2] >= 1))
    expect_true(all(test2$subsets[[1]][[1]][,3] <= 1))
})

test_that("chrono.subsets detects distance matrices", {
    non_dist <- matrix(1:100, 10, 10)
    rownames(non_dist) <- letters[1:10]
    is_dist <- as.matrix(dist(non_dist))

    set.seed(1)
    tree <- rtree(10, tip.label = letters[1:10])
    tree$root.time <- max(tree.age(tree)$age)

    expect_warning(chrono.subsets(is_dist, method = "discrete", time = c(1, 0.5, 0), tree = tree))
    msg <- capture_warnings(chrono.subsets(is_dist, method = "discrete", time = c(1, 0.5, 0), tree = tree))
    expect_equal(msg, "chrono.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!")
})

test_that("do.cbind.fill and recursive.combine list works", {
        x  <- matrix(1, nrow = 2, ncol = 1)
        x2 <- matrix(1, nrow = 2, ncol = 2)
        y  <- matrix(2, nrow = 4, ncol = 1)
        expect_equal(dim(do.cbind.fill(x, x2)[[1]]), dim(cbind(x, x2)))
        expect_equal(dim(do.cbind.fill(x, y)[[1]]) , c(4,2))
        expect_equal(dim(do.cbind.fill(x2, y)[[1]]), c(4,3))
        expect_equal(do.cbind.fill(x, y)$elements, matrix(c(1,1,NA,NA,2,2,2,2), ncol = 2))
        expect_equal(do.cbind.fill(y, x)$elements, matrix(c(2,2,2,2,1,1,NA,NA), ncol = 2))

        ## Dummy test lists
        test1 <- list("A" = list("elements" = matrix(1, nrow = 1, ncol = 1)),
                      "B" = list("elements" = matrix(2, nrow = 2, ncol = 1)),
                      "C" = list("elements" = matrix(3, nrow = 3, ncol = 1)))

        test2 <- list("A" = list("elements" = matrix(4, nrow = 1, ncol = 1)),
                      "B" = list("elements" = matrix(5, nrow = 2, ncol = 1)),
                      "C" = list("elements" = matrix(6, nrow = 3, ncol = 1)))

        test3 <- list("A" = list("elements" = matrix(7, nrow = 2, ncol = 1)),
                      "B" = list("elements" = matrix(8, nrow = 2, ncol = 1)),
                      "C" = list("elements" = matrix(9, nrow = 4, ncol = 1)))


        ## Combine them
        testA <- recursive.combine.list(list(test1, test2))
        testB <- recursive.combine.list(list(test1, test2, test3))
        testC <- recursive.combine.list(list(testA, test1, test2, test3))
        expect_equal(unlist(lapply(testA, lapply, dim), use.names = FALSE), c(1,2, 2,2, 3,2))
        expect_equal(unlist(lapply(testB, lapply, dim), use.names = FALSE), c(2,3, 2,3, 4,3))
        expect_equal(unlist(lapply(testC, lapply, dim), use.names = FALSE), c(2,5, 2,5, 4,5))
})

test_that("chrono.subsets works with multiPhylo", {
    #Simulate some fossil ranges with simFossilRecord
    load("paleotree_test_data.rda")
    tree <- paleotree_data$tree
    data <- paleotree_data$data

    ## Test if it works normally
    expect_is(chrono.subsets(data, tree[[1]], method = "continuous", time = 3, model = "proximity"), "dispRity")
    expect_is(chrono.subsets(data, tree[[2]], method = "continuous", time = 3, model = "proximity"), "dispRity")

    ## Creating a couple of error message testing trees
    tree_wrong_label <- tree_wrong_roottime <- trees_no_root_time <- trees_wrong_tip <- tree_bkp <- tree
    tree_wrong_label[[1]]$node.label[1] <- "WRONG"
    tree_wrong_roottime[[1]]$root.time <- 81
    trees_no_root_time[[1]]$root.time <- NULL
    trees_wrong_tip[[2]] <- drop.tip(trees_wrong_tip[[2]], "t1")

    error <- capture_error(chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = trees_no_root_time))
    expect_equal(error$message, "The following tree(s) in trees_no_root_time 1 needs a $root.time element.")
    expect_warning(error <- capture_error(chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = trees_wrong_tip)))
    expect_equal(error$message, "trees_wrong_tip: wrong number of tips in the following tree(s): 2.")
    error <- capture_error(chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = tree_wrong_label))
    expect_equal(error$message, "Node WRONG not found in the data. Nodes cannot be trimmed automatically. You can try using the following to remove them\n  my_tree$node.labels <- NULL")
    warning <- capture_warning(test <- chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = tree_wrong_roottime, model = "acctran"))
    expect_equal(warning$message, "Differing root times in tree_wrong_roottime. The $root.time for all tree has been set to the maximum (oldest) root time: 81 by stretching the root edge.")

    ## Works with a multiPhylo object
    test <- chrono.subsets(data, tree, method = "continuous", time = 3, model = "proximity")

    expect_is(test, "dispRity")
    expect_equal(names(test), c("matrix", "tree", "call", "subsets"))
    expect_equal(names(test$subsets), c("9.31", "4.66", "0"))
    expect_equal(unique(unlist(lapply(test$subsets, names), use.names = FALSE)), "elements")
    expect_equal(unlist(lapply(test$subsets, lapply, dim), use.names = FALSE), c(3, 2, 5, 2, 10, 2))
    expect_equal(unique(c(test$subsets[[2]]$elements)), c(17, 22, 21, 26, NA, 2, 25, 27))

    ## Works with discrete
    test <- chrono.subsets(data, tree, method = "discrete", time = 3, inc.nodes = TRUE)
    expect_is(test, "dispRity")
    expect_equal(unlist(lapply(test$subsets, lapply, dim), use.names = FALSE), c(6, 2, 5, 2, 14, 2))

    ## Works with probabilities
    test <- chrono.subsets(data, tree, method = "continuous", time = 3, model = "gradual.split")
    expect_is(test, "dispRity")
    expect_equal(unlist(lapply(test$subsets, lapply, dim), use.names = FALSE), c(3, 6, 7, 6, 10, 6))

    ## The output saves the tree
    expect_is(test$tree, "multiPhylo")
    expect_equal(test$tree[[1]]$edge.length, tree[[1]]$edge.length)
    expect_equal(test$tree[[2]]$edge.length, tree[[2]]$edge.length)
})

test_that("chrono.subsets works with multiple matrices", {

    data <- list(BeckLee_mat50, BeckLee_mat50)
    data_wrong <- list(BeckLee_mat50, BeckLee_mat99)

    error <- capture_error(test <- chrono.subsets(data_wrong, tree = BeckLee_tree, method = "discrete", time = 4))
    expect_equal(error[[1]], "data must be matrix or a list of matrices with the same dimensions and unique row names.")
    expect_warning(test <- chrono.subsets(data, tree = BeckLee_tree, method = "discrete", time = 4))
    
    expect_is(test, "dispRity")
    expect_is(test$matrix, "list")
    expect_equal(length(test$matrix), 2)
    expect_equal(dim(test$matrix[[1]]), c(50, 48))

    load("bound_test_data.rda")
    trees <- bound_test_data$trees
    matrices <- bound_test_data$matrices

    ## Test if it works with multiple trees and with multiple matrices ok
    test <- chrono.subsets(matrices, tree = trees[[1]], time = 3, method = "continuous", model = "acctran", t0 = 5)
    expect_is(test, "dispRity")
    test_print <- capture_output(print(test))
    expect_equal(test_print, " ---- dispRity object ---- \n3 continuous (acctran) time subsets for 19 elements in 3 matrices with 1 phylogenetic tree\n    5, 2.5, 0.")
    test <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
    expect_is(test, "dispRity")
    test_print <- capture_output(print(test))
    expect_equal(test_print, " ---- dispRity object ---- \n3 continuous (acctran) time subsets for 19 elements in one matrix with 3 phylogenetic trees\n    5, 2.5, 0.")


    matrices_wrong1 <- matrices_wrong2 <- matrices
    rownames(matrices_wrong1[[2]])[1] <- "t2000" 
    rownames(matrices_wrong2[[3]])[11] <- "root" 


    ## Now warnings for multi dispRity
    # error <- capture_error(chrono.subsets(matrices_wrong1, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5))
    # expect_equal(error[[1]], "data must be matrix or a list of matrices with the same dimensions and unique row names.")
    warn <- capture_warning(chrono.subsets(matrices_wrong1, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5))
    expect_equal(warn[[1]], "The following elements are not present in all matrices: t2000, t1, t2000. The matrices will be treated as separate trait-spaces.")

    # error <- capture_error(chrono.subsets(matrices_wrong2, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5))
    # expect_equal(error[[1]], "data must be matrix or a list of matrices with the same dimensions and unique row names.")
    warn <- capture_warning(chrono.subsets(chrono.subsets(matrices_wrong2, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)))
    expect_equal(warn[[1]], "The following elements are not present in all matrices: root, root, n1. The matrices will be treated as separate trait-spaces.")


    ## Test working fine
    test <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)

    expect_is(test, "dispRity")
    expect_is(test$matrix, "list")
    expect_equal(length(test$matrix), 3)
    expect_is(test$matrix[[1]], "matrix")
    expect_equal(rownames(test$matrix[[1]]), rownames(matrices[[1]]))
    expect_is(test$subsets, "list")
    expect_equal(length(test$subsets), 3)
    expect_equal(dim(test$subsets$`5`$elements), c(7, 3))

    ## The output saves the tree
    expect_is(test$tree, "multiPhylo")
    expect_equal(test$tree[[1]]$edge.length, trees[[1]]$edge.length)
    expect_equal(test$tree[[2]]$edge.length, trees[[2]]$edge.length)
    expect_equal(test$tree[[3]]$edge.length, trees[[3]]$edge.length)
})

test_that("fast internal functions work", {

    tree <- read.tree(text = "(((((A:1,B:1):2,C:3):1,D:1):1,E:5):1,F:3);")
    tree$node.label <- as.character(paste0("n",seq(1:5)))
    tree$root.time <- 6
    edge_table <- tree$edge
    rownames(edge_table) <- paste("edge", 1:nrow(edge_table), sep = "")
    colnames(edge_table) <- c("parent", "child")
    tree$tip.label <- paste0("t", 1:Ntip(tree))
    tree$node.label <- paste0("n", (Ntip(tree)+1):(Ntip(tree)+Nnode(tree)))
    data <- matrix(NA, ncol = 1, nrow = 11)
    rownames(data) <- c(tree$tip.label, tree$node.label)
    # plot(tree)
    # nodelabels(tree$node.label)
    # edgelabels(rownames(edge_table))
    # axisPhylo()
    # slice <- 3
    # abline(v = tree$root.time - slice)
    # edge_table

    ## Couple of edge slices
    t1 <- test <- fast.slice.table(3.5, tree)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(4,4))
    expect_equal(test[1,], c(9, 0.5, 10, 0.5))
    expect_equal(test[2,], c(9, 0.5, 4, 0.5))
    expect_equal(test[3,], c(8, 1.5, 5, 3.5))
    expect_equal(test[4,], c(7, 2.5, 6, 0.5))

    t2 <- test <- fast.slice.table(1.5, tree)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(3,4))
    expect_equal(test[1,], c(10, 1.5, 11, 0.5))
    expect_equal(test[2,], c(10, 1.5, 3, 1.5))
    expect_equal(test[3,], c(8, 3.5, 5, 1.5))

    t3 <- test <- fast.slice.table(5.75, tree)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(2,4))
    expect_equal(test[1,], c(7, 0.25, 8, 0.75))
    expect_equal(test[2,], c(7, 0.25, 6, 2.75))

    ## Tip slices
    t4 <- test <- fast.slice.table(0, tree)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(4,4))
    expect_equal(test[1,], c(11, 1, 1, 0))
    expect_equal(test[2,], c(11, 1, 2, 0))
    expect_equal(test[3,], c(10, 3, 3, 0))
    expect_equal(test[4,], c(8, 5, 5, 0))

    ## Node slice
    t5 <- test <- fast.slice.table(3, tree)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(4,4))
    expect_equal(test[1,], c(9, 1, 10, 0))
    expect_equal(test[2,], c(9, 1, 4, 0))
    expect_equal(test[3,], c(8, 2, 5, 3))
    expect_equal(test[4,], c(7, 3, 6, 0))

    ## Root slice
    t6 <- test <- fast.slice.table(6, tree)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(2,4))
    expect_equal(test[1,], c(7, 0, 8, 1))
    expect_equal(test[2,], c(7, 0, 6, 3))

    ## Beyond the tips slice
    old_tree <- tree
    old_tree$root.time <- 8
    test <- fast.slice.table(0, old_tree)
    expect_null(test)

    ## Selecting the right model works
    expect_equal(select.table.tips(t1, "acctran"), c(10, 4, 5, 6))
    expect_equal(select.table.tips(t2, "acctran"), c(11, 3, 5))
    expect_equal(select.table.tips(t3, "acctran"), c(8, 6))
    expect_equal(select.table.tips(t4, "acctran"), c(1, 2, 3, 5))
    expect_equal(select.table.tips(t5, "acctran"), c(10, 4, 5, 6))
    expect_equal(select.table.tips(t6, "acctran"), c(8, 6))
    
    expect_equal(select.table.tips(t1, "deltran"), c(9, 8, 7))
    expect_equal(select.table.tips(t2, "deltran"), c(10, 8))
    expect_equal(select.table.tips(t3, "deltran"), c(7))
    expect_equal(select.table.tips(t4, "deltran"), c(11, 10, 8))
    expect_equal(select.table.tips(t5, "deltran"), c(9, 8, 7))
    expect_equal(select.table.tips(t6, "deltran"), c(7))
    
    set.seed(1)
    expect_equal(select.table.tips(t1, "random"), c(9, 4, 8, 7))
    expect_equal(select.table.tips(t2, "random"), c(11, 10, 8))
    expect_equal(select.table.tips(t3, "random"), c(7, 6))
    expect_equal(select.table.tips(t4, "random"), c(1, 11, 10, 8))
    expect_equal(select.table.tips(t5, "random"), c(9, 5, 6))
    expect_equal(select.table.tips(t6, "random"), c(8, 6))

    expect_equal(select.table.tips(t1, "proximity"), c(9, 8, 6))
    expect_equal(select.table.tips(t2, "proximity"), c(11, 10, 5))
    expect_equal(select.table.tips(t3, "proximity"), c(7))
    expect_equal(select.table.tips(t4, "proximity"), c(1, 2, 3, 5))
    expect_equal(select.table.tips(t5, "proximity"), c(10, 4, 8, 6))
    expect_equal(select.table.tips(t6, "proximity"), c(7))
    
    expect_equal(select.table.tips(t1, "equal.split"), cbind(t1[,c(1,3)], 0.5))
    expect_equal(select.table.tips(t2, "equal.split"), cbind(t2[,c(1,3)], 0.5))
    expect_equal(select.table.tips(t3, "equal.split"), cbind(t3[,c(1,3)], 0.5))
    expect_equal(select.table.tips(t4, "equal.split"), cbind(t4[,c(1,3)], 0.5))
    expect_equal(select.table.tips(t5, "equal.split"), cbind(t5[,c(1,3)], 0.5))
    expect_equal(select.table.tips(t6, "equal.split"), cbind(t6[,c(1,3)], 0.5))

    expect_equal(select.table.tips(t1, "gradual.split"), cbind(t1[,c(1,3)], 1-(t1[,2]/(t1[,2]+t1[,4]))))
    expect_equal(select.table.tips(t2, "gradual.split"), cbind(t2[,c(1,3)], 1-(t2[,2]/(t2[,2]+t2[,4]))))
    expect_equal(select.table.tips(t3, "gradual.split"), cbind(t3[,c(1,3)], 1-(t3[,2]/(t3[,2]+t3[,4]))))
    expect_equal(select.table.tips(t4, "gradual.split"), cbind(t4[,c(1,3)], 1-(t4[,2]/(t4[,2]+t4[,4]))))
    expect_equal(select.table.tips(t5, "gradual.split"), cbind(t5[,c(1,3)], 1-(t5[,2]/(t5[,2]+t5[,4]))))
    expect_equal(select.table.tips(t6, "gradual.split"), cbind(t6[,c(1,3)], 1-(t6[,2]/(t6[,2]+t6[,4]))))

    ## get.time.slice works
    test1 <- get.time.slice(time = 3.5, tree, model = "deltran", verbose = FALSE)
    expect_is(test1, "list")
    expect_equal(names(test1), "elements")
    expect_equal(test1[[1]], matrix(c(9, 8 ,7)))
    test2 <- get.time.slice(time = 3.5, tree, model = "gradual.split", verbose = FALSE)
    expect_is(test2, "list")
    expect_equal(names(test2), "elements")
    expect_equal(test2[[1]], cbind(t1[,c(1,3)], 1-(t1[,2]/(t1[,2]+t1[,4]))))

    ## add.FADLAD works
    FADLAD <- matrix(c(3, 1.5, 2, 0, 5, 4), 3, 2, byrow = TRUE, dimnames = list(c("t4", "t3", "t6"), c("FAD", "LAD")))
    test1 <- get.time.slice(time = 2, tree, model = "proximity", verbose = FALSE)
    test2 <- get.time.slice(time = 2, tree, model = "equal.split", verbose = FALSE)


    res1 <- add.FADLAD(test1, 2, FADLAD, rownames(data))
    expect_is(res1, "list")
    expect_equal(names(res1), "elements")
    expect_equal(res1[[1]], matrix(c(10, 5 , 3, 4)))

    res2 <- add.FADLAD(test2, 2, FADLAD, rownames(data))
    expect_is(res2, "list")
    expect_equal(names(res2), "elements")
    expect_equal(res2[[1]][,1], c(10,3,8,4))
    expect_equal(res2[[1]][,3], c(.5,1,.5,1))

    ## FADLAD works with nodes
    FADLAD <- rbind(FADLAD, "n10" = c(3, 1))
    res1 <- add.FADLAD(test1, 2, FADLAD, rownames(data))
    expect_is(res1, "list")
    expect_equal(names(res1), "elements")
    expect_equal(res1[[1]], matrix(c(10, 5 , 3, 4)))

    res2 <- add.FADLAD(test2, 2, FADLAD, rownames(data))
    expect_is(res2, "list")
    expect_equal(names(res2), "elements")
    expect_equal(res2[[1]][,1], c(10,3,8,4))
    expect_equal(res2[[1]][,2], c(10,3,5,4))
    expect_equal(res2[[1]][,3], c(1,1,.5,1))
})

test_that("infinite loop blocker for get.percent.age", {
   ## Testing data
    matrix <- do.call(rbind, list(matrix(1, 5, 5), matrix(2, 3, 5), matrix(3, 4, 5)))
    rownames(matrix) <- paste0("t", 1:12)
    test_tree <- stree(12, type = "right")
    test_tree$edge.length <- rep(1, Nedge(test_tree))
    test_tree$root.time <- 12

    ## custom subsets
    error <- capture_error(chrono.subsets(matrix, test_tree, method = "continuous", time = 5, model = "acctran"))
    expect_equal(error[[1]], "Impossible to find a starting point to slice the tree. This can happen if the tree has no branch length or has a \"ladder\" structure. You can try to fix that by setting specific slicing times.")
})

test_that("tree Sanitizing works", {

    data(BeckLee_mat99)
    data(BeckLee_mat50)
    data(BeckLee_tree)
    ## t0 = true
    test <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = 5, model = "acctran", t0 = TRUE)
    expect_is(test, "dispRity")

    BeckLee_tree_wrong <- BeckLee_tree
    BeckLee_tree_wrong$tip.label[1] <- "hahahaha"
    error <- capture_error(chrono.subsets(BeckLee_mat50, BeckLee_tree_wrong, method = "discrete", time = 5, inc.nodes = FALSE))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")    
})

test_that("automatic time binning by size works", {
    ## Sanitizing
    ## Error when no tree
    ## Error when wrong format for time
    ## Error when size is too big

    ## Works with a single tree and data

    ## Works with a tree distribution and a single data

    ## Works with a single tree and a data distribution

    ## Works with multiple trees and multiple data
})