## TESTING chrono.subsets

context("chrono.subsets")

data(BeckLee_tree)
data(BeckLee_mat50)
data(BeckLee_ages)
data(BeckLee_mat99)

# load("test_data.Rda")
# tree <- test_data$tree_data
# data <- test_data$ord_data_tips
# FADLAD <- test_data$FADLAD_data



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
data <- test_data$ord_data_tips_nodes

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
    
    # Get slice
    # expect_equal(
    #     as.vector(na.omit(unlist(get.slice(1, time[2], "ACCTRAN", adjust.FADLAD(FADLAD, tree, data), data, verbose = FALSE))))
    #     , c(7, 8, 9, 1, 2, 3, 12, 13, 14, 15, 44, 70, 73, 76, 79, 85, 48, 90, 47, 95, 46, 98))
    # expect_equal(
    #     unname(unlist(get.slice(slice = 1, time = time[2], model = "proximity", ages_tree = adjust.FADLAD(FADLAD, tree, data), data = data, verbose = FALSE, tree = tree)))
    #     , c(7, 8, 9, 1, 2, NA, 12, 13, 14, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    # )

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
    subsets_2 <- c("n6","Asioryctes","n7","n8","n10","n15","n17","n19","n22","n25","n34","n39","n44","n47")
    subsets_3 <- c("n17","n23","n27","n28","n29","n31","n32","n39","n42","n44","n48","n49")
    expect_equal(
        rownames(data[time_subsets[[1]]$elements,])
        , subsets_1)
    expect_equal(
        rownames(data[time_subsets[[2]]$elements,])
        , subsets_2)
    expect_equal(
        rownames(data[time_subsets[[3]]$elements,])
        , subsets_3)
})

## ACCTRAN
time_subsets <- chrono.subsets.continuous(data, tree, time, model = "acctran", FADLAD, inc.nodes = NULL, verbose)

## Test
test_that("chrono.subsets.continuous works properly with acctran model", {
    subsets_1 <- c("n2","n6","n8","n12","n16", "Zhangolestes")
    subsets_2 <- c("Kennalestes","Asioryctes","Ukhaatherium","Cimolestes","Maelestes","Batodon","Zalambdalestes","Barunlestes","Gypsonictops","Leptictis","Oxyclaenus","n20","n23","n26","n29","n35","Cynocephalus","n40","Patriomanis","n45","Icaronycteris","n48")
    subsets_3 <- c("Leptictis","Dasypodidae","n24","Potamogalinae","Dilambdogale","Widanelfarasia","Rhynchocyon","Procavia","Moeritherium","Trichechus","Cynocephalus","Adapis","Patriomanis","Soricidae","Solenodon")
    expect_equal(
        rownames(data[time_subsets[[1]]$elements,])
        , subsets_1)
    expect_equal(
        rownames(data[time_subsets[[2]]$elements,])
        , subsets_2)
    expect_equal(
        rownames(data[time_subsets[[3]]$elements,])
        , subsets_3)
})

## chrono.subsets
data = BeckLee_mat99
tree = BeckLee_tree
method = "continuous"
model = "acctran"
inc.nodes = TRUE
FADLAD = BeckLee_ages
verbose = FALSE

test_that("Sanitizing works for chrono.subsets (wrapper)", {
    ## Data
    expect_error(
        chrono.subsets(data = "A", tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data = 1, tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data = matrix(NA, nrow = 2, ncol = 3), tree, method, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    ## tree
    expect_error(
        chrono.subsets(data, tree = "A", method, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data, tree = 1, method, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data, tree = rtree(5), method, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    ## method
    expect_error(
        chrono.subsets(data, tree, method = 1, time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data, tree, method = "a", time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data, tree, method = c("c","d"), time, model, inc.nodes, FADLAD, verbose = FALSE)
        )
    ## time
    expect_error(
        chrono.subsets(data, tree, method, time = "time", model, inc.nodes, FADLAD, verbose = FALSE)
        )
    ## model
    expect_error(
        chrono.subsets(data, tree, method, time, model = 3, inc.nodes, FADLAD, verbose = FALSE)
        )
    expect_error(
        chrono.subsets(data, tree, method, time, model = c("acctran","deltran"), inc.nodes, FADLAD, verbose = FALSE)
        )
    ## FADlAD
    expect_error(
        chrono.subsets(data, tree, method, time, model, inc.nodes, FADLAD = data.frame(nrow = 2, ncol = 3), verbose = FALSE)
        )

    ## t0
    expect_error(
        chrono.subsets(data, tree, method, time, model, inc.nodes, FADLAD = data.frame(nrow = 2, ncol = 3), verbose = FALSE, t0 = "a")
        )
    expect_error(
        chrono.subsets(data, tree, method, time, model, inc.nodes, verbose = FALSE, t0 = c(1,2))
        )

    data(BeckLee_mat99)
    data(BeckLee_mat50)
    data(BeckLee_ages)
    data(BeckLee_tree)

    ## Method shortcuts
    continous_shortcut <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "c", time = 3, model = "acctran")
    discrete_shortcut <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "d", time = 3, inc.nodes = TRUE)

    expect_is(continous_shortcut, "dispRity")
    expect_is(discrete_shortcut, "dispRity")
    expect_equal(continous_shortcut$call$subsets[1], "continuous")
    expect_equal(discrete_shortcut$call$subsets[1], "discrete")
    
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
    expect_error(chrono.subsets(BeckLee_mat99, wrong_tree, method = "c", time = 3, model = "acctran", inc.nodes = FALSE))
    expect_error(chrono.subsets(BeckLee_mat99, wrong_tree, method = "c", time = 3, model = "acctran", inc.nodes = TRUE))

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
    error <- capture_error(chrono.subsets(BeckLee_mat99, multitrees, method = "c", time = 3, model = "deltran"))
    expect_equal(error[[1]], "The trees in multitrees must have the same tip labels.")

    ## No phylogeny provided!
    error <- capture_error(chrono.subsets(data = BeckLee_mat99, method = "c", time = 3, model = "acctran", FADLAD = BeckLee_ages))
    expect_equal(error[[1]], "If no phylogeny is provided, method must be \"discrete\".")

    ## Wrong label match between tree and data
    data_wrong <- BeckLee_mat99
    rownames(data_wrong)[1] <- "wrong!"
    error <- capture_error(chrono.subsets(data = data_wrong, tree = BeckLee_tree, method = "c", time = 3, model = "acctran"))
    expect_equal(error[[1]], "The labels in the matrix and in the tree do not match!\nTry using clean.data() to match both tree and data or make sure whether nodes should be included or not (inc.nodes = FALSE by default).")

})

test_that("Output format is correct", {
    out_test <- chrono.subsets(data, tree, method, time, model, inc.nodes, FADLAD)
    ## Class
    expect_is(
        out_test, "dispRity"
        )
    ## Length
    expect_equal(
        length(out_test)
        , 3)
    ## Names
    expect_equal(
        names(out_test)
        , c("matrix", "call", "subsets")
        )
})

test_that("Output format is correct", {
    output_continuous <- capture_message(test <- chrono.subsets(data, tree, method = "continuous", time, model, inc.nodes, FADLAD, verbose = TRUE))
    expect_equal(strsplit(as.character(output_continuous), split = ", : ")[[1]][2], "Creating 3 time samples through the tree:\n")

    output_discrete <- capture_message(test <- chrono.subsets(data, tree, method = "discrete", time, model, inc.nodes, FADLAD, verbose = TRUE))
    expect_equal(strsplit(as.character(output_continuous), split = ", : ")[[1]][2], "Creating 3 time samples through the tree:\n")
})

test_that("Example works", {
    data(BeckLee_tree) ; data(BeckLee_mat50) ; data(BeckLee_mat99) ; data(BeckLee_ages)
    ex1 <- chrono.subsets(data = BeckLee_mat50, tree = BeckLee_tree, method = "discrete", time = c(120, 80, 40), inc.nodes = FALSE, FADLAD = BeckLee_ages)
    expect_equal(
        length(ex1)
        , 3)
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
        , 3)
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
        , 3)
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
})

test_that("t0 works", {
    data <- test_data$ord_data_tips_nodes
    test <- chrono.subsets(data, tree, method = "continuous", model = "acctran", inc.nodes = TRUE, FADLAD = FADLAD, t0 = 100, time = 11)
    expect_is(test, "dispRity")
    expect_equal(names(test$subsets), as.character(rev(seq(from = 0, to = 100, by = 10))))
})

test_that("chrono.subsets works for empty subsets", {
    data <- test_data$ord_data_tips
    time <- c(145, 140, 139, 0)

    ## Discrete
    warnings <- capture_warnings(test <- chrono.subsets(data, tree, method = "discrete", time = c(145, 140, 139, 0)))
    expect_equal(warnings, c("The interval 145 - 140 is empty.", "The interval 140 - 139 is empty."))
    expect_equal(test$subsets[[1]][[1]][,1], NA)
    expect_equal(test$subsets[[2]][[1]][,1], NA)
    expect_equal(test$subsets[[3]][[1]][,1], c(5, 4, 6, 7, 8, 9, 1, 43, 2, 3, 10, 11, 42, 12, 13, 14, 15, 44, 17, 18, 36, 37, 38, 41, 32, 39, 40, 33, 34, 35, 49, 50, 24, 25, 26, 27, 28, 48, 16, 21, 22, 23, 47, 45, 19, 20, 46, 29, 30, 31))

    ## Continuous
    data <- test_data$ord_data_tips_nodes
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
    
    test1 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(120, 100, 80, 60, 40 , 20, 0), model = "gradual.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)
    test2 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(120, 100, 80, 60, 40 , 20, 0), model = "equal.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)

    expect_is(test1, "dispRity")
    expect_is(test1$subsets[[1]][[1]], "matrix")
    expect_equal(dim(test1$subsets[[1]][[1]]), c(6,3))
    expect_true(all(test1$subsets[[1]][[1]][,1:2] >= 1))
    expect_true(all(test1$subsets[[1]][[1]][,3] < 1))

    expect_is(test2, "dispRity")
    expect_is(test2$subsets[[1]][[1]], "matrix")
    expect_equal(dim(test2$subsets[[1]][[1]]), c(6,3))
    expect_true(all(test2$subsets[[1]][[1]][,1:2] >= 1))
    expect_true(all(test2$subsets[[1]][[1]][,3] < 1))
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

test_that("cbind.fill and recursive.combine list works", {
        x  <- matrix(1, nrow = 2, ncol = 1)
        x2 <- matrix(1, nrow = 2, ncol = 2)
        y  <- matrix(2, nrow = 4, ncol = 1)
        expect_equal(dim(cbind.fill(x, x2)[[1]]), dim(cbind(x, x2)))
        expect_equal(dim(cbind.fill(x, y)[[1]]) , c(4,2))
        expect_equal(dim(cbind.fill(x2, y)[[1]]), c(4,3))
        expect_equal(cbind.fill(x, y)$elements, matrix(c(1,1,NA,NA,2,2,2,2), ncol = 2))
        expect_equal(cbind.fill(y, x)$elements, matrix(c(2,2,2,2,1,1,NA,NA), ncol = 2))

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
    load("paleotree_test_data.Rda")
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
    error <- capture_error(chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = trees_wrong_tip))
    expect_equal(error$message, "trees_wrong_tip: wrong number of tips in the following tree(s): 2.")
    error <- capture_error(chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = tree_wrong_label))
    expect_equal(error$message, "The trees in tree_wrong_label must have the same node labels.")
    warning <- capture_warning(test <- chrono.subsets(data, method = "continuous", time = c(1, 0.5, 0), tree = tree_wrong_roottime, model = "acctran"))
    expect_equal(warning$message, "Differing root times in tree_wrong_roottime. The $root.time for all tree has been set to the maximum (oldest) root time: 81.")

    ## Works with a multiPhylo object
    test <- chrono.subsets(data, tree, method = "continuous", time = 3, model = "proximity")

    expect_is(test, "dispRity")
    expect_equal(names(test), c("matrix", "call", "subsets"))
    expect_equal(names(test$subsets), c("9.31405078347417", "4.65702539173708", "0"))
    expect_equal(unique(unlist(lapply(test$subsets, names), use.names = FALSE)), "elements")
    expect_equal(unlist(lapply(test$subsets, lapply, dim), use.names = FALSE), c(3, 2, 5, 2, 10, 2))
    expect_equal(unique(c(test$subsets[[2]]$elements)), c(17, 22, 26, 21, NA, 2, 25, 27))

    ## Works with discrete
    test <- chrono.subsets(data, tree, method = "discrete", time = 3, inc.nodes = TRUE)
    expect_is(test, "dispRity")
    expect_equal(unlist(lapply(test$subsets, lapply, dim), use.names = FALSE), c(6, 2, 5, 2, 14, 2))

    ## Works with probabilities
    test <- chrono.subsets(data, tree, method = "continuous", time = 3, model = "gradual.split")
    expect_is(test, "dispRity")
    expect_equal(unlist(lapply(test$subsets, lapply, dim), use.names = FALSE), c(3, 6, 7, 6, 10, 6))
})

test_that("chrono.subsets works with multiple matrices", {

    data <- list(BeckLee_mat50, BeckLee_mat50)
    data_wrong <- list(BeckLee_mat50, BeckLee_mat99)

    error <- capture_error(test <- chrono.subsets(data_wrong, tree = BeckLee_tree, method = "discrete", time = 4))
    expect_equal(error[[1]], "data must be matrix or a list of matrices with the same dimensions and row names.")
    warn <- capture_warning(test <- chrono.subsets(data, tree = BeckLee_tree, method = "discrete", time = 4))
    expect_equal(warn[[1]], "The interval 133.51104 - 100.13328 is empty.")

    expect_is(test, "dispRity")
    expect_is(test$matrix, "list")
    expect_equal(length(test$matrix), 2)
    expect_equal(dim(test$matrix[[1]]), c(50, 48))
})