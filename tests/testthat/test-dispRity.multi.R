test_that("dispRity.multi.split", {
    load("bound_test_data.rda")
    trees <- bound_test_data$trees
    matrices <- bound_test_data$matrices

    ## Split just data
    data <- fill.dispRity(make.dispRity(data = matrices))
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_null(test[[i]]$tree[[1]])
    }
    ## Three trees and one matrix
    data <- fill.dispRity(make.dispRity(data = matrices[[1]]))
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 1)
    expect_is(test[[1]], "dispRity")
    expect_equal(length(test[[1]]$matrix), 1)
    expect_null(test[[1]]$tree[[1]])


    ## Split non-subseted data
    ## Three trees and three matrices
    data <- fill.dispRity(make.dispRity(data = matrices), tree = trees)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "list")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three trees and one matrix
    data <- fill.dispRity(make.dispRity(data = matrices[[1]]), tree = trees)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "list")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three matrices and one tree
    data <- fill.dispRity(make.dispRity(data = matrices), tree = trees[[1]])
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "list")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    
    ## Split subseted data
    ## Three trees and three matrices
    data <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "list")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three trees and one matrix
    data <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "list")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three matrices and one tree
    data <- chrono.subsets(matrices, tree = trees[[1]], time = 3, method = "continuous", model = "acctran", t0 = 5)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "list")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
})

test_that("dispRity.multi.apply", {

    ## dispRity
    load("bound_test_data.rda")
    trees <- bound_test_data$trees
    matrices <- bound_test_data$matrices

    ## Split just data
    data <- fill.dispRity(make.dispRity(data = matrices))
    data <- dispRity.multi.split(data)

    set.seed(1)
    test <- dispRity.multi.apply(data, fun = dispRity, metric = centroids, centroid = 1000)
    expect_is(test, "dispRity")
    expect_is(test, "multi")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "dispRity")
    }
    ## Option is parsed correctly
    expect_equal(summary(test[[1]])$obs.median, 1732)
})

test_that("dispRity.multi works for custom.subsets", {

    set.seed(1)
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    tree_trifurc <- tree[[1]]
    tree_trifurc$edge <- tree_trifurc$edge[-5, ]
    tree_trifurc$edge[c(5,6),1] <- 8
    tree_trifurc$edge.length <- tree_trifurc$edge.length[-5] 
    tree_trifurc$Nnode <- 3
    tree_trifurc$node.label <- tree_trifurc$node.label[-4]
    tree_trifurcs <- list(tree[[1]], tree_trifurc)
    tree_diff <- tree
    tree_diff[[1]]$node.label[1] <- "noooooode"
    class(tree_diff) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- list(data, data)
    data_diff <- data
    rownames(data_diff[[1]])[6] <- "noooooode"
        
    ## For custom.subsets

    groups <- list(paste0("t", 1:3), paste0("t", 3:5))
    groups_bugged <- list(paste("t", 1:5), paste0("Node", 1:4))
    ## Normal test
    test <- custom.subsets(data = data, tree = tree, group = groups)
    expect_is(test, "dispRity")
    expect_equal(length(test$matrix), 2)
    expect_equal(length(test$tree), 2)

    ## Just matrices
    expect_warning(test <- custom.subsets(data_diff, group = groups))
    expect_is(test, c("dispRity", "multi"))
    expect_equal(length(test), 2)
    expect_equal(length(test[[1]]$matrix), 1)
    expect_equal(length(test[[2]]$matrix), 1)
    expect_equal(length(test[[1]]$tree[[1]]), 0)
    expect_equal(length(test[[2]]$tree[[1]]), 0)
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "2 customised subsets for 9 elements in 2 separated matrices:",
        "    1, 2." 
    ))

    ## 2 Matrices and 2 trees 
    expect_warning(test <- custom.subsets(data = data_diff, tree = tree_diff, group = groups))
    expect_is(test, c("dispRity", "multi"))
    expect_equal(length(test), 2)
    expect_equal(length(test[[1]]$matrix), 1)
    expect_equal(length(test[[2]]$matrix), 1)
    expect_equal(length(test[[1]]$tree), 1)
    expect_equal(length(test[[2]]$tree), 1)
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "2 customised subsets for 9 elements in 2 separated matrices with 2 phylogenetic trees",
        "    1, 2."
    ))

    ## 1 Matrix (with everything) and 2 trees
    data_all <- rbind(data_diff[[1]], "Node1" = c(0,0))
    expect_warning(test <- custom.subsets(data = data_all, tree = tree_diff, group = groups))
    expect_is(test, c("dispRity", "multi"))
    expect_equal(length(test), 2)
    expect_equal(length(test[[1]]$matrix), 1)
    expect_equal(length(test[[2]]$matrix), 1)
    expect_equal(length(test[[1]]$tree), 1)
    expect_equal(length(test[[2]]$tree), 1)
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "2 customised subsets for 9 elements in 2 separated matrices with 2 phylogenetic trees",
        "    1, 2." 
    ))
})

test_that("dispRity.multi works for chrono.subsets", {
    ## Two matrices and two trees
    set.seed(1)
    tree <- rmtree(2, 10)
    tree[[1]] <- makeNodeLabel(tree[[1]])
    tree[[2]] <- makeNodeLabel(tree[[2]], prefix = "shnode")
    tree[[1]]$root.time <- max(tree.age(tree[[1]])$ages)
    tree[[2]]$root.time <- max(tree.age(tree[[2]])$ages)

    data <- list(matrix(0, nrow = Ntip(tree[[1]]) + Nnode(tree[[1]]), dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label))),
                 matrix(0, nrow = Ntip(tree[[2]]) + Nnode(tree[[2]]), dimnames = list(c(tree[[2]]$tip.label, tree[[2]]$node.label))))

    ## Test working fine
    warn <- capture_warning(chrono.subsets(data = data, tree = tree, time = 3, method = "continuous", model = "acctran"))
    expect_equal(warn[[1]], "The following elements are not present in all matrices: shnode1, shnode2, shnode3, shnode4, shnode5, shnode6, shnode7, shnode8, shnode9, Node1, Node2, Node3, Node4, Node5, Node6, Node7, Node8, Node9. The matrices will be treated as separate trait-spaces.")
    expect_warning(test <- chrono.subsets(data = data, tree = tree, time = 3, method = "continuous", model = "acctran"))
    expect_is(test, c("dispRity", "multi"))
    expect_equal(length(test), 2)
    expect_equal(length(test[[1]]$matrix), 1)
    expect_equal(length(test[[2]]$matrix), 1)
    expect_equal(length(test[[1]]$tree), 1)
    expect_equal(length(test[[2]]$tree), 1)
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "3 continuous (acctran) time subsets for 19 elements in 2 separated matrices with 2 phylogenetic trees",
        "    2.62/1.95, 1.31/0.98, 0." 
    ))

    expect_warning(write <- capture_messages(test <- chrono.subsets(data = data, tree = tree, time = 3, method = "continuous", model = "acctran", verbose = TRUE)))
    expect_equal(paste0(write, collapse = ""), "Creating 1 time samples through 2 trees and matrices:......Done.\n")
})

test_that("dispRity.multi works for boot.matrix", {
    ## Two matrices
    tree <- rmtree(2, 10)
    tree[[1]] <- makeNodeLabel(tree[[1]])
    tree[[2]] <- makeNodeLabel(tree[[2]], prefix = "shnode")
    tree[[1]]$root.time <- max(tree.age(tree[[1]])$ages)
    tree[[2]]$root.time <- max(tree.age(tree[[2]])$ages)
    data <- list(matrix(0, nrow = Ntip(tree[[1]]) + Nnode(tree[[1]]), dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label))),
                 matrix(0, nrow = Ntip(tree[[2]]) + Nnode(tree[[2]]), dimnames = list(c(tree[[2]]$tip.label, tree[[2]]$node.label))))

    ## Test working fine
    expect_warning(test <- boot.matrix(data, bootstraps = 7))
    expect_is(test, c("dispRity", "multi"))
    expect_equal(length(test), 2)
    expect_equal(length(test[[1]]$matrix), 1)
    expect_equal(length(test[[2]]$matrix), 1)
    expect_equal(length(test[[1]]$tree[[1]]), 0)
    expect_equal(length(test[[2]]$tree[[1]]), 0)
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "19 elements in 2 separated matrices with 1 dimensions.",
        "Data was bootstrapped 7 times (method:\"full\")." 
    ))

    expect_warning(write <- capture_messages(test <- boot.matrix(data, bootstraps = 5, verbose = TRUE, boot.type = "single")))
    expect_equal(paste0(write, collapse = ""), "Bootstrapping..Done.")
})

test_that("dispRity.multi works for dispRity", {
    ## Two matrices
    tree <- rmtree(2, 10)
    tree[[1]] <- makeNodeLabel(tree[[1]])
    tree[[2]] <- makeNodeLabel(tree[[2]], prefix = "shnode")
    tree[[1]]$root.time <- max(tree.age(tree[[1]])$ages)
    tree[[2]]$root.time <- max(tree.age(tree[[2]])$ages)
    data <- list(matrix(0, nrow = Ntip(tree[[1]]) + Nnode(tree[[1]]), dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label))),
                 matrix(0, nrow = Ntip(tree[[2]]) + Nnode(tree[[2]]), dimnames = list(c(tree[[2]]$tip.label, tree[[2]]$node.label))))

    ## Test working fine
    expect_warning(test <- dispRity(data, metric = mean, tree = tree))    
    expect_is(test, c("dispRity"))
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "disparity"))
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "19 elements in 2 separated matrices with 1 dimensions with 2 phylogenetic trees",
        "Disparity was calculated as: mean." 
    ))
    expect_null(plot(test))
    expect_equal(summary(test)$obs.median, 0)

    ## Verbose
    # text <- capture_messages(expect_warning(test <- dispRity(data, metric = mean, tree = tree, verbose = TRUE)))
    # expect_equal(paste0(text, collapse = ""), "Calculating multiple disparities..Done.\n")

    ## Recycling custom.subsets
    set.seed(1)
    tree <- makeNodeLabel(rtree(5))
    tree <- list(tree, tree)
    class(tree) <- "multiPhylo"
    tree_trifurc <- tree[[1]]
    tree_trifurc$edge <- tree_trifurc$edge[-5, ]
    tree_trifurc$edge[c(5,6),1] <- 8
    tree_trifurc$edge.length <- tree_trifurc$edge.length[-5] 
    tree_trifurc$Nnode <- 3
    tree_trifurc$node.label <- tree_trifurc$node.label[-4]
    tree_trifurcs <- list(tree[[1]], tree_trifurc)
    tree_diff <- tree
    tree_diff[[1]]$node.label[1] <- "noooooode"
    class(tree_diff) <- "multiPhylo"
    data <- matrix(0, nrow = 9, ncol = 2, dimnames = list(c(paste0("t", 1:5), paste0("Node", 1:4))))
    data <- list(data, data)
    data_diff <- data
    rownames(data_diff[[1]])[6] <- "noooooode"
    ## 1 Matrix (with everything) and 2 trees
    data_all <- rbind(data_diff[[1]], "Node1" = c(0,0))
    groups <- list(paste0("t", 1:3), paste0("t", 3:5))
    expect_warning(custom_subsets <- custom.subsets(data = data_all, tree = tree_diff, group = groups))
    
    test <- dispRity(custom_subsets, metric = centroids)
    expect_is(test, c("dispRity"))
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "disparity"))
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "2 customised subsets for 9 elements in 2 separated matrices with 2 phylogenetic trees",
        "    1, 2.",
        "Disparity was calculated as: centroids." 
    ))
    expect_null(plot(test))
    expect_equal(summary(test)$obs.median, c(0, 0))


    ## Recycling chrono.subsets
    ## Two matrices and two trees
    set.seed(1)
    tree <- rmtree(2, 10)
    tree[[1]] <- makeNodeLabel(tree[[1]])
    tree[[2]] <- makeNodeLabel(tree[[2]], prefix = "shnode")
    tree[[1]]$root.time <- max(tree.age(tree[[1]])$ages)
    tree[[2]]$root.time <- max(tree.age(tree[[2]])$ages)

    data <- list(matrix(0, nrow = Ntip(tree[[1]]) + Nnode(tree[[1]]), dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label))),
                 matrix(0, nrow = Ntip(tree[[2]]) + Nnode(tree[[2]]), dimnames = list(c(tree[[2]]$tip.label, tree[[2]]$node.label))))

    ## Test working fine
    expect_warning(chrono_subsets <- chrono.subsets(data = data, tree = tree, time = 3, method = "continuous", model = "acctran"))
    expect_warning(test <- dispRity(chrono_subsets, metric = centroids))
    expect_is(test, c("dispRity", "multi"))
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "disparity"))
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "3 continuous (acctran) time subsets for 19 elements in 2 separated matrices with 2 phylogenetic trees",
        "    2.62/1.95, 1.31/0.98, 0.",
        "Disparity was calculated as: centroids." 
    ))
    expect_null(plot(test))
    expect_equal(summary(test)$obs.median, c(0, 0, NA))

    ## Recycling boot.matrix
    ## Two matrices
    tree <- rmtree(2, 10)
    tree[[1]] <- makeNodeLabel(tree[[1]])
    tree[[2]] <- makeNodeLabel(tree[[2]], prefix = "shnode")
    tree[[1]]$root.time <- max(tree.age(tree[[1]])$ages)
    tree[[2]]$root.time <- max(tree.age(tree[[2]])$ages)
    data <- list(matrix(0, nrow = Ntip(tree[[1]]) + Nnode(tree[[1]]), dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label))),
                 matrix(0, nrow = Ntip(tree[[2]]) + Nnode(tree[[2]]), dimnames = list(c(tree[[2]]$tip.label, tree[[2]]$node.label))))

    ## Test working fine
    expect_warning(boot_matrix <- boot.matrix(data, bootstraps = 7))
    test <- dispRity(boot_matrix, metric = centroids)
    expect_is(test, c("dispRity"))
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "disparity"))
    expect_equal(capture.output(test), c(
        " ---- dispRity object ---- ",
        "19 elements in 2 separated matrices with 1 dimensions.",
        "Data was bootstrapped 7 times (method:\"full\").",
        "Disparity was calculated as: centroids." 
    ))
    expect_null(plot(test))
    expect_equal(summary(test)$obs.median, 0)
})
