# #context("dispRity.utilities")

# # 1 - sanitizing, detect if the trees and matrices don't match. If they don't you need the same number of trees and matrices. Then output apply.multi.trees activator


# # - test case Mario

# # Email Mario:
# # For cleaning the trees, you can use dispRity::clean.data
# # For removing the 0 branch lengths, you can use dispRity::remove.zero.brlen (which shifts nodes to remove 0 values rather than adding branch lengths which can results in the trees not being ultrametric anymore)

# matrix <- Claddis::read_nexus_matrix("~/R/data/Matrix_20230214", equalize_weights = TRUE)
# tree <- read.nexus("~/R/data/sample100")
# data <- Claddis::ordinate_cladistic_matrix(morpho_matrix)$vectors

# ## Tips in the tree and data are the same


# #Extract the vector matrix from the Claddis morphospace object to feed it to dispRity
# morphosp = morphospace$vectors
# rownames(morphosp) = c(tree$tip.label,tree$node.label)


# ######POARS analysis
# ####################

# ## Time slices
# time_slices <- seq(from = 0, to = 280, by = 10)

# ## Gradual splits model
# gradual_model <- chrono.subsets(morphosp, tree, time = time_slices, method = "continuous",
#                                 model = "gradual.split", inc.nodes=TRUE)



# # 2 - run the apply.multi.trees
# # 3 - depending on the function, combine or leave the results as is.


# test_that("dispRity.multi.split", {
#     load("bound_test_data.rda")
#     trees <- bound_test_data$trees
#     matrices <- bound_test_data$matrices

#     ## Split just data
#     data <- fill.dispRity(make.dispRity(data = matrices))
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_null(test[[i]]$tree[[1]])
#     }
#     ## Three trees and one matrix
#     data <- fill.dispRity(make.dispRity(data = matrices[[1]]))
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 1)
#     expect_is(test[[1]], "dispRity")
#     expect_equal(length(test[[1]]$matrix), 1)
#     expect_null(test[[1]]$tree[[1]])


#     ## Split non-subseted data
#     ## Three trees and three matrices
#     data <- fill.dispRity(make.dispRity(data = matrices), tree = trees)
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
#     ## Three trees and one matrix
#     data <- fill.dispRity(make.dispRity(data = matrices[[1]]), tree = trees)
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
#     ## Three matrices and one tree
#     data <- fill.dispRity(make.dispRity(data = matrices), tree = trees[[1]])
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
#     ## Three matrices and two tree
#     data <- fill.dispRity(make.dispRity(data = matrices), tree = trees[1:2])
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 6)
#     for(i in 1:6) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }

    
#     ## Split subseted data
#     ## Three trees and three matrices
#     data <- chrono.subsets(matrices, tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
#     ## Three trees and one matrix
#     data <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
#     ## Three matrices and one tree
#     data <- chrono.subsets(matrices, tree = trees[[1]], time = 3, method = "continuous", model = "acctran", t0 = 5)
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
#     ## Three matrices and two tree
#     data <- chrono.subsets(matrices, tree = trees[1:2], time = 3, method = "continuous", model = "acctran", t0 = 5)
#     test <- dispRity.multi.split(data)
#     expect_is(test, "list")
#     expect_equal(length(test), 6)
#     for(i in 1:6) {
#         expect_is(test[[i]], "dispRity")
#         expect_equal(length(test[[i]]$matrix), 1)
#         expect_equal(length(test[[i]]$tree), 1)
#     }
# })

# test_that("dispRity.multi.apply", {

#     ## dispRity
#     load("bound_test_data.rda")
#     trees <- bound_test_data$trees
#     matrices <- bound_test_data$matrices

#     ## Split just data
#     data <- fill.dispRity(make.dispRity(data = matrices))
#     data <- dispRity.multi.split(data)

#     set.seed(1)
#     test <- dispRity.multi.apply(data, fun = dispRity, metric = centroids, centroid = 1000)
#     expect_is(test, "list")
#     expect_equal(length(test), 3)
#     for(i in 1:3) {
#         expect_is(test[[i]], "dispRity")
#     }
#     ## Option is parsed correctly
#     expect_equal(summary(test[[1]])$obs.median, 1732)
# })

## utilities internals
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
        "    3.3/2.79, 1.65/1.39, 0." 
    ))
})






    ## Matrices and trees

    ## For boot.matrix
    ## Just matrices
    ## Matrices and trees



    ## For dispRity
    ## Just matrices
    ## Matrices and trees
    ## Custom subsets
    ## Chrono subsets
    ## Boot matrices

    ## For fecking everything!
    # test1 <- dispRity(boot.matrix(custom.subsets(data)), metric = centroids)
    # test2 <- dispRity(boot.matrix(chrono.subsets(data)), metric = centroids)


    # set.seed(1)
    # load("bound_test_data.rda")
    # trees <- bound_test_data$trees
    # matrices <- bound_test_data$matrices

    # ## Test if it works with multiple trees and with multiple matrices ok
    # test <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
    # expect_is(test, "dispRity")
    # test_print <- capture_output(print(test))
    # expect_equal(test_print, " ---- dispRity object ---- \n3 continuous (acctran) time subsets for 19 elements in one matrix with 3 phylogenetic trees\n    5, 2.5, 0.")
    # tust <- dispRity(test, metric = centroids)
    # summary(tust)



    # ## 5 trees and matrices
    # tree <- rmtree(5, 5)
    # tree <- lapply(tree, makeNodeLabel)
    # class(tree) <- "multiPhylo"
    # data <- matrix(1, nrow = 9, ncol = 1, dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label)))
    # data <- replicate(5, data, simplify = FALSE)

    # ## Modifying one tree and one matrix
    # # tree[[1]]$node.label[1] <- rownames(data[[1]])[6] <- "nooooode"

    # ## Matrix with all nodes
    # # data_all <- matrix(1, nrow = 10, ncol = 1, dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label, "nooooode")))

    # data <- fill.dispRity(make.dispRity(data = data), tree = tree)
    # ## Works normally (multiple trees and multiple matrices)
    # test <- dispRity(chrono.subsets(data, method = "continuous", model = "acctran", time = 2), centroids)
