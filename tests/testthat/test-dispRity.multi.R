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
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three trees and one matrix
    data <- fill.dispRity(make.dispRity(data = matrices[[1]]), tree = trees)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three matrices and one tree
    data <- fill.dispRity(make.dispRity(data = matrices), tree = trees[[1]])
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three matrices and two tree
    data <- fill.dispRity(make.dispRity(data = matrices), tree = trees[1:2])
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 6)
    for(i in 1:6) {
        expect_is(test[[i]], "dispRity")
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
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three trees and one matrix
    data <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three matrices and one tree
    data <- chrono.subsets(matrices, tree = trees[[1]], time = 3, method = "continuous", model = "acctran", t0 = 5)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 3)
    for(i in 1:3) {
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
    ## Three matrices and two tree
    data <- chrono.subsets(matrices, tree = trees[1:2], time = 3, method = "continuous", model = "acctran", t0 = 5)
    test <- dispRity.multi.split(data)
    expect_is(test, "list")
    expect_equal(length(test), 6)
    for(i in 1:6) {
        expect_is(test[[i]], "dispRity")
        expect_equal(length(test[[i]]$matrix), 1)
        expect_equal(length(test[[i]]$tree), 1)
    }
})

test_that("dispRity.multi.apply", {

})

test_that("dispRity.multi.merge", {

})


## utilities internals
test_that("dispRity.multi works", {
    set.seed(1)
    load("bound_test_data.rda")
    trees <- bound_test_data$trees
    matrices <- bound_test_data$matrices

    ## Test if it works with multiple trees and with multiple matrices ok
    test <- chrono.subsets(matrices[[1]], tree = trees, time = 3, method = "continuous", model = "acctran", t0 = 5)
    expect_is(test, "dispRity")
    test_print <- capture_output(print(test))
    expect_equal(test_print, " ---- dispRity object ---- \n3 continuous (acctran) time subsets for 19 elements in one matrix with 3 phylogenetic trees\n    5, 2.5, 0.")
    tust <- dispRity(test, metric = centroids)
    summary(tust)



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

})