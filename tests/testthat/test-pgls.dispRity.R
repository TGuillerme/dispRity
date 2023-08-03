test_that("sanitizing works", {

    ## Simple examples
    data(BeckLee_tree)
    data(BeckLee_mat99)
    data(BeckLee_mat50)
    data(BeckLee_ages)
    nonode_tree <- BeckLee_tree
    nonode_tree$node.labels <- NULL

    ## Base examples
    disparity_base <- dispRity(BeckLee_mat50, metric = centroids)
    disparity_group <- dispRity(custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE)), tree = nonode_tree, metric = centroids)
    expect_warning(disparity_time <- dispRity(chrono.subsets(BeckLee_mat50, FADLAD = BeckLee_ages, tree = BeckLee_tree, time = 5, method = "discrete"), metric = centroids, tree = nonode_tree))

    ## Sanitizing
    ## data
    wrong_dimensions <- dispRity(BeckLee_mat50, metric = c(sum, variances))
    error <- capture_error(pgls.dispRity(data = "wrong_dimensions", tree = BeckLee_tree))
    expect_equal(error[[1]], "data must be of class dispRity.")
    error <- capture_error(pgls.dispRity(data = wrong_dimensions, tree = BeckLee_tree))
    expect_equal(error[[1]], "Impossible to run a univariate pgls on wrong_dimensions because doesn't contain a dimension level-2 metric. See ?dispRity.metric for more info.")
    
    ## tree
    wrong_tree <- rtree(50)
    error <- capture_error(pgls.dispRity(data = wrong_dimensions, tree = "wrong_tree"))
    expect_equal(error[[1]], "tree must be of class multiPhylo or phylo.")
    expect_warning(error <- capture_error(pgls.dispRity(data = wrong_dimensions, tree = wrong_tree)))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")

    ## formula
    wrong_formula <- dispoority ~ 1
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = BeckLee_tree, formula = wrong_formula))
    expect_equal(error[[1]], "The response term of the formula must be 'disparity'.")

    ## model


    ## Testing super simple
    data <- disparity_base


    ## Multiple trees example
    set.seed(1)
    trees <- replicate(3, rcoal(10), simplify = FALSE)
    class(trees) <- "multiPhylo"
    disparities <- replicate(3, matrix(rnorm(30), ncol = 3, dimnames = list(paste0("t", 1:10))), simplify = FALSE)

    ## Multiple matrices example

    ## Multiple trees and matrices

    ## Checks whether the dispRity object contains a tree or whether a tree can be added

    ## Check whether the dispRity object is a level 2 metric at least

    ## Check if formula is interpretable

    ## Check arguments for phylolm
})