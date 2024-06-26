# #TESTING check.morpho

#context("check.morpho")

test_that("check.morpho works", {

    set.seed(1)
    #Get a random tree
    random.tree <- rcoal(10)
    #Get a random matrix
    random.matrix <- sim.morpho(random.tree, characters = 50, model = "HKY", rates = c(rgamma, 1, 1), substitution = c(runif, 2, 2))
    #Errors
    expect_error(
        check.morpho("a", parsimony = "fitch", first.tree = c(phangorn::dist.hamming, phangorn::NJ), distance = phangorn::RF.dist)
        )
    expect_error(
        check.morpho(random.matrix, parsimony = "a", first.tree = c(phangorn::dist.hamming, phangorn::NJ), distance = phangorn::RF.dist)
        )
    expect_error(
        check.morpho(random.matrix, parsimony = "fitch", first.tree = "a", distance = phangorn::RF.dist)
        )
    expect_error(
        check.morpho(random.matrix, parsimony = "fitch", first.tree = c(phangorn::dist.hamming, phangorn::NJ), orig.tree = "a", distance = phangorn::RF.dist)
        )
    expect_error(
        check.morpho(random.matrix, parsimony = "fitch", first.tree = c(phangorn::dist.hamming, phangorn::NJ), distance = "a")
        )
    expect_error(
        check.morpho(random.matrix, parsimony = sum, first.tree = c(phangorn::dist.hamming, phangorn::NJ), distance = phangorn::RF.dist)
        )

    set.seed(1)
    wrong_tree <- rtree(10)
    wrong_tree$tip.label <- letters[1:10]
    expect_error(
        check.morpho(random.matrix, orig.tree = wrong_tree, parsimony = "fitch", first.tree = c(phangorn::dist.hamming, phangorn::NJ), distance = phangorn::RF.dist)
        )

    #Output
    set.seed(1)
    test <- check.morpho(random.matrix, parsimony = "sankoff", orig.tree = random.tree)
    expect_equal(
        dim(test), c(4,1)
        )
    expect_equal(
       test[1,], 44, tolerance = 0.05
) # tol from bug in macos
    expect_equal(
       round(test[2,], 4), round(0.704545, 4), tolerance = 0.1
        ) # tol from bug in macos
    expect_equal(
       round(test[3,], digit = 4), round(0.6976744, digit = 4), tolerance = 0.1
        ) # tol from bug in macos
    expect_equal(
       test[4,], 6
        )

    #Verbose version
    set.seed(1)
    verbose <- capture_output(check.morpho(random.matrix, parsimony = "fitch", verbose = TRUE))
    expect_equal(strsplit(verbose, split = "p-score")[[1]][1], "Most parsimonious tree search:\nFinal ") # split from bug in macos


    #Test example (seed 10)
    set.seed(10)
    random.tree <- rcoal(10)
    random.matrix <- sim.morpho(random.tree, characters = 50, model = "ER", rates = c(rgamma, 1, 1))
    test <- check.morpho(random.matrix, orig.tree = random.tree)
    expect_equal(
        dim(test), c(4,1)
        )
    expect_equal(
       test[1,], 48
        )
    expect_equal(
       round(test[2,], digit = 4), round(0.6666667, digit = 4)
        )
    expect_equal(
       round(test[3,], digit = 4), round(0.6923077, digit = 4)
        )
    expect_equal(
       test[4,], 2
        )    

    ## Works with input contrast matrix
    set.seed(1)
    random_tree <- rcoal(10)
    random_matrix <- sim.morpho(random_tree, characters = 10, model = "ER", rates = c(rgamma, 1, 1))
    contrast_matrix <- get.contrast.matrix(random_matrix)
    test_out <- check.morpho(random_matrix, contrast.matrix = contrast_matrix)
    expect_is(test_out, "matrix")
    expect_equal(dim(test_out), c(3,1))

    ## First tree is a function
    first.tree.fun <- function(X) {phangorn::NJ(phangorn::dist.hamming(X))}
    test_out <- check.morpho(random_matrix, first.tree = first.tree.fun)
    expect_is(test_out, "matrix")
    expect_equal(dim(test_out), c(3,1))

})
