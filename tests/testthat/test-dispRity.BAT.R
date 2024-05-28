test_that("make.BAT.comm works", {
    dummy_matrix <- matrix(rnorm(90), 10, 9, dimnames = list(letters[1:10]))
    sub_matrix <- dummy_matrix[c(1:3), ]
    test <- make.BAT.comm(sub_matrix)
    expect_equal(dim(test), c(1,3))
    expect_equal(sum(test), 3)
    test <- make.BAT.comm(sub_matrix, data = dummy_matrix)
    expect_equal(dim(test), c(1,10))
    expect_equal(sum(test), 3)
})

test_that("dispRity.BAT works", {
    ## Test convert simple
    dummy_matrix <- matrix(rnorm(90), 10, 9, dimnames = list(letters[1:10]))
    data <- make.dispRity(dummy_matrix)
    out <- dispRity.BAT(data)
    expect_is(out, "list")
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(10,9))
    expect_equal(dim(out$comm), c(1,10))
    expect_true(is.null(out$tree))

    ## Test convert complex
    data(demo_data)
    eco_data <- demo_data$jones    
    out <- dispRity.BAT(eco_data)
    expect_is(out, "list")
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(48,47))
    expect_equal(dim(out$comm), c(2,48))
    expect_true(is.null(out$tree))

    ## Test convert complex
    data(disparity)
    out <- dispRity.BAT(disparity)
    expect_is(out, "list")
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(99,97))
    expect_equal(dim(out$comm), c(2807,99))
    expect_is(out$tree, "phylo")
})


test_that("dispRity.BAT works with bootstraps", {
    ## Test convert simple
    data(demo_data)
    data <- boot.matrix(demo_data$jones, bootstraps = 3, rarefaction = c(24,12))
    out <- dispRity.BAT(data)
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(48,47))
    expect_equal(dim(out$comm), c(14,48))
    expect_equal(rownames(out$comm)[c(1,7,2,8,6,14)], c("aspen.elements", "aspen.bootstrap.12.3", "aspen.bootstrap.24.1", "grassland.elements", "aspen.bootstrap.12.2", "grassland.bootstrap.12.3"))
    expect_true(is.null(out$tree))

    ## Works with probabilistic bootstraps
    data(BeckLee_mat99)
    data(BeckLee_ages)
    data(BeckLee_tree)
    data <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(120, 100, 80, 60, 40 , 20, 0), model = "gradual.split", inc.nodes = TRUE, FADLAD = BeckLee_ages, verbose = FALSE, t0 = FALSE)
    out <- dispRity.BAT(data)
    expect_equal(names(out), c("comm", "tree", "traits"))
    expect_equal(dim(out$traits), c(99,97))
    expect_equal(dim(out$comm), c(7,99))
    expect_equal(rownames(out$comm), as.character(seq(from = 120, to = 0, by = -20)))
    expect_equal(unique(c(out$comm)), c(0,1))
})
