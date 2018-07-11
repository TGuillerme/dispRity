## TESTING boot.matrix

context("boot.dispRity")

## Loading the data
load("test_data.Rda")
data <- test_data$ord_data_tips


## Internal functions tests
test_that("internal: bootstrap methods", {
    ## Full bootstrap selects three value
    expect_equal(
        length(boot.full(seq(1:20), 3))
        ,3)
    ## Full bootstrap selects more values than the sequence length
    expect_equal(
        length(boot.full(seq(1:20), 40))
        ,40)

    ## Single bootstrap doesn't work with less than two values
    expect_error(boot.single(seq(1:20), 1))
    ## Single boostrap selects three value
    expect_equal(
        length(boot.single(seq(1:20), 3))
        ,3)
})

## Internal functions tests
test_that("internal: bootstrap replicates", {
    data(disparity)
    subsets <- disparity$subsets[[1]]

    ## Select rarefactions
    expect_equal(
        select.rarefaction(subsets, 8)
        ,list(18, 8))
    expect_equal(
        select.rarefaction(subsets, c(3,5))
        ,list(18, 3, 5))

    ## One bootstrap replicate
    set.seed(1)
    test_silent <- replicate.bootstraps.silent(6, 5, subsets, boot.full)
    set.seed(1)
    expect_message(test_verbose <- replicate.bootstraps.verbose(6, 5, subsets, boot.full))

    ## Both are the same!
    expect_is(test_silent, "matrix")
    expect_is(test_verbose, "matrix")
    expect_equal(dim(test_silent), c(6,5))
    expect_equal(dim(test_verbose), c(6,5))
    expect_true(all(as.vector(test_silent) == as.vector(test_verbose)))

    ## Bootstrap replicates wrapper
    test_boot <- bootstrap.wrapper(subsets, bootstraps = 6, rarefaction = c(3,5), boot.type.fun = boot.full, verbose = FALSE)
    expect_is(test_boot, "list")
    expect_equal(length(test_boot), 3)
    expect_equal(unlist(lapply(test_boot, dim)), c(18, 6, 3, 6, 5, 6))
})

## Testing boot.matrix with a single matrix input
bootstraps = 5
rarefaction = FALSE
boot.type = "full"
## Sanitizing
test_that("Sanitizing works correctly", {
    expect_error(
        boot.matrix(data = "a", bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps = FALSE, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps = "a", rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction = "a", dimensions = FALSE, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = -1, verbose = FALSE, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = 8, boot.type = "full")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "rangers")
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = 2)
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = FALSE, verbose = FALSE, boot.type = "full", parallel = TRUE)
        )
    expect_error(
        boot.matrix(data, bootstraps, rarefaction, dimensions = 49)
        )
    ## Wrong data input
    dutu <- list(1,2,3) ; class(dutu) <- "dispRity"
    expect_error(
        boot.matrix(dutu)
        )

    names(dutu) <- letters[1:3]
    expect_error(
        boot.matrix(dutu)
        )
})

## No bootstrap (is equal to the matrix)
test_that("No bootstraps", {
    test <- boot.matrix(data, bootstraps = 0)
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        as.vector(test$subsets[[1]][[1]])
        , seq(1:nrow(test$matrix)))
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        length(test$subsets[[1]])
        ,1)
    expect_equal(
        length(test$subsets)
        ,1)
    expect_is(
        test$subsets[[1]][[1]]
        ,"matrix")
})

## No bootstrap but remove dimensions
test_that("Remove dimensions", {
    expect_equal(
        boot.matrix(data, bootstraps = 0, dimensions = 0.5)$call$dimensions
        ,24)
    expect_equal(
        boot.matrix(data, bootstraps = 0, dimensions = 24)$call$dimensions
        ,24)
})


## Bootstraps = 5
test_that("5 bootstraps", {
    data <- test_data$ord_data_tips
    test <- boot.matrix(data, bootstraps = 5)
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        dim(test$subsets[[1]][[1]])
        ,c(50,1))
    expect_equal(
        dim(test$subsets[[1]][[2]])
        ,c(50,5))
    expect_equal(
        length(test$subsets[[1]])
        ,2)
})

## Bootstraps = 5 + Rarefaction = 5
test_that("5 bootstraps, rarefaction = 5", {
    test <- boot.matrix(data, bootstraps = 5, rarefaction = 5)
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        dim(test$subsets[[1]][[2]])
        ,c(50,5))
    expect_equal(
        dim(test$subsets[[1]][[3]])
        ,c(5,5))
})

## Bootstraps = 5 + Rarefaction = TRUE
test_that("5 bootstraps, rarefaction = TRUE", {
    test <- boot.matrix(data, bootstraps = 5, rarefaction = TRUE)
    expect_equal(
        length(test$subsets[[1]])
        , 49)
    for(rare in 2:49) {
        expect_equal(
            dim(test$subsets[[1]][[rare]])
            ,c(50-(rare-2),5))
    }
})

## Bootstraps = 5 + Rarefaction = c(5,6) + boot.type
test_that("5 bootstraps, rarefaction = 5,6, boot type", {
    test <- boot.matrix(data, bootstraps = 5, rarefaction = c(5,6), boot.type = "single")
    expect_equal(
        test$call$bootstrap[[1]]
        , 5)
    expect_equal(
        test$call$bootstrap[[2]]
        , "single")
    expect_equal(
        test$call$bootstrap[[3]]
        , c(5,6))
})


## Bootstraps = 5 + Rarefaction = c(5,6) + subsets
test_that("5 bootstraps, rarefaction = 5,6, subsets", {
    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
    groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))
    matrix_list <- custom.subsets(ordinated_matrix, groups)
    test <- boot.matrix(matrix_list, bootstraps = 2, rarefaction = c(4,3))
    expect_is(
        test
        , "dispRity")
    expect_equal(
        length(test)
        , 3)
    expect_equal(
        test$call$dimensions
        , ncol(test$matrix))
    expect_equal(
        length(test$subsets)
        ,2)
    expect_equal(
        length(test$subsets[[1]])
        ,4)
    expect_equal(
        length(test$subsets[[2]])
        ,4)
    expect_equal(
        dim(test$subsets[[2]][[2]])
        ,c(nrow(test$subsets[[2]]$elements), 2))
})


## Verbose bootstrap
test_that("verbose bootstrap works", {
    data(BeckLee_mat99)
    data(BeckLee_tree)
    data(BeckLee_ages)
    data <- matrix(rnorm(25), 5, 5)
    out <- capture_messages(boot.matrix(data, verbose = TRUE))
    expect_equal(out,
        c("Bootstrapping", ".", "Done."))

    ## Verbose works with single elements subsets
    data1 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(139, 60), model = "gradual.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)

    data1$subsets$`139`$elements <- matrix(data1$subsets$`139`$elements[-1,], nrow = 1)

    expect_equal(capture_messages(
        boot.matrix(data1, 10, boot.type = "single", verbose = TRUE)
        ), c("Bootstrapping", ".", ".", "Done."))
    expect_equal(capture_messages(
        boot.matrix(data1, 10, boot.type = "full", verbose = TRUE)
        ), c("Bootstrapping", ".", ".", "Done."))
    set.seed(1)
    test_single <- boot.matrix(data1, 10, boot.type = "single", verbose = FALSE)
    set.seed(1)
    test_full <- boot.matrix(data1, 10, boot.type = "full", verbose = FALSE)

    expect_equal(test_single$subsets$`139`[[2]], matrix(51, ncol = 10, nrow = 1))
    expect_equal(test_full$subsets$`139`[[2]], matrix(51, ncol = 10, nrow = 1))
})


## Bootstrap works with empty or small (<3 subsets)
test_that("Boot.matrix works with small, empty/subsets", {

    tree <- test_data$tree_data
    data <- test_data$ord_data_tips_nodes
    FADLAD <- test_data$FADLAD_data

    silent <- capture_warnings(data <- chrono.subsets(data, tree, model = "deltran", method = "continuous", time = c(140, 138, 130, 120, 100)))

    warnings <- capture_warnings(test <- boot.matrix(data))
    expect_equal(warnings, "The following subsets have less than 3 elements: 140, 138, 130.\nThis might effect the bootstrap/rarefaction output.")

    expect_equal(test$subsets[[1]][[2]], matrix(rep(NA, 100), nrow = 1))
    expect_equal(unique(sort(test$subsets[[2]][[2]])), c(51))
})


test_that("boot.single.proba works well", {

    elements <- matrix(c(1,3,5,2,4,6,0.75, 0.01, 0.99), ncol = 3, byrow = FALSE)

    expect_length(boot.single.proba(elements, rarefaction = 3), 3)
    expect_length(boot.single.proba(elements, rarefaction = 2), 2)
    expect_error(boot.single.proba(elements, rarefaction = 1))
    
    set.seed(1)
    expect_equal(boot.single.proba(elements, rarefaction = 3), c(5,1,1))

    elements[1,2] <- NA
    set.seed(1)
    expect_equal(boot.single.proba(elements, rarefaction = 3), c(3,5,3))    
})

test_that("boot.matrix deals with probabilities subsets", {
    data(BeckLee_mat99)
    data(BeckLee_ages)
    data(BeckLee_tree)
    

    data1 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100, 60), model = "gradual.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)
    data2 <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100, 60), model = "proximity", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)

    set.seed(1)
    test1 <- boot.matrix(data1, bootstraps = 10)
    set.seed(1)
    test2 <- boot.matrix(data2, bootstraps = 10)
    set.seed(1)
    test3 <- boot.matrix(data2, bootstraps = 10)

    expect_equal(dim(test1$subsets[[1]][[2]]), c(15,10))
    expect_equal(dim(test1$subsets[[2]][[2]]), c(21,10))
    expect_equal(dim(test2$subsets[[1]][[2]]), c(11,10))
    expect_equal(dim(test2$subsets[[2]][[2]]), c(20,10))
})


test_that("boot.matrsix works with the prob option (for probabilities sampling)", {

    
    ## Custom subsets
    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
    groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))
    cust_test <- custom.subsets(ordinated_matrix, groups)

    ## Probas
    prob_v <- c("a" = 0.125, "b" = 1, "c" = 0, "j" = 8, "h" = 0.125)
    prob_m <- matrix(c(1,9,3,0,1), ncol = 1)


    ## Errors
    expect_error(
        boot.matrix(cust_test, prob = "a")
        )
    expect_error( # Matrix without dimnames
        boot.matrix(cust_test, prob = prob_m)
        )
    rownames(prob_m) <- names(prob_v)
    expect_error( # Matrix with two columns
        boot.matrix(cust_test, prob = cbind(prob_m, prob_m))
        )
    expect_error( # wrong name
        boot.matrix(cust_test, prob = c("A" = 0.125))
        )
    expect_error( # Negative probabilities
        boot.matrix(cust_test, prob = c("a" = -0.125))
        )

    ## Working with vector
    set.seed(1)
    test <- boot.matrix(cust_test, prob = prob_v)
    expect_is(test, "dispRity")
    results <- table(as.vector(unlist(lapply(test$subsets, function(X) return(X[[2]])))))
    expect_equal( #no 3
        names(results), as.character(seq(1:10)[-3])
        )
    expect_equal( #10 oversampled
        names(which(results == max(results))), "10"
        )

    ## Working with a matrix
    set.seed(1)
    test <- boot.matrix(cust_test, prob = prob_m)
    expect_is(test, "dispRity")
    results <- table(as.vector(unlist(lapply(test$subsets, function(X) return(X[[2]])))))
    expect_equal( #no 10
        names(results), as.character(seq(1:10)[-10])
        )
    expect_equal( #2 oversampled
        names(which(results == max(results))), "2"
        )

    ## Chrono subsets
    data(BeckLee_mat99)
    data(BeckLee_ages)
    data(BeckLee_tree)
    slice_disc <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "discrete", time = c(100, 60, 0), inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)
    slice_grad <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "gradual.split", time = c(100, 60, 0), inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)
    no_nodes <- rep(0, 49)
    no_nodes_nonames <- rep(0, 49)
    names(no_nodes) <- paste0("n", seq(1:49))
    
    ## Errors
    expect_error(boot.matrix(slice_grad, prob = no_nodes))
    expect_error(boot.matrix(slice_disc, prob = no_nodes_nonames))


    ## Working with a chrono.subset
    set.seed(1)
    test <- boot.matrix(slice_disc, prob = no_nodes)
    expect_is(test, "dispRity")
    results <- table(as.vector(unlist(lapply(test$subsets, function(X) return(X[[2]])))))
    expect_equal( #no nodes
        length(results), 49
        )

    ## TODO
    # slice_cont <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", time = c(100, 60), model = "gradual.split", inc.nodes = TRUE, BeckLee_ages, verbose = FALSE, t0 = FALSE)


})