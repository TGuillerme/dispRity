 # - 1. metrics can now have `RAM.help` arguments that intake a function that will run some pre-calculations. For example, this function can be `vegan::vegdist`.
 # - 2. detect the need for RAM help in `get.dispRity.metric.handle`
 # - 3. compute heavy calculations at the whole data level in `dispRity` using the `RAM.help` function before the `lapply_loop`
 # - 4. store the calculations in `data` similarly as tree as `RAM.helper`
 # - 5. run the metrics using a potential `RAM.helper` similarly as tree.


dist.with.help <- function(matrix, method = "euclidean", RAM.helper = vegan::vegdist) {
    ## Check for distance
    distances <- check.dist.matrix(matrix, method = method)[[1]]
    ## Return distances
    return(as.vector(distances))
}


test_that("make.metric handles help", {

    data <- make.dispRity(data = matrix(rnorm(90), 9, 10))

    ## Get the help from make.metric
    test <- make.metric(fun = dist.with.help, data.dim = data, get.help = TRUE, silent = TRUE)
    expect_is(test, "list")
    expect_equal(names(test), c("type", "tree", "RAM.help"))
    expect_is(test$RAM.help, "dist")

    #NEXT STEP: feed make.metric to get.metric.handle in dispRity with get.help = TRUE

})

test_that("general structure works", {
    ## General pipeline works with RAM.helper


    # ## metric and data
    # dimensions = NULL
    # between.groups = FALSE
    # verbose = FALSE
    # tree = NULL
    # metric <- dist.with.help
    # data <- matrix(rnorm(90), 9, 10)








    # ## multiple metrics
    # metric = c(mean, dist.with.help)


})

test_that("works with help being an object", {

})

test_that("works with bootstraps", {

})

test_that("works with trees", {
    
})

test_that("works with covar", {
    
})

test_that("works with between groups", {

})

test_that("works with multiple matrices", {

})



test_that("works with dispRity multi", {

})

