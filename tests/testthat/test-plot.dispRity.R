#TESTING plot.dispRity

context("plot.dispRity") 

## Loading the data
data("disparity")

#######################
#Testing
#######################

## Default settings
test_that("set.default works", {
    sum_data <- summary(disparity)
    expect_error(
        set.default("1", disparity, elements = FALSE, ylim = "default", xlab = "default", ylab = "default", col = "default", rarefaction = FALSE, is_bootstrapped = TRUE, is_between.groups = FALSE)
        )
    expect_error(
        set.default(sum_data, "1", elements = FALSE, ylim = "default", xlab = "default", ylab = "default", col = "default", rarefaction = FALSE, is_bootstrapped = TRUE, is_between.groups = FALSE)
        )
    test <- set.default(sum_data, disparity, elements = FALSE, ylim = "default", xlab = "default", ylab = "default", col = "default", rarefaction = FALSE, is_bootstrapped = TRUE, is_between.groups = FALSE)
    expect_equal(
        round(test[[1]], 3)
        , round(c(1.343, 2.930), 3))
    expect_equal(
        test[[2]]
        , "Subsets")
    expect_equal(
        test[[3]]
        , "c(median, centroids)")
    expect_equal(
        test[[4]]
        , c("black", "#BEBEBE", "#D3D3D3"))

    test <- set.default(sum_data, disparity, elements = TRUE, ylim = c(1,2), xlab = "Bla", ylab = c("Bli", "Blu"), col = c("pink", "knip"), rarefaction = FALSE, is_bootstrapped = TRUE, is_between.groups = FALSE)
    expect_equal(
        round(test[[1]], 5)
        , round(c(1, 2), 5))
    expect_equal(
        test[[2]]
        , "Bla")
    expect_equal(
        test[[3]]
        , c("Bli", "Blu"))
    expect_equal(
        test[[4]]
        , c("pink", "knip", "#BEBEBE"))
})

test_that("extract.from.summary works", {
    sum_data <- summary(disparity)
    expect_error(
        extract.from.summary("sum_data", what = 4, rarefaction = FALSE)
        )
    expect_null(
        extract.from.summary(sum_data, what = "4", rarefaction = FALSE)
        )
    expect_equal(
        length(extract.from.summary(sum_data, what = 4, rarefaction = 99))
        , 0)
    expect_equal(
        length(extract.from.summary(sum_data, what = 4, rarefaction = FALSE))
        , length(extract.from.summary(sum_data, what = "rows", rarefaction = FALSE)))
    expect_equal(
        extract.from.summary(sum_data, what = 1, rarefaction = FALSE)
        , as.character(c(90,80,70,60,50,40,30)))
    expect_equal(
        extract.from.summary(sum_data, what = 2, rarefaction = 10)
        ,rep(10, 7))
    expect_equal(
        extract.from.summary(sum_data, what = 3, rarefaction = 5)
        ,as.numeric(rep(NA, 7)))
    expect_equal(
        round(extract.from.summary(sum_data, what = 4, rarefaction = FALSE), 2)
        ,c(2.66, 2.71, 2.75, 2.73, 2.76, 2.75, 2.63))
expect_equal(
        extract.from.summary(sum_data, what = "rows", rarefaction = 10)
        ,c(3, 8, 13, 18, 22, 25, 27))
})

test_that("transpose.box works", {

    expect_error(
        transpose.box("disparity", rarefaction = FALSE, is_bootstrapped = TRUE)
        )
    expect_error(
        transpose.box(disparity, rarefaction = 99, is_bootstrapped = TRUE)
        )
    expect_equal(
        length(transpose.box(disparity, rarefaction = FALSE, is_bootstrapped = TRUE))
        ,7)
    expect_equal(
        length(transpose.box(disparity, rarefaction = 5, is_bootstrapped = TRUE))
        ,7)

    test <- transpose.box(disparity, rarefaction = FALSE, is_bootstrapped = TRUE)
    for(subsets in 1:length(disparity$subsets)) {
        expect_equal(
            as.vector(test[[subsets]])
            ,as.vector(disparity$disparity[[subsets]][[2]]))
    }
    ## Unequal boxes test
    data(BeckLee_mat99)
    data(BeckLee_tree)
    data <- dispRity(custom.subsets(BeckLee_mat99, group = crown.stem(BeckLee_tree)), metric = centroids)
    subset_length <- unique(unlist(lapply(data$subsets, function(x) return(length(x[[1]])))))
    names(subset_length) <- c("crown", "stem") 
    test <- transpose.box(data, rarefaction = FALSE, is_bootstrapped = FALSE)
    expect_is(test, "list")
    expect_equal(names(test), c("crown", "stem"))
    expect_equal(unlist(lapply(test, length)), subset_length)
})

test_that("split.summary.data works", {

    data(disparity)
    sum_data <- summary(disparity)
    subsets <- unique(sum_data$subsets)

    for(sub in 1:length(subsets)) {
        ## Create a split
        split <- split.summary.data(subsets[sub], sum_data)
        ## test
        expect_is(split, "data.frame")
        expect_equal(dim(split), c(length(which(sum_data$subsets == subsets[sub])),8))
    }
})

test_that("plot.dispRity examples work", {

    data(disparity)

    ## No data
    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10)
    expect_warning(data <- custom.subsets(ordinated_matrix, list(c(1:4), c(5:10))))

    ## Rarefaction is ignored if no BS
    expect_null(plot(dispRity(data, metric = mean), rarefaction = TRUE))
    expect_null(plot(dispRity(data, metric = mean), type = "l"))
    expect_null(plot(dispRity(data, metric = mean), type = "c"))

    ## Discrete plotting
    expect_null(plot(disparity, type = "box"))
    expect_null(plot(disparity, type = "box", elements = TRUE))
    expect_null(plot(disparity, type = "box", observed = TRUE))
    expect_null(plot(disparity, type = "polygon", quantiles = c(0.1, 0.5, 0.95), cent.tend = mode.val))
    expect_error(plot(disparity, type = "polygon", quantiles = c(10, 50, 110), cent.tend = mode.val))
    expect_error(plot(disparity, type = "polygon", quantiles = c(10, 50), cent.tend = var))
    expect_null(plot(disparity, type = "line", elements = TRUE, ylim = c(0, 5),xlab = ("Time (Ma)"), ylab = "disparity"))
    expect_null(plot(disparity, type = "continuous"))
    expect_null(plot(disparity, type = "continuous", chrono.subsets = FALSE, elements = TRUE, col = c("red", "orange", "yellow")))
    expect_null(plot(disparity, rarefaction = TRUE, col = "blue"))
    expect_null(plot(disparity, elements = TRUE))
    data(BeckLee_mat50)
    data(BeckLee_tree)
    expect_null(plot(dispRity(boot.matrix(custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))), metric = c(sum, variances))))
    expect_error(plot(disparity, rarefaction = 1))
    expect_null(plot(disparity, rarefaction = 5))
    expect_null(plot(disparity, observed = TRUE))
    expect_null(plot(disparity, observed = list("pch" = 19, col = "blue", cex = 4)))

    ## Testing additional behaviours for plot.discrete/continuous
    expect_null(plot(disparity, rarefaction = 5, type = "l", col = c("blue", "orange")))
    expect_null(plot(disparity, rarefaction = 5, type = "p", col = "blue", observed = TRUE))
    expect_null(plot(disparity, type = "c", col = c("blue", "orange")))
    expect_null(plot(disparity, density = 50))

    ## Auto colouring of quantiles for discrete bins
    data(BeckLee_tree) ; data(BeckLee_mat50)
    test <- custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))
    test <- dispRity(boot.matrix(test), metric = c(sum, variances))
    expect_null(plot(disparity, col = c("blue", "red", "orange"), quantiles = c(10, 20, 30, 40, 50, 60), type = "p"))

    ## Some other tests
    error <- capture_error(plot(disparity, ylab = c("blabla", "blu")))
    expect_equal(error[[1]], "ylab must be a character string.")
    error <- capture_error(plot(disparity, ylab = c("blabla", "blu", "1"), elements = TRUE))
    expect_equal(error[[1]], "ylab can have maximum of two elements.")

})

test_that("plot.dispRity continuous with NAs", {

    data(BeckLee_mat99)
    data(BeckLee_tree)
    test_data <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "acctran", time = seq(from = 40, to = 80, by = 10))

    test_data$subsets$`60`$elements <- matrix(NA)
    expect_warning(test_data <- dispRity(boot.matrix(test_data), c(sum, variances)))
    expect_null(plot(test_data)) 
})

test_that("plot.dispRity.discrete with ADD", {

    data(BeckLee_mat50) ; data(BeckLee_tree)
    result <- dispRity.through.time(BeckLee_mat50, BeckLee_tree, 3)

    expect_null(plot(result, type = "polygon"))
    expect_null(plot(result,  type = "line", add = TRUE, col = "blue", quantiles = c(5, 10, 15)))
})

test_that("plot.dispRity with model.test data", {
    load("model_test_data.Rda")

    ## Run two models (silent)
    models <- list("BM", "OU")
    set.seed(42)
    tested_models <- model.test(model_test_data, models, time.split = 65, fixed.optima = TRUE, verbose = FALSE)
    summary_model.tests <- summary(tested_models)

    expect_null(plot(tested_models))

    ## Testing normal model
    model_simulation_empty <- model.test.sim(sim = 10, model = "BM")
    expect_null(plot(model_simulation_empty))

    ## Testing inherited model
    set.seed(42)
    model_simulation_inherit <- model.test.sim(sim = 10, model = tested_models)
    expect_null(plot(model_simulation_inherit))

    ## Works with adding the plot
    expect_null(plot(model_test_data))
    expect_null(plot(model_simulation_inherit, add = TRUE))
})

test_that("plot subclasses works", {
  
    ## Randtest
    data(BeckLee_mat50)
    ## Calculating the disparity as the ellipsoid volume
    obs_disparity <- dispRity(BeckLee_mat50, metric = ellipse.volume)
    ## Testing against normal distribution
    expect_warning(results <- null.test(obs_disparity, replicates = 2, null.distrib = rnorm))
    expect_is(results, c("dispRity", "randtest"))
    expect_null(plot(results))

    ## DTT
    ## Loading geiger's example data set
    require(geiger)
    geiger_data <- get(data(geospiza))
    average.sq <- function(X) mean(pairwise.dist(X)^2)
    expect_warning(dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 2))

    ## Plotting the results
    expect_null(plot(dispRity_dtt, quantiles = c(0.1, 0.95)))
    expect_error(plot(dispRity_dtt, quantiles = c(10, 110)))
    expect_error(plot(dispRity_dtt, cent.tend = var))
})

test_that("plot preview works", {
    data(BeckLee_mat99)
    data(BeckLee_tree)
    data_cust <- custom.subsets(BeckLee_mat99, crown.stem(BeckLee_tree, inc.nodes = TRUE))
    data_slice <- chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 5)

    expect_null(plot.preview(data_cust, dimensions = c(1,2), matrix = 1))
    expect_null(plot.preview(data_slice, dimensions = c(1,2), matrix = 1))
    expect_null(plot(data_cust))
    expect_null(plot(data_slice, type = "preview", dimensions = c(38, 22), main = "Ha!"))
    expect_error(plot(data_slice, type = "p"))
})