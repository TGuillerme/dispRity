#TESTING plot.dispRity

#context("plot.dispRity") 

## Loading the data
data("disparity")

#######################
#Testing
#######################
test_that("get.data.params works", {
    test <- get.data.params(disparity)
    expect_is(test, "list")
    expect_equal(names(test), c("distribution", "bootstrap", "rarefaction", "between.groups", "elements"))
})

test_that("get.plot.params works", {

    ## All defaults
    plot_params <- get.plot.params(data = disparity,
                                  data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50,95),
                                  rarefaction_level = NULL,
                                  type = "continuous",
                                  observed_args = list(observed = TRUE, col = c("black", "blue")),
                                  elements_args = list(elements = FALSE))
    
    expect_is(plot_params, "list")
    expect_equal(names(plot_params), c("disparity", "helpers", "options", "observed_args", "elements_args"))
    ## The data to plot
    expect_equal(names(plot_params$disparity), c("names", "data"))
    expect_is(plot_params$disparity$data, "data.frame")
    expect_is(plot_params$disparity$names, "data.frame")
    expect_equal(dim(plot_params$disparity$names), c(7, 2))
    expect_equal(dim(plot_params$disparity$data), c(7, 6))
    ## The plotting helpers
    expect_equal(plot_params$helpers$n_quantiles, 2)
    expect_equal(plot_params$helpers$n_points, 7)
    ## The plotting options
    expect_equal(plot_params$options$xlab, "Time (Mya)")
    expect_equal(plot_params$options$ylab, "c(median, centroids)")
    expect_equal_round(plot_params$options$ylim, c(1.516207, 1.971640), 6)
    expect_equal(plot_params$options$col, c("black", "#BEBEBE", "#D3D3D3"))
    ## Observed data
    expect_equal(names(plot_params$observed_args), c("observed", "col", "names", "data", "pch", "cex"))
    expect_true(plot_params$observed_args$observed)
    expect_equal(dim(plot_params$observed_args$names), c(7,2))
    expect_equal(dim(plot_params$observed_args$data), c(7,6))
    expect_equal(plot_params$observed_args$col, c("black", "blue"))
    expect_equal(plot_params$observed_args$pch, 4)
    expect_equal(plot_params$observed_args$cex, 1)


    ## Options handled correctly
    plot_params <- get.plot.params(data = disparity, data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50, 75, 95, 99),
                                  ylim = c(1,2),
                                  xlab = "xlab",
                                  ylab = c("ylab", "ylab2"),
                                  col  = c("orange", "blue"),
                                  rarefaction_level = 10,
                                  type = "discrete",
                                  main = "main",
                                  observed_args = list(observed = FALSE),
                                  elements_args = list(elements = TRUE, col = "grey", cex = 22))
    ## The plotting options
    expect_equal(dim(plot_params$disparity$names), c(7, 2))
    expect_equal(dim(plot_params$disparity$data), c(7, 10))
    expect_equal(plot_params$helpers$n_quantiles, 4)
    expect_equal(plot_params$options$xlab, "xlab")
    expect_equal(plot_params$options$ylab, c("ylab", "ylab2"))
    expect_equal_round(plot_params$options$ylim, c(1,2), 6)
    expect_equal(plot_params$options$col, c("orange", "blue", "#BEBEBE", "#C8C8C8", "#D3D3D3"))
    expect_equal(plot_params$options$main, "main")
    expect_true(plot_params$elements_args$elements)
    expect_equal(plot_params$elements_args$lty, c(2, 2))
    expect_equal(plot_params$elements_args$col, c("grey", "grey"))
    expect_equal(plot_params$elements_args$cex, 22)
    expect_equal(plot_params$elements_args$pch, c(15, 15))

    plot_params <- get.plot.params(data = disparity, data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50, 75, 95, 99),
                                  ylim = c(1,2),
                                  xlab = "xlab",
                                  ylab = c("ylab", "ylab2"),
                                  col  = c("orange", "blue"),
                                  rarefaction_level = 10,
                                  type = "discrete",
                                  main = "main",
                                  observed_args = list(observed = FALSE),
                                  elements_args = list(elements = TRUE, col = "grey", cex = 22, pch = 16, lty = 3))
    expect_true(plot_params$elements_args$elements)
    expect_equal(plot_params$elements_args$lty, c(3, 3))
    expect_equal(plot_params$elements_args$col, c("grey", "grey"))
    expect_equal(plot_params$elements_args$cex, 22)
    expect_equal(plot_params$elements_args$pch, c(16, 16))


    ## Boxplot data
    plot_params <- get.plot.params(data = disparity, data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50, 75, 95, 99),
                                  elements_args = list(elements = FALSE),
                                  rarefaction_level = NULL,
                                  type = "box",
                                  main = "main",
                                  observed_args = list(observed = FALSE))
    expect_is(plot_params$disparity$data, c("list"))
    expect_equal(unname(unlist(lapply(plot_params$disparity$data, length))), rep(100, 7))

    plot_params <- get.plot.params(data = disparity, data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50, 75, 95, 99),
                                  elements_args = list(elements = FALSE),
                                  rarefaction_level = NULL,
                                  type = "box",
                                  main = "main",
                                  observed_args = list(observed = TRUE))
    expect_is(plot_params$disparity$data, c("list"))
    expect_equal(unname(unlist(lapply(plot_params$disparity$data, length))), rep(100, 7))

    error <- capture_error(get.plot.params(data = disparity, data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50, 75, 95, 99),
                                  elements_args = list(elements = FALSE),
                                  rarefaction_level = NULL,
                                  type = "box",
                                  main = "main",
                                  observed_args = list(observed = TRUE),
                                  ylab = c("1","2", "3")))
    expect_equal(error[[1]], "ylab can have maximum of two elements.")
})

test_that("get.dots works", {
    ## Default
    my_args <- list(something = 1)
    expect_equal(my_args$something, 1)
    ## Empty dots
    dots <- list()
    test <- get.dots(dots, my_args, "anything", 2)
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    test <- get.dots(dots, test, "bob", 3)
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    expect_equal(test$bob, 3)
    test <- get.dots(dots, test, "def")
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    expect_equal(test$bob, 3)
    expect_null(test$def)

    ## Some args
    dots <- list("bob" = 4)
    test <- get.dots(dots, my_args, "anything", 2)
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    test <- get.dots(dots, test, "bob", 3)
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    expect_equal(test$bob, 4)
    test <- get.dots(dots, test, "def")
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    expect_equal(test$bob, 4)
    expect_null(test$def)

    ## Some args with fun
    dots <- list("bib.bob" = 5)
    test <- get.dots(dots, my_args, "anything", 2, fun = "bib")
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    test <- get.dots(dots, test, "bob", 3, fun = "bib")
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    expect_equal(test$bob, 5)
    test <- get.dots(dots, test, "def", fun = "bib")
    expect_equal(test$something, 1)
    expect_equal(test$anything, 2)
    expect_equal(test$bob, 5)
    expect_null(test$def)
    test <- get.dots(dots, my_args, "bob", 1000)
    expect_equal(test$bob, 1000)
})

test_that("get.shift works", {
    ## Dummy plot_params
    plot_params <- list(helpers = list(n_points = 8))

    test <- get.shift(add = FALSE, plot_params)
    expect_equal(test, 0)

    par("xaxp" = c(1, 1, 1))
    test <- get.shift(add = TRUE, plot_params)
    expect_equal(test, 0.5)
})

test_that("get.quantile.col works", {
    expect_equal(get.quantile.col(1, 1, 1), c(2, 3))
    expect_equal(get.quantile.col(2, 1, 1), c(3, 4))
    expect_equal(get.quantile.col(1, 2, 1), c(3, 2))
    expect_equal(get.quantile.col(2, 2, 1), c(4, 3))
    expect_equal(get.quantile.col(1, 1, 2), c(2, 5))
    expect_equal(get.quantile.col(1, 2, 2), c(3, 4))
})

test_that("do.plot.observed works", {

    ## Set plot params
    plot_params <- get.plot.params(data = disparity,
                                  data_params = get.data.params(disparity),
                                  cent.tend = median,
                                  quantiles = c(50,95),
                                  rarefaction_level = NULL,
                                  elements_args = list(elements = FALSE),
                                  type = "continuous",
                                  observed_args = list(observed = TRUE, col = c("black", "blue")))
    plot(1)
    expect_null(do.plot.observed(plot_params))

    plot(1)
    plot_params <- list(observed_args = list(observed = FALSE))
    expect_null(do.plot.observed(plot_params))

})

## Function works
test_that("plot.dispRity examples work", {

    data(disparity)

    ## No data
    ordinated_matrix <- matrix(data = rnorm(90), nrow = 10)
    expect_warning(data <- custom.subsets(ordinated_matrix, list(c(1:4), c(5:10))))

    ## Rarefaction is ignored if no BS
    error <- capture_error(plot(dispRity(data, metric = mean), rarefaction = TRUE))
    expect_null(plot(dispRity(data, metric = mean), type = "l"))
    expect_null(plot(dispRity(data, metric = mean), type = "c"))

    ## Discrete plotting
    expect_null(plot(disparity, type = "box"))
    expect_null(plot(disparity, type = "box", elements = TRUE))
    expect_null(plot(disparity, type = "box", elements = list(ylab = "not elements")))
    expect_null(plot(disparity, type = "box", observed = TRUE))
    expect_null(plot(disparity, type = "box", rarefaction = 5))
    expect_null(plot(disparity, type = "polygon", quantiles = c(0.1, 0.5, 0.95), cent.tend = mode.val))
    expect_null(plot(disparity, type = "polygon", quantiles = c(0.1, 0.5, 0.95), cent.tend = mode.val, pch = 2))
    expect_error(plot(disparity, type = "polygon", quantiles = c(10, 50, 110), cent.tend = mode.val))
    expect_error(plot(disparity, type = "polygon", quantiles = c(10, 50), cent.tend = var))
    expect_null(plot(disparity, type = "line", elements = TRUE, ylim = c(0, 5),xlab = ("Time (Ma)"), ylab = "disparity"))
    expect_null(plot(disparity, type = "continuous"))
    expect_null(plot(disparity, type = "continuous", elements = TRUE, col = c("red", "orange", "yellow")))
    expect_null(plot(disparity, elements = TRUE))
    data(BeckLee_mat50)
    data(BeckLee_tree)
    expect_null(plot(dispRity(boot.matrix(custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))), metric = c(sum, variances))))
    expect_error(plot(disparity, rarefaction = 1))
    expect_null(plot(disparity, rarefaction = 5))
    expect_null(plot(disparity, observed = TRUE))
    expect_null(plot(disparity, observed = list("pch" = 19, col = "blue", cex = 4)))


    ## Rarefaction plotting:
    test <- custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))
    test <- dispRity(boot.matrix(test, rarefaction = TRUE), metric = c(sum, variances))
    test_wrong <- dispRity(boot.matrix(BeckLee_mat50, rarefaction = 5), metric = c(sum, variances))
    expect_null(plot(test, rarefaction = TRUE))
    expect_null(plot(disparity, rarefaction = TRUE, col = "blue"))
    error <- capture_error(plot(test_wrong, rarefaction = TRUE, col = "blue"))
    expect_equal(error[[1]], "Impossible to plot rarefaction curves with only one level of rarefaction. Try to use plot(..., rarefaction = 5) to just see the rarefied data for that level instead.")

    ## Testing additional behaviours for do.plot.discrete/continuous
    expect_null(plot(disparity, rarefaction = 5, type = "l", col = c("blue", "orange")))
    expect_null(plot(disparity, rarefaction = 5, type = "p", col = "blue", observed = TRUE))
    expect_null(plot(disparity, type = "c", col = c("blue", "orange")))
    expect_null(plot(disparity, density = 50))

    ## Auto colouring of quantiles for discrete bins
    data(BeckLee_tree) ; data(BeckLee_mat50)
    test <- custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))
    test <- dispRity(boot.matrix(test), metric = c(sum, variances))
    expect_null(plot(disparity, col = c("blue", "red", "orange"), quantiles = c(10, 20, 30, 40, 50, 60), type = "p"))
})

test_that("plot.dispRity continuous with NAs", {

    data(BeckLee_mat99)
    data(BeckLee_tree)
    test_data <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "acctran", time = seq(from = 40, to = 80, by = 10))

    test_data$subsets$`60`$elements <- matrix(NA)
    expect_warning(test_data <- dispRity(boot.matrix(test_data, rarefaction = 3), c(sum, variances)))
    expect_null(plot(test_data))
})

test_that("plot.dispRity.discrete with ADD", {

    data(BeckLee_mat50) ; data(BeckLee_tree)
    result <- dispRity.through.time(BeckLee_mat50, BeckLee_tree, 3)

    expect_null(plot(result, type = "polygon"))
    expect_null(plot(result,  type = "line", add = TRUE, col = "blue", quantiles = c(5, 10, 15)))
})

test_that("plot.dispRity with preview", {
    ## Preview
    data(BeckLee_mat99)
    data(BeckLee_tree)
    data_cust <- custom.subsets(BeckLee_mat99, crown.stem(BeckLee_tree, inc.nodes = TRUE))
    data_slice <- chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 5)

    expect_null(do.plot.preview(data_cust, specific.args = list(dimensions = c(1,2), matrix = 1)))
    expect_null(do.plot.preview(data_slice, specific.args = list(dimensions = c(1,2), matrix = 1)))
    expect_null(plot(data_cust))
    expect_null(plot(data_slice, type = "preview", specific.args = list(dimensions = c(38, 22)), main = "Ha!"))
    expect_null(plot(data_slice, type = "preview", legend = FALSE, main = "Ha!"))
    expect_null(plot(data_slice, type = "preview", legend.x = 0, legend.y = 0, main = "Ha!"))
    error <- capture_error(plot(data_slice, type = "p"))
    expect_equal(error[[1]], "data_slice must contain disparity data.\nTry running dispRity(data_slice, ...)")
    expect_null(plot.dispRity(x = matrix(rnorm(50), 25, 2)))
})

test_that("plot.dispRity with randtest data", {
    ## Randtest
    data(BeckLee_mat50)
    data(BeckLee_tree)
    data_cust <- custom.subsets(BeckLee_mat99, crown.stem(BeckLee_tree, inc.nodes = TRUE))
    ## Calculating the disparity as the ellipsoid volume
    one_group <- dispRity(BeckLee_mat50, metric = c(sum, centroids))
    two_groups <- dispRity(data_cust, metric = c(sum, centroids))
    ## Testing against normal distribution
    expect_warning(results_one <- null.test(one_group, replicates = 2, null.distrib = rnorm))
    expect_warning(results_two <- null.test(two_groups, replicates = 2, null.distrib = runif))
    expect_is(results_one, c("dispRity", "randtest"))
    expect_is(results_two, c("dispRity", "randtest"))
    expect_null(plot(results_one))
    expect_null(plot(results_one, main = "hahaha", col = "blue"))
    expect_null(plot(results_two))
    expect_null(plot(results_two, main = "same"))
    expect_null(plot(results_two, legend = FALSE))
    expect_null(plot(results_two, points.col = "red", lty = 2, legend.x = "bottomright", legend.bty = "o"))
})

test_that("plot.dispRity with dtt data", {
    ## DTT
    ## Loading geiger's example data set
    # require(geiger)
    # geiger_data <- get(data(geospiza))
    data(BeckLee_mat50)
    data(BeckLee_tree)
    average.sq <- function(X) mean(pairwise.dist(X)^2)
    dispRity_dtt <- dtt.dispRity(data = BeckLee_mat50, metric = average.sq, tree = BeckLee_tree, nsim = 2)

    ## Plotting the results
    expect_null(plot(dispRity_dtt, quantiles = c(0.1, 0.95)))
    expect_null(plot(dispRity_dtt, quantiles = c(0.1, 0.95), main = "ho!", lines.col = "blue", lwd = 5))
    expect_error(plot(dispRity_dtt, quantiles = c(10, 110)))
    expect_error(plot(dispRity_dtt, cent.tend = var))
})

test_that("plot.dispRity with model.test data", {
    load("model_test_data.rda")
    ## Run two models (silent)
    models <- list("BM", "OU")
    set.seed(42)
    tested_models <- model.test(model_test_data, models, time.split = 65, fixed.optima = TRUE, verbose = FALSE)
    expect_null(plot(tested_models, col = c("blue", "pink"), main = "ho", lwd = 2))

    ## Testing normal model
    model_simulation_empty <- model.test.sim(sim = 10, model = "BM")
    expect_null(plot(model_simulation_empty, density = 1))

    ## Testing inherited model
    set.seed(42)
    model_simulation_inherit <- model.test.sim(sim = 10, model = tested_models)
    expect_null(plot(model_simulation_inherit))

    ## Works with adding the plot
    expect_null(plot(model_test_data))
    expect_null(plot(model_simulation_inherit, add = TRUE))
})

test_that("preview works with fuzzy matrices and trees", {

  ## Get some bound data
  load("bound_test_data.rda")
  data <- bound_test_data$matrices
  ## Simple plot
  expect_null(plot(make.dispRity(data[[1]])))
  expect_null(plot(make.dispRity(data)))

  ## Plot with groups
  data <- custom.subsets(data, group = list(tips = bound_test_data$tree[[1]]$tip.label, nodes = bound_test_data$tree[[1]]$node.label))

  ## Plot with one tree
  expect_null(plot(data))
  expect_null(plot(data, specific.args = list(matrix = 1)))

  ## Plot with multiple trees
  data <- bound_test_data$matrices
  tree <- bound_test_data$trees
  expect_null(plot(make.dispRity(data[[1]], tree[[1]]), specific.args = list(tree = TRUE)))
  expect_null(plot(make.dispRity(data[[1]], tree), specific.args = list(tree = TRUE)))
  expect_null(plot(make.dispRity(data, tree[[1]]), specific.args = list(tree = TRUE)))
  expect_null(plot(make.dispRity(data, tree[[1]]), lty = 3, specific.args = list(tree = TRUE)))

  ## Plot with groups
  data <- custom.subsets(data, group = list(tips = bound_test_data$tree[[1]]$tip.label, nodes = bound_test_data$tree[[1]]$node.label), tree = tree)

  ## Plot with one tree
  expect_null(plot(data))
  expect_null(plot(data, specific.args = list(tree = TRUE)))
  expect_null(plot(data, specific.args = list(matrix = 1, tree = 1)))
})

test_that("get.center.scale.range gives the correct scales", {
    set.seed(1)
    ## X bigger
    xrange <- range(rnorm(10))
    yrange <- range(runif(10))
    test <- get.center.scale.range(xrange, yrange)
    expect_gt(diff(xrange), diff(yrange))
    expect_equal(diff(test$xlim), diff(test$ylim))

    yrange <- range(runif(10, max = 100))
    test <- get.center.scale.range(xrange, yrange)
    expect_lt(diff(xrange), diff(yrange))
    expect_equal(diff(test$xlim), diff(test$ylim))
})