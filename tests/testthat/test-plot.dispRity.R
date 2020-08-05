#TESTING plot.dispRity

context("plot.dispRity") 

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
                                  elements = FALSE,
                                  type = "continuous",
                                  observed_args = list(observed = TRUE, col = c("black", "blue")))
    expect_is(plot_params, "list")
    expect_equal(names(plot_params), c("disparity", "helpers", "options", "observed_args"))
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
    expect_equal_round(plot_params$options$ylim, c(2.248737, 2.918863), 6)
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
                                  rarefaction = FALSE,
                                  elements = FALSE,
                                  rarefaction_level = 10,
                                  type = "discrete",
                                  main = "main",
                                  observed_args = list(observed = FALSE))
    ## The plotting options
    expect_equal(dim(plot_params$disparity$names), c(7, 2))
    expect_equal(dim(plot_params$disparity$data), c(7, 10))
    expect_equal(plot_params$helpers$n_quantiles, 4)
    expect_equal(plot_params$options$xlab, "xlab")
    expect_equal(plot_params$options$ylab, c("ylab", "ylab2"))
    expect_equal_round(plot_params$options$ylim, c(1,2), 6)
    expect_equal(plot_params$options$col, c("orange", "blue", "#BEBEBE", "#C8C8C8", "#D3D3D3"))
    expect_equal(plot_params$options$main, "main")
})


# test_that("transpose.box works", {

#     expect_error(
#         transpose.box("disparity", rarefaction = FALSE, is_bootstrapped = TRUE)
#         )
#     expect_error(
#         transpose.box(disparity, rarefaction = 99, is_bootstrapped = TRUE)
#         )
#     expect_equal(
#         length(transpose.box(disparity, rarefaction = FALSE, is_bootstrapped = TRUE))
#         ,7)
#     expect_equal(
#         length(transpose.box(disparity, rarefaction = 5, is_bootstrapped = TRUE))
#         ,7)

#     test <- transpose.box(disparity, rarefaction = FALSE, is_bootstrapped = TRUE)
#     for(subsets in 1:length(disparity$subsets)) {
#         expect_equal(
#             as.vector(test[[subsets]])
#             ,as.vector(disparity$disparity[[subsets]][[2]]))
#     }
#     ## Unequal boxes test
#     data(BeckLee_mat99)
#     data(BeckLee_tree)
#     data <- dispRity(custom.subsets(BeckLee_mat99, group = crown.stem(BeckLee_tree)), metric = centroids)
#     subset_length <- unique(unlist(lapply(data$subsets, function(x) return(length(x[[1]])))))
#     names(subset_length) <- c("crown", "stem") 
#     test <- transpose.box(data, rarefaction = FALSE, is_bootstrapped = FALSE)
#     expect_is(test, "list")
#     expect_equal(names(test), c("crown", "stem"))
#     expect_equal(unlist(lapply(test, length)), subset_length)
# })

# test_that("split.summary.data works", {

#     data(disparity)
#     sum_data <- summary(disparity)
#     subsets <- unique(sum_data$subsets)

#     for(sub in 1:length(subsets)) {
#         ## Create a split
#         split <- split.summary.data(subsets[sub], sum_data)
#         ## test
#         expect_is(split, "data.frame")
#         expect_equal(dim(split), c(length(which(sum_data$subsets == subsets[sub])),8))
#     }
# })










# ## Function works

# test_that("plot.dispRity examples work", {

#     data(disparity)

#     ## No data
#     ordinated_matrix <- matrix(data = rnorm(90), nrow = 10)
#     expect_warning(data <- custom.subsets(ordinated_matrix, list(c(1:4), c(5:10))))

#     ## Rarefaction is ignored if no BS
#     expect_null(plot(dispRity(data, metric = mean), rarefaction = TRUE))
#     expect_null(plot(dispRity(data, metric = mean), type = "l"))
#     expect_null(plot(dispRity(data, metric = mean), type = "c"))

#     ## Discrete plotting
#     expect_null(plot(disparity, type = "box"))
#     expect_null(plot(disparity, type = "box", elements = TRUE))
#     expect_null(plot(disparity, type = "box", observed = TRUE))
#     expect_null(plot(disparity, type = "polygon", quantiles = c(0.1, 0.5, 0.95), cent.tend = mode.val))
#     expect_error(plot(disparity, type = "polygon", quantiles = c(10, 50, 110), cent.tend = mode.val))
#     expect_error(plot(disparity, type = "polygon", quantiles = c(10, 50), cent.tend = var))
#     expect_null(plot(disparity, type = "line", elements = TRUE, ylim = c(0, 5),xlab = ("Time (Ma)"), ylab = "disparity"))
#     expect_null(plot(disparity, type = "continuous"))
#     expect_null(plot(disparity, type = "continuous", chrono.subsets = FALSE, elements = TRUE, col = c("red", "orange", "yellow")))
#     expect_null(plot(disparity, rarefaction = TRUE, col = "blue"))
#     expect_null(plot(disparity, elements = TRUE))
#     data(BeckLee_mat50)
#     data(BeckLee_tree)
#     expect_null(plot(dispRity(boot.matrix(custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))), metric = c(sum, variances))))
#     expect_error(plot(disparity, rarefaction = 1))
#     expect_null(plot(disparity, rarefaction = 5))
#     expect_null(plot(disparity, observed = TRUE))
#     expect_null(plot(disparity, observed = list("pch" = 19, col = "blue", cex = 4)))

#     ## Testing additional behaviours for plot.discrete/continuous
#     expect_null(plot(disparity, rarefaction = 5, type = "l", col = c("blue", "orange")))
#     expect_null(plot(disparity, rarefaction = 5, type = "p", col = "blue", observed = TRUE))
#     expect_null(plot(disparity, type = "c", col = c("blue", "orange")))
#     expect_null(plot(disparity, density = 50))

#     ## Auto colouring of quantiles for discrete bins
#     data(BeckLee_tree) ; data(BeckLee_mat50)
#     test <- custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE))
#     test <- dispRity(boot.matrix(test), metric = c(sum, variances))
#     expect_null(plot(disparity, col = c("blue", "red", "orange"), quantiles = c(10, 20, 30, 40, 50, 60), type = "p"))

#     ## Some other tests
#     error <- capture_error(plot(disparity, ylab = c("blabla", "blu")))
#     expect_equal(error[[1]], "ylab must be a character string.")
#     error <- capture_error(plot(disparity, ylab = c("blabla", "blu", "1"), elements = TRUE))
#     expect_equal(error[[1]], "ylab can have maximum of two elements.")

# })

test_that("plot.dispRity continuous with NAs", {

    data(BeckLee_mat99)
    data(BeckLee_tree)
    test_data <- chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", model = "acctran", time = seq(from = 40, to = 80, by = 10))

    test_data$subsets$`60`$elements <- matrix(NA)
    expect_warning(test_data <- dispRity(boot.matrix(test_data, rarefaction = 3), c(sum, variances)))
    expect_null(plot(test_data))
})

# test_that("plot.dispRity.discrete with ADD", {

#     data(BeckLee_mat50) ; data(BeckLee_tree)
#     result <- dispRity.through.time(BeckLee_mat50, BeckLee_tree, 3)

#     expect_null(plot(result, type = "polygon"))
#     expect_null(plot(result,  type = "line", add = TRUE, col = "blue", quantiles = c(5, 10, 15)))
# })

# test_that("plot.dispRity with model.test data", {
#     load("model_test_data.Rda")

#     ## Run two models (silent)
#     models <- list("BM", "OU")
#     set.seed(42)
#     tested_models <- model.test(model_test_data, models, time.split = 65, fixed.optima = TRUE, verbose = FALSE)
#     summary_model.tests <- summary(tested_models)

#     expect_null(plot(tested_models))

#     ## Testing normal model
#     model_simulation_empty <- model.test.sim(sim = 10, model = "BM")
#     expect_null(plot(model_simulation_empty))

#     ## Testing inherited model
#     set.seed(42)
#     model_simulation_inherit <- model.test.sim(sim = 10, model = tested_models)
#     expect_null(plot(model_simulation_inherit))

#     ## Works with adding the plot
#     expect_null(plot(model_test_data))
#     expect_null(plot(model_simulation_inherit, add = TRUE))
# })

# test_that("plot subclasses works", {
  
#     ## Randtest
#     data(BeckLee_mat50)
#     ## Calculating the disparity as the ellipsoid volume
#     obs_disparity <- dispRity(BeckLee_mat50, metric = ellipse.volume)
#     ## Testing against normal distribution
#     expect_warning(results <- null.test(obs_disparity, replicates = 2, null.distrib = rnorm))
#     expect_is(results, c("dispRity", "randtest"))
#     expect_null(plot(results))

#     ## DTT
#     ## Loading geiger's example data set
#     require(geiger)
#     geiger_data <- get(data(geospiza))
#     average.sq <- function(X) mean(pairwise.dist(X)^2)
#     expect_warning(dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 2))

#     ## Plotting the results
#     expect_null(plot(dispRity_dtt, quantiles = c(0.1, 0.95)))
#     expect_error(plot(dispRity_dtt, quantiles = c(10, 110)))
#     expect_error(plot(dispRity_dtt, cent.tend = var))
# })

# test_that("plot preview works", {
#     data(BeckLee_mat99)
#     data(BeckLee_tree)
#     data_cust <- custom.subsets(BeckLee_mat99, crown.stem(BeckLee_tree, inc.nodes = TRUE))
#     data_slice <- chrono.subsets(BeckLee_mat99, tree = BeckLee_tree, method = "discrete", time = 5)

#     expect_null(plot.preview(data_cust, dimensions = c(1,2), matrix = 1))
#     expect_null(plot.preview(data_slice, dimensions = c(1,2), matrix = 1))
#     expect_null(plot(data_cust))
#     expect_null(plot(data_slice, type = "preview", dimensions = c(38, 22), main = "Ha!"))
#     expect_error(plot(data_slice, type = "p"))
# })