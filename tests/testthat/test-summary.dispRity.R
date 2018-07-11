# TESTING summary.dispRity

context("summary.dispRity")
 
# Loading the data
load("test_data.Rda")
data <- test_data$ord_data_tips
data(disparity)

#######################
#Internal
#######################

test_that("CI.converter", {
    expect_equal(CI.converter(c(50,75)), c(0.125, 0.250, 0.750, 0.875))
    expect_equal(CI.converter(c(75,50)), c(0.125, 0.250, 0.750, 0.875))
})

test_that("get.summary", {
    test <- get.summary(disparity$disparity$`30`[[2]], mean, c(50))
    expect_is(test, "list")
    expect_equal(names(test), c("cent_tend", "quantiles"))
    expect_equal(round(test[[1]], digit = 5), round(mean(unlist(disparity$disparity$`30`[[2]])), digit = 5))
    expect_equal(round(test[[2]], digit = 5), c("25%" = 1.78386, "75%" = 1.87866))

    test_no_cent_tend <- get.summary(disparity$disparity$`30`[[2]], quantiles = c(50))
    expect_is(test_no_cent_tend, "list")
    expect_equal(names(test_no_cent_tend), "quantiles")
    expect_equal(round(test_no_cent_tend[[1]], digit = 5), c("25%" = 1.78386, "75%" = 1.87866))

    test_no_quant <- get.summary(disparity$disparity$`30`[[2]], cent.tend = mean)
    expect_is(test_no_quant, "list")
    expect_equal(names(test_no_quant), "cent_tend")
    expect_equal(round(test_no_quant[[1]], digit = 5), round(mean(unlist(disparity$disparity$`30`[[2]])), digit = 5))

})

test_that("lapply.summary", {
    test <- lapply.summary(disparity$disparity$`30`, mean, 50)
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(unique(unlist(lapply(test, names))), c("cent_tend", "quantiles"))
    expect_equal(as.vector(round(unlist(lapply(test, `[[`, 1)), digit = 5)), c(round(mean(unlist(disparity$disparity$`30`[[2]])), digit = 5), round(mean(unlist(disparity$disparity$`30`[[3]])), digit = 5)))

})

test_that("lapply.get.elements", {
    test_nobs <- as.vector(lapply.get.elements(disparity$subsets[[1]], bootstrapped = FALSE))
    test_bs <- as.vector(lapply.get.elements(disparity$subsets[[1]], bootstrapped = TRUE))
    expect_is(test_nobs, "integer")
    expect_is(test_bs, "integer")
    expect_equal(test_nobs, c(18,18,15,10,5))
    expect_equal(test_bs, c(18,15,10,5))
})

test_that("lapply.observed", {
    expect_equal(lapply.observed(disparity$disparity[[1]]), as.vector(disparity$disparity[[1]]$elements))
})

test_that("mapply.observed", {
    elements <- lapply.get.elements(disparity$subsets[[1]])
    disparity_value <- lapply.observed(disparity$disparity[[1]])
    expect_equal(mapply.observed(disparity_value, elements), c(disparity_value, rep(NA,3)))
})

test_that("get.digit", {
    ## Shifts the point to contain maximum 4 characters
    expect_equal(get.digit(1.123), 3)
    expect_equal(get.digit(1.123456789), 3)
    expect_equal(get.digit(12.123456789), 2)
    expect_equal(get.digit(123.123456789), 1)
    expect_equal(get.digit(1234.123456789), 0)
})


test_that("round.column", {
    column <- c(12.123, 1.1234)
    expect_equal(round.column(column, digits = "default"), c(12.12, 1.12))
    expect_equal(round.column(column, digits = 5), c(12.12300, 1.12340))
    expect_equal(round.column(column, digits = 1), c(12.1, 1.1))
    expect_equal(round.column(column, digits = 0), c(12, 1))
    expect_equal(round.column(column, digits = -1), c(10, 0))
})

test_that("digits.fun", {
    test <- matrix(c(1, 1, 123.123456), nrow = 1)
    expect_equal(digits.fun(test, digits = "default")[1,3], 123.1)
    expect_equal(digits.fun(test, digits = 3)[1,3], 123.123)
    expect_equal(digits.fun(test, digits = -2)[1,3], 100)
})

#######################
#Testing
#######################

# Errors
data(disparity)
test_that("Correct error management", {
    expect_error(summary(disparity, cent.tend = var))
    expect_error(summary(make.dispRity()))
    expect_error(summary(disparity, quantiles = c(0.1, 10)))
    expect_error(summary(disparity, quantiles = c(10, 101)))


    dummy <- disparity
    class(dummy) <- c("dispRity", "bob")
    expect_error(summary(dummy))

})

#Case 1, no bootstrap
data <- test_data$ord_data_tips
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works without bootstraps", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 1
        )
    expect_equal(
        ncol(test), 3
        )

    ## Recall works as well
    out <- capture.output(test <- summary(data, recall = TRUE))
    expect_equal(out,
        c(" ---- dispRity object ---- ",
          "50 elements with 48 dimensions.",
          "Disparity was calculated as: c(sum, ranges)."))

})

#Case 2, bootstraps
data <- test_data$ord_data_tips
data <- boot.matrix(data, bootstrap = 5)
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with bootstraps", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 1
        )
    expect_equal(
        ncol(test), 8
        )
})

#Case 3, bootstraps + rarefaction
data <- test_data$ord_data_tips
data <- boot.matrix(data, bootstrap = 5, rarefaction = c(5,50))
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with bootstraps and rarefaction", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 2
        )
    expect_equal(
        ncol(test), 8
        )
    expect_equal(
        test$obs
        , c(45.36, NA))
})

#Case 4, time subsets
data <- test_data$ord_data_tips
group <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(group) <- rownames(data)
data <- custom.subsets(data, group)
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with subsets", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 2
        )
    expect_equal(
        ncol(test), 3
        )
    expect_equal(
        as.vector(test$obs)
        ,c(37.00, 37.97))
})

#Case 5, time subsets + bootstraps
set.seed(1)
data <- test_data$ord_data_tips
group <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(group) <- rownames(data)
data <- custom.subsets(data, group)
data <- boot.matrix(data, bootstrap = 5)
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with subsets and bootstraps", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 2
        )
    expect_equal(
        ncol(test), 8
        )
    expect_equal(
        test$bs.median
        ,c(32.65, 34.09))
})

#Case 5, time subsets + bootstraps + rarefaction
set.seed(1)
data <- test_data$ord_data_tips
group <- as.data.frame(matrix(data = c(rep(1, nrow(data)/2),rep(2, nrow(data)/2)), nrow = nrow(data), ncol = 1))
rownames(group) <- rownames(data)
data <- custom.subsets(data, group)
data <- boot.matrix(data, bootstrap = 5, rarefaction = c(5,6))
data <- dispRity(data, metric = c(sum, ranges))
test <- summary(data)

#Test
test_that("Works with subsets, bootstraps and rarefaction", {
    expect_is(
        test, "data.frame"
        )
    expect_equal(
        nrow(test), 6
        )
    expect_equal(
        ncol(test), 8
        )
    expect_equal(
        test$obs
        , c(37.00, NA, NA, 37.97, NA, NA))
    expect_equal(
        test$bs.median
        , c(32.65, 20.39, 21.86, 33.75, 21.44, 23.33))
})

#Example
test_that("Example works", {
    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsets <- custom.subsets(BeckLee_mat50, groups)
    bootstrapped_data <- boot.matrix(customised_subsets, bootstraps=100)
    sum_of_ranges <- dispRity(bootstrapped_data, metric=c(sum, ranges))
    expect_is(
        summary(sum_of_ranges), "data.frame"
        )
    expect_equal(
        dim(summary(sum_of_ranges)), c(2,8)
        )
    expect_is(
        summary(sum_of_ranges, quantile=75, cent.tend=median, digits=0), "data.frame"
        )
    expect_equal(
        dim(summary(sum_of_ranges, quantile=75, cent.tend=median, digits=0)), c(2,8)
        )
})

#Testing with distributions
test_that("Test with disparity as a distribution", {
    groups <- as.data.frame(matrix(data = c(rep(1, nrow(BeckLee_mat50)/2), rep(2, nrow(BeckLee_mat50)/2)), nrow = nrow(BeckLee_mat50), ncol = 1, dimnames = list(rownames(BeckLee_mat50))))
    customised_subsets <- custom.subsets(BeckLee_mat50, groups)
    sum_of_ranges1 <- dispRity(customised_subsets, metric=ranges)
    bootstrapped_data <- boot.matrix(customised_subsets, bootstraps=100)
    sum_of_ranges2 <- dispRity(bootstrapped_data, metric=ranges)

    expect_is(
        summary(sum_of_ranges1), "data.frame"
        )
    expect_is(
        summary(sum_of_ranges2), "data.frame"
        )

    expect_equal(
        dim(summary(sum_of_ranges1))
        , c(2,7))
    expect_equal(
        dim(summary(sum_of_ranges2)), c(2,8)
        )
})

## summary.dispRity works with empty or small (<3 subsets)
test_that("summary.dispRity works with small, empty/subsets", {

    load("test_data.Rda")
    tree <- test_data$tree_data
    data <- test_data$ord_data_tips_nodes
    FADLAD <- test_data$FADLAD_data

    silent <- capture_warnings(data <- dispRity(boot.matrix(chrono.subsets(data, tree, model = "deltran", method = "continuous", time = c(140, 138, 130, 120, 100))), metric = c(sum, variances)))

    test <- summary(data)
    expect_equal(as.numeric(test[1,]), c(5, 0, rep(NA, 6)))
    expect_equal(as.numeric(test[2,]), c(4, 1, rep(NA, 6)))
    expect_false(all(is.na(test[3,])))
})




test_that("summary.dispRity with model.test data", {
    load("model_test_data.Rda")

    ## Run two models (silent)
    models <- list("BM", "OU")
    set.seed(42)
    tested_models <- model.test(model_test_data, models, time.split = 65, fixed.optima = TRUE, verbose = FALSE)
    summary_model.tests <- summary(tested_models)

    expect_is(summary_model.tests, "matrix")
    expect_equal(dim(summary_model.tests), c(2,8))
    expect_equal(colnames(summary_model.tests), c("aicc", "delta_aicc", "weight_aicc", "log.lik", "param", "ancestral state", "sigma squared", "alpha"))
    expect_equal(rownames(summary_model.tests), unlist(models))

    ## Testing normal model
    model_simulation_empty <- model.test.sim(sim = 10, model = "BM")
    summary_model.sim1 <- summary(model_simulation_empty)

    expect_is(summary_model.sim1, "matrix")
    expect_equal(dim(summary_model.sim1), c(100,8))
    expect_equal(colnames(summary_model.sim1), c("subsets", "n", "var", "median", "2.5%", "25%", "75%", "97.5%"))
    expect_equal(rownames(summary_model.sim1), as.character(model_simulation_empty$simulation.data$fix$subsets))

    ## Testing inherited model
    set.seed(42)
    model_simulation_inherit <- model.test.sim(sim = 10, model = tested_models)
    summary_model.sim2 <- summary(model_simulation_inherit)

    expect_is(summary_model.sim2, "matrix")
    expect_equal(dim(summary_model.sim2), c(25,8))
    expect_equal(colnames(summary_model.sim2), c("subsets", "n", "var", "median", "2.5%", "25%", "75%", "97.5%"))
    expect_equal(rownames(summary_model.sim2), as.character(rev(25:1)))
})


# test_that("Test seq.test object management", {
#     data(BeckLee_mat50)
#     groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12), rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
#     customised_subsets <- custom.subsets(BeckLee_mat50, groups)
#     bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 3)
#     sum_of_variances <- dispRity(bootstrapped_data, metric =  variances)
#     data_distribution <- sequential.test(extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = FALSE), family = gaussian)
#     data_concatenated <- sequential.test(extract.dispRity(sum_of_variances, observed = FALSE, keep.structure = TRUE, concatenate = TRUE), family = gaussian)

#     expect_is(
#         summary(data_distribution), "list"
#         )
#     expect_is(
#         summary(data_concatenated), "list"
#         )

#     expect_equal(
#         length(summary(data_distribution))
#         ,2)
#     expect_equal(
#         length(summary(data_concatenated))
#         ,2)

#     expect_equal(
#         names(summary(data_distribution))
#         ,c("Slopes","Intercepts"))
#     expect_equal(
#         names(summary(data_concatenated))
#         ,c("Slopes","Intercepts"))

#     #concatenated results are two matrices
#     expect_equal(
#         unique(unlist(lapply(summary(data_concatenated), class)))
#         ,"matrix")

#     #distribution results are two lists...
#     expect_equal(
#         unique(unlist(lapply(summary(data_distribution), class)))
#         ,"list")

#     #... of matrices
#     expect_equal(
#         unique(unlist(lapply(summary(data_distribution), lapply, class)))
#         ,"matrix")
# })