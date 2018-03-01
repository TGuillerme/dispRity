# #TESTING null.test

# context("model.test")

# ## Select model data
# load("model_test_data.Rda")
# data <- model_test_data

# test_that("simple models work", {
#     set.seed(1)
#     test <- model.test(data, model = "BM", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

#     expect_equal(class(test), c("dispRity", "model.test"))
#     expect_equal(names(test), c("aic.models", "full.details", "call"))
#     expect_is(test[[1]], c("matrix"))
#     expect_is(test[[2]], c("list"))
#     expect_equal(round(test[[2]][[1]]$value, digit = 4), 10.0892)

#     set.seed(1)
#     test <- model.test(data, model = "Stasis", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

#     expect_equal(class(test), c("dispRity", "model.test"))
#     expect_equal(names(test), c("aic.models", "full.details", "call"))
#     expect_is(test[[1]], c("matrix"))
#     expect_is(test[[2]], c("list"))
#     expect_equal(round(test[[2]][[1]]$value, digit = 4), round(-15.11164, digit = 4))

#     set.seed(1)
#     test <- model.test(data, model = "Trend", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

#     expect_equal(class(test), c("dispRity", "model.test"))
#     expect_equal(names(test), c("aic.models", "full.details", "call"))
#     expect_is(test[[1]], c("matrix"))
#     expect_is(test[[2]], c("list"))
#     expect_equal(round(test[[2]][[1]]$value, digit = 4), round(12.77702, digit = 4))

#     set.seed(1)
#     test <- model.test(data, model = "OU", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

#     expect_equal(class(test), c("dispRity", "model.test"))
#     expect_equal(names(test), c("aic.models", "full.details", "call"))
#     expect_is(test[[1]], c("matrix"))
#     expect_is(test[[2]], c("list"))
#     expect_equal(round(test[[2]][[1]]$value, digit = 4), round(13.36439, digit = 4))

#     set.seed(1)
#     test <- model.test(data, model = "multi.OU", pool.variance = NULL, time.split = c(45, 65), fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

#     expect_equal(class(test), c("dispRity", "model.test"))
#     expect_equal(names(test), c("aic.models", "full.details", "call"))
#     expect_is(test[[1]], c("matrix"))
#     expect_is(test[[2]], c("list"))
#     expect_equal(round(test[[2]][[1]]$value, digit = 4), round(10.08924, digit = 4))

# })

# test_that("multiple.models work", {
#     models.to.test <- list("BM", "OU", "Stasis", "EB", "Trend", "multi.OU", c("Stasis", "Stasis"), c("BM", "Trend"), c("BM", "EB"), c("OU", "Trend"), c("OU", "EB"), c("Stasis", "EB"), c("Stasis", "Trend"))

#     test <- model.test(data, model = models.to.test, control.list=list(fnscale = -1), time.split = 65, verbose = FALSE) 

#     expect_equal(class(test), c("dispRity", "model.test"))
#     expect_equal(names(test), c("aic.models", "full.details", "call"))
#     expect_is(test[[1]], c("matrix"))
#     expect_is(test[[2]], c("list"))
# })