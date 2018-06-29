#TESTING model.test
context("model.test")

## Select model data
load("model_test_data.Rda")
data <- model_test_data

test_that("simple models work", {
    set.seed(1)
    test <- model.test(data, model = "BM", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    expect_equal(round(test[[2]][[1]]$value, digit = 4), 8.9143) #8.2029

    set.seed(1)
    test <- model.test(data, model = "Stasis", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list")) 
    expect_equal(round(test[[2]][[1]]$value, digit = 4), -14.9971) #round(-14.7086, digit = 4)

    set.seed(1)
    test <- model.test(data, model = "Trend", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    expect_equal(round(test[[2]][[1]]$value, digit = 4), 11.1296) #round(10.5916, digit = 4)

    set.seed(1)
    test <- model.test(data, model = "OU", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    expect_equal(round(test[[2]][[1]]$value, digit = 4), 11.5986) #round(10.9821, digit = 4)

    set.seed(1)
    test <- model.test(data, model = "multi.OU", pool.variance = NULL, time.split = c(45, 65), fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)

    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    expect_equal(round(test[[2]][[1]]$value, digit = 4), 12.8101) #round(12.0183, digit = 4)
})

test_that("multiple.models work", {
    models.to.test <- list("BM", "OU", "Stasis", "EB", "Trend", "multi.OU", c("Stasis", "Stasis"), c("BM", "Trend"), c("BM", "EB"), c("OU", "Trend"), c("OU", "EB"), c("Stasis", "EB"), c("Stasis", "Trend"))

    test <- model.test(data, model = models.to.test, control.list=list(fnscale = -1), time.split = 65, verbose = FALSE) 

    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    expect_equal(dim(test$aic.models), c(13, 3))
})


test_that("model.test example works", {
    set.seed(42)
    ## Mammal disparity through time
    data(BeckLee_disparity)
    models <- list("BM", "OU", "multi.OU", c("BM", "OU"))

    ## Fitting the four models to the disparity data
    tests <- model.test(BeckLee_disparity, models, time.split = 66, verbose = FALSE)

    expect_is(tests, c("dispRity", "model.test"))
    expect_equal(length(tests), 5)
    expect_equal(lapply(tests, length),
                list("aic.models" = 12,
                     "full.details" = 4,
                     "call" = 5,
                     "model.data" = 4,
                     "fixed.optima" = 1))

    ## Summarising the models
    summary_out <- summary(tests)
    expect_is(summary_out, "matrix")
    expect_equal(dim(summary_out), c(4, 10))
    expect_equal(colnames(summary_out), c("aicc", "delta_aicc", "weight_aicc", "log.lik", "param", "ancestral state", "sigma squared", "alpha", "optima.1", "optima.2"))
    expect_equal(rownames(summary_out), c("BM", "OU", "multi.OU", "BM:OU"))
    
    ## Plotting only the models support
    expect_null(plot(tests))
})

test_that("model.test.sim example works", {
    set.seed(42)
    data(BeckLee_disparity)
    models <- list("Trend", "BM")
    model_test_output <- model.test(BeckLee_disparity, models, time.split = 66, verbose = FALSE)
    expect_is(model_test_output, c("dispRity", "model.test"))
    expect_equal(length(model_test_output), 5)
    expect_equal(lapply(model_test_output, length),
                list("aic.models" = 6,
                     "full.details" = 2,
                     "call" = 5,
                     "model.data" = 4,
                     "fixed.optima" = 1))
     
    ## simulations using the output from model.test
    model_test_sim_output <- model.test.sim(sim = 1000, model= model_test_output)
     
    expect_is(model_test_sim_output, c("dispRity", "model.sim"))
    expect_equal(length(model_test_sim_output), 5)
    expect_equal(lapply(model_test_sim_output, length),
                list("simulation.data" = 2,
                     "p.value" = 12,
                     "call" = 3,
                     "nsim" = 1,
                     "model" = 6))

    ## Plot the simulated best model
    expect_null(plot(model_test_sim_output))
    ## Add the observed data
    expect_null(plot(BeckLee_disparity, add = TRUE, col = c("pink", "#ff000050", "#ff000050")))
    
    ## Simulating a specific model with specific parameters parameters
    model_simulation <- model.test.sim(sim = 1000, model = "BM", time.span = 120, variance = 0.1,
                                       sample.size = 100, parameters = list(ancestral.state = 0,
                                       sigma.squared = 0.1))
    expect_is(model_simulation, c("dispRity", "model.sim"))
    expect_equal(length(model_simulation), 4)
    expect_equal(lapply(model_simulation, length),
                list("simulation.data" = 2,
                     "call" = 7,
                     "nsim" = 1,
                     "model" = 1))
    
    ## Summarising the results
    expect_null(plot(model_simulation, main = "A simple Brownian motion"))
})


test_that("model.test.wrapper example works", {
    set.seed(42)
    data(BeckLee_disparity)
    models <- list("Trend", "BM")
    model_test_output <- model.test(BeckLee_disparity, models, time.split = 66, verbose = FALSE)
    expect_is(model_test_output, c("dispRity", "model.test"))
    expect_equal(length(model_test_output), 5)
    expect_equal(lapply(model_test_output, length),
                list("aic.models" = 6,
                     "full.details" = 2,
                     "call" = 5,
                     "model.data" = 4,
                     "fixed.optima" = 1))
     
    ## simulations using the output from model.test
    model_test_sim_output <- model.test.sim(sim = 1000, model= model_test_output)
     
    expect_is(model_test_sim_output, c("dispRity", "model.sim"))
    expect_equal(length(model_test_sim_output), 5)
    expect_equal(lapply(model_test_sim_output, length),
                list("simulation.data" = 2,
                     "p.value" = 12,
                     "call" = 3,
                     "nsim" = 1,
                     "model" = 6))

    ## Plot the simulated best model
    expect_null(plot(model_test_sim_output))
    ## Add the observed data
    expect_null(plot(BeckLee_disparity, add = TRUE, col = c("pink", "#ff000050", "#ff000050")))
    
    ## Simulating a specific model with specific parameters parameters
    model_simulation <- model.test.sim(sim = 1000, model = "BM", time.span = 120, variance = 0.1,
                                       sample.size = 100, parameters = list(ancestral.state = 0,
                                       sigma.squared = 0.1))
    expect_is(model_simulation, c("dispRity", "model.sim"))
    expect_equal(length(model_simulation), 4)
    expect_equal(lapply(model_simulation, length),
                list("simulation.data" = 2,
                     "call" = 7,
                     "nsim" = 1,
                     "model" = 1))
    
    ## Summarising the results
    expect_null(plot(model_simulation, main = "A simple Brownian motion"))
})

# data(BeckLee_disparity)
# data = BeckLee_disparity
# model <- list("BM", "OU", "multi.OU", "Trend")
# fixed.optima = TRUE
# time.split = 66
# show.p = TRUE
# pool.variance = NULL
# control.list = list(fnscale = -1)
# verbose = TRUE
# sim = 1000
# plot.sim = TRUE
# col.sim = c("grey30", "#00000020", "#00000020")
# col.obs = "hotpink"
#  lwd.obs = 2
#  show.p = FALSE
# model.test.wrapper(data = BeckLee_disparity, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE)