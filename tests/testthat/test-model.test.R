#TESTING model.test
context("model.test")

## Select model data
load("model_test_data.Rda")
data <- model_test_data

test_that("simple models work", {

    ## BM model
    set.seed(1)
    test <- model.test(data, model = "BM", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    # expect_equal(round(test[[2]][[1]]$value, digit = 4), 8.9143) #8.2029


    ## BM model verbose
    set.seed(1)
    verbose <- capture.output(test <- model.test(data, model = "BM"))
    expect_equal(length(verbose), 3)
    expect_equal(verbose[1], "Evidence of equal variance (Bartlett's test of equal variances p = 0).")
    expect_equal(verbose[2], "Variance is not pooled.")
    expect_equal(strsplit(verbose[3], split = "=")[[1]][1], "Running BM model...Done. Log-likelihood ")


    ## Stasis model
    set.seed(1)
    test <- model.test(data, model = "Stasis", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list")) 
    # expect_equal(round(test[[2]][[1]]$value, digit = 4), -14.9971) #round(-14.7086, digit = 4)


    ## Trend model
    set.seed(1)
    test <- model.test(data, model = "Trend", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    # expect_equal(round(test[[2]][[1]]$value, digit = 4), 11.1296) #round(10.5916, digit = 4)


    ## OU model
    set.seed(1)
    test <- model.test(data, model = "OU", pool.variance = NULL, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    # expect_equal(round(test[[2]][[1]]$value, digit = 4), 11.5986) #round(10.9821, digit = 4)


    ## Multi.OU model
    set.seed(1)
    test <- model.test(data, model = "multi.OU", pool.variance = NULL, time.split = c(45, 65), fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    # expect_equal(round(test[[2]][[1]]$value, digit = 4), 12.8101) #round(12.0183, digit = 4)


    ## Pooling variance works
    set.seed(1)
    test <- model.test(data, model = "BM", pool.variance = TRUE, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    test2 <- model.test(data, model = "BM", pool.variance = FALSE, time.split = NULL, fixed.optima = FALSE, control.list = list(fnscale = -1), verbose = FALSE)
    ## Both results differ
    expect_false(test$aic.models[1,1] == test2$aic.models[1,1])


    ## multi.OU with no time.split variance works
    set.seed(1)
    expect_error(model.test(data, model = "multi.OU", time.split = NULL, verbose = FALSE))
    ## Running the multi OU on a 32 split dataset
    data_tmp <- dispRity(boot.matrix(chrono.subsets(BeckLee_mat99, BeckLee_tree, time = 32, model = "acctran", method = "continuous")), metric = mean)
    verbose <- capture.output(test <- model.test(data_tmp, model = "multi.OU", time.split = NULL, verbose = TRUE))
    expect_equal(class(test), c("dispRity", "model.test"))
    expect_equal(names(test), c("aic.models", "full.details", "call", "model.data", "fixed.optima"))
    expect_is(test[[1]], c("matrix"))
    expect_is(test[[2]], c("list"))
    ## Check the message
    expect_equal(length(verbose), 18)
    expect_equal(verbose[1], "Evidence of equal variance (Bartlett's test of equal variances p = 0).")
    expect_equal(verbose[2], "Variance is not pooled.")
    expect_equal(verbose[3], "Running multi.OU on 13 shift times...")

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
    models <- list("BM", "OU", "multi.OU", c("BM", "OU"))

    ## Fitting the four models to the disparity data
    tests <- model.test(data, models, time.split = 66, verbose = FALSE)

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
    models <- list("Trend", "BM", "Stasis")
    model_test_output <- model.test(data, models, time.split = 66, verbose = FALSE)
    expect_is(model_test_output, c("dispRity", "model.test"))
    expect_equal(length(model_test_output), 5)
    expect_equal(lapply(model_test_output, length),
                list("aic.models" = 9,
                     "full.details" = 3,
                     "call" = 5,
                     "model.data" = 4,
                     "fixed.optima" = 1))
     
    ## simulations using the output from model.test
    expect_error(model.test.sim(sim = 10, model = data))
    expect_error(model.test.sim(sim = 10, model = model_test_output, model.rank = 4))
    ## Warning for ignored argument (inherited) + absolute value for sim -10 (silly)
    expect_warning(model_test_sim_output <- model.test.sim(sim = -10, model = model_test_output, time.span = 8, alternative = "lesser"))     
    expect_is(model_test_sim_output, c("dispRity", "model.sim"))
    expect_equal(length(model_test_sim_output), 5)
    expect_equal(lapply(model_test_sim_output, length),
                list("simulation.data" = 2,
                     "p.value" = 12,
                     "call" = 5,
                     "nsim" = 1,
                     "model" = 6))


    ## Plot the simulated best model
    expect_null(plot(model_test_sim_output))
    ## Add the observed data
    expect_null(plot(data, add = TRUE, col = c("pink", "#ff000050", "#ff000050")))
    
    ## Warning for ignored argument (inherited) + absolute value for sim -10 (silly)
    model_test_sim_output <- model.test.sim(sim = 10, model = model_test_output, model.rank = 3)
    expect_is(model_test_sim_output, c("dispRity", "model.sim"))
    expect_equal(length(model_test_sim_output), 5)
    expect_equal(lapply(model_test_sim_output, length),
                list("simulation.data" = 2,
                     "p.value" = 12,
                     "call" = 4,
                     "nsim" = 1,
                     "model" = 5))



    ## Simulating a specific model with specific parameters parameters
    model_simulation <- model.test.sim(sim = 10, model = "BM", time.span = 120, variance = 0.1,
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
    models <- list("BM", "OU", "multi.OU", "Trend")

    ## Some errors
    expect_error(model.test.wrapper(data = data, model = "BIM", fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = FALSE, sim = 10))
    expect_error(model.test.wrapper(data = data, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = FALSE, sim = "a"))
    expect_error(model.test.wrapper(data = "a", model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = FALSE, sim = 10))
    expect_error(model.test.wrapper(data = data, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = "yes", sim = 10))
    expect_error(model.test.wrapper(data = data, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = FALSE, sim = 10, col.sim = 1))
    expect_error(model.test.wrapper(data = data, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = FALSE, sim = 10, cex.p = "a"))

    test <- model.test.wrapper(data = data, model = models, fixed.optima = TRUE, time.split = 66, show.p = TRUE, verbose = FALSE, sim = -10, legend = TRUE, cex.p = 0.6)

    ## Check test
    expect_is(test, "matrix")
    expect_equal(dim(test), c(4, 13))
    expect_equal(rownames(test), c("Trend", "BM", "multi.OU", "OU"))
    expect_equal(colnames(test), c("aicc", "delta_aicc", "weight_aicc", "log.lik", "param", "ancestral state", "sigma squared", "alpha", "optima.2", "trend", "median p value", "lower p value",  "upper p value"))

    ## Testing with a single model
    test2 <- model.test.wrapper(data = data, model = "BM", fixed.optima = TRUE, time.split = 66, show.p = FALSE, verbose = FALSE, sim = 10)
    expect_is(test2, "matrix")
    expect_equal(dim(test2), c(1, 10))
    expect_equal(rownames(test2), "")
    expect_equal(colnames(test2), c("aicc", "delta_aicc", "weight_aicc", "log.lik", "param", "ancestral state", "sigma squared", "median p value", "lower p value",  "upper p value"))

})