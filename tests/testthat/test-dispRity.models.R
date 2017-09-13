#TESTING null.test

context("dispRity.models")

## Select model data
data(BeckLee_mat99) ; data(BeckLee_ages) ; data(BeckLee_tree)
data_bootstrapped <- boot.matrix(time.subsamples(BeckLee_mat99, BeckLee_tree, method = "continuous", rev(seq(from = 0, to = 120, by = 5)), model = "gradual"))
data <- dispRity(data_bootstrapped, c(sum, variances))

model_test_input <- select.model.list(data)


test_that("select.parameters internal", {
    model_input <- select.model.list(data)

    expect_null(select.parameters(model_input, "bob"))

    expect_equal(
        round(select.parameters(model_input, "Stasis"), digit = 6)
        , round(c("omega" = 0.1782444, "theta" = 3.444206), digit = 6))

    expect_equal(
        round(select.parameters(model_input, "BM"), digit = 6)
        , round(0.000799266, digit = 6))

    expect_equal(
        round(select.parameters(model_input, "EB"), digit = 5)
        , round(c("sigma_squared" = -0.003437541, "alpha" = -0.047970523), digit = 5))

    expect_equal(
        round(select.parameters(model_input, "Trend"), digit = 5)
        ,round(c("sigma_squared" = 5.701158e-05, "trend" = 1.218404e-02), digit = 5))
})

test_that("get.aic.aicc internal", {
    expect_equal(get.aic.aicc(10, 5, 5), c(-10, -70))
})

## Utility for precision
precision <- function(x, digit = 6) {
    return(as.vector(round(x)))
}

## Testing the BM model
test_that("BM works", {
    set.seed(1)

    model_name <- "BM"
    model_fun <- optim.bm.ml

    model_input <- select.model.list(data)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.5924895, 0.0025335)
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 2.533500e-07))
    expected_output <- c("log_likelihood" = 11.438161231,
                         "ancestral_state" = 2.615795319,
                         "sigma_squared" = 0.003873534,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 2.000000000, 
                         "AIC" = -18.876322463,
                         "AICc" = -18.330867917)

    ## Input works for BM
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for BM
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps, digit = 8)
        , precision(expected_control_list$ndeps, digit = 8))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_up, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = model_input)

    ## Get the arguments out

    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})

## Testing if OU works
test_that("OU works", {
    set.seed(1)

    model_name <- "OU"
    model_fun <- optim.ou.ml

    time.split <- 0
    n.optima <- 1

    model_input <- select.model.list(data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.59248947, 0.00025335, 0.02310491, 4.05457455 )
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 2.533500e-08, 2.310491e-06, 4.054575e-04))
    expected_output <- c("log_likelihood" = 10.088996276,
                         "ancestral_state" = 2.664797195,
                         "sigma_squared" = 0.003803996,
                         "alpha" = 0.000000010,
                         "optimum" = 4.054574548,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 4.000000000, 
                         "AIC" = -12.177992552,
                         "AICc" = -10.177992552)

    ## Input works for OU
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name, fixed.optima = FALSE, n.optima = n.optima))
        , precision(expected_input_parameters))

    ## Control works for OU
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps, digit = 8)
        , precision(expected_control_list$ndeps, digit = 8))


    lower_model <- c(NA, 1e-10, 1e-08, rep(NA, n.optima))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control.list, method = "L-BFGS-B", lower = lower_model, hessian = FALSE, data.model.test = model_input, time.split = 0, n.optima = n.optima, fixed.optima = FALSE)

    ## Get the arguments out
    output <- extract.argument(optimised_model, model_input, model_name, fixed.optima = FALSE)
    expect_equal(precision(output), precision(expected_output))
    expect_equal(names(output), names(expected_output))
})

## Testing if EB works
test_that("EB works", {
    set.seed(1)

    model_name <- "EB"
    model_fun <- optim.eb.ml

    model_input <- select.model.list(data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.59248947, 0.00010000, -0.04797052)
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 1.000000e-08, 4.797052e-06 ))
    expected_output <- c("log_likelihood" = 11.557088407,
                         "ancestral_state" = 2.610379188,
                         "sigma_squared" = 0.005406442,
                         "eb_rate" = -0.005499671,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 3.000000000, 
                         "AIC" = -17.114176814,
                         "AICc" = -15.971319671)

    ## Input works for EB
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for EB
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps, digit = 8)
        , precision(expected_control_list$ndeps, digit = 8))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_list, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = model_input)

    ## Get the arguments out

    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})

## Testing if Stasis works
test_that("Stasis works", {
    set.seed(1)

    model_name <- "Stasis"
    model_fun <- optim.stasis.ml

    model_input <- select.model.list(data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(0.1782444, 3.4442059)
    expected_control_list <- list(fnscale = -1, ndeps = c(1.782444e-05, 3.444206e-04))
    expected_output <- c("log_likelihood" = -15.3914926,
                         "omega" = 0.1945245,
                         "theta" = 3.4120313,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 2.000000000, 
                         "AIC" = 34.7829853,
                         "AICc" = 35.3284398)

    ## Input works for Stasis
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for Stasis
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps, digit = 8)
        , precision(expected_control_list$ndeps, digit = 8))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_list, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = model_input)

    ## Get the arguments out

    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})

## Testing if Trend works
test_that("Trend works", {
    set.seed(1)

    model_name <- "Trend"
    model_fun <- optim.trend.ml

    model_input <- select.model.list(data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.592489e+00, 5.701158e-05, 1.218404e-02)
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 5.701158e-09, 1.218404e-06 ))
    expected_output <- c("log_likelihood" = 14.167199103,
                         "ancestral_state" = 2.605751686,
                         "sigma_squared" = 0.002572468,
                         "trend" = 0.012222037,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 3.000000000, 
                         "AIC" = -22.334398206,
                         "AICc" = -21.191541063)

    ## Input works for Trend
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for Trend
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps, digit = 8)
        , precision(expected_control_list$ndeps, digit = 8))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_list, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = model_input)

    ## Get the arguments out

    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})
