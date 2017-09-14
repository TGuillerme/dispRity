#TESTING null.test

context("dispRity.models")

## Test data
load("model_test_data.Rda")

## Selecting parameters
test_that("select.parameters internal", {
    model_input <- select.model.list(model_test_data)

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

## Getting AIC
test_that("get.aic.aicc internal", {
    expect_equal(get.aic.aicc(10, 5, 5), c(-10, -70))
})

## Utility for precision
precision <- function(x, digit = 6) {
    return(as.vector(round(x, digit = digit)))
}

## Testing the BM model
test_that("BM function failed", {
    set.seed(1)

    model_name <- "BM"
    model_fun <- optim.bm.ml

    model_input <- select.model.list(model_test_data)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.592489466, 0.000799266)
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 2.533500e-07))
    expected_output <- c("log_likelihood" = 11.438161232,
                         "ancestral_state" = 2.615795999,
                         "sigma_squared" = 0.003873499,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 2.000000000, 
                         "AIC" = -18.876322463,
                         "AICc" = -18.330867918)

    ## Input works for BM
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for BM
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps)
        , precision(expected_control_list$ndeps))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_up, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = model_input)

    ## Get the arguments out

    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})

## Testing if OU works
test_that("OU function failed", {
    set.seed(1)

    model_name <- "OU"
    model_fun <- optim.ou.ml

    time.split <- 0
    n.optima <- 1

    model_input <- select.model.list(model_test_data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.5924894662, 0.0000799266, 0.0231049060, 4.0545745484 )
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 2.533500e-08, 2.310491e-06, 4.054575e-04))
    expected_output <- c("log_likelihood" = 10.089239720,
                         "ancestral_state" = 2.667379156,
                         "sigma_squared" = 0.003793125,
                         "alpha" = 0.000000010,
                         "optimum" = 4.054574548,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 4.000000000, 
                         "AIC" = -12.178479441,
                         "AICc" = -10.178479441)

    ## Input works for OU
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name, fixed.optima = FALSE, n.optima = n.optima))
        , precision(expected_input_parameters))

    ## Control works for OU
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps)
        , precision(expected_control_list$ndeps))


    lower_model <- c(NA, 1e-10, 1e-08, rep(NA, n.optima))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_up, method = "L-BFGS-B", lower = lower_model, hessian = FALSE, data.model.test = model_input, time.split = 0, n.optima = n.optima, fixed.optima = FALSE)

    ## Get the arguments out
    output <- extract.argument(optimised_model, model_input, model_name, fixed.optima = FALSE, n.optima = n.optima)
    expect_equal(precision(output), precision(expected_output))
    expect_equal(names(output), names(expected_output))
})

## Testing if EB works
test_that("EB function failed", {
    set.seed(1)

    model_name <- "EB"
    model_fun <- optim.eb.ml

    model_input <- select.model.list(model_test_data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.59248947, 0.00010000, -0.04797052)
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 1.000000e-08, 4.797052e-06 ))
    expected_output <- c("log_likelihood" = 10.117494989,
                         "ancestral_state" = 2.660248233,
                         "sigma_squared" = 0.004500607,
                         "eb_rate" = -0.002827673,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 3.000000000, 
                         "AIC" = -14.234989978,
                         "AICc" = -13.092132835)

    ## Input works for EB
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for EB
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps)
        , precision(expected_control_list$ndeps))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_up, method = "L-BFGS-B", lower = c(NA, 1e-6), data.model.test = model_input)

    ## Get the arguments out
    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})

## Testing if Stasis works
test_that("Stasis function failed", {
    set.seed(1)

    model_name <- "Stasis"
    model_fun <- optim.stasis.ml

    time.split <- 1
    n.optima <- 1

    model_input <- select.model.list(model_test_data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(0.1782444, 3.4442059)
    expected_control_list <- list(fnscale = -1, ndeps = c(1.782444e-05, 3.444206e-04))
    expected_output <- c("log_likelihood" = -15.1116412,
                         "omega" = 0.1830209,
                         "theta" = 3.4124066,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 2.000000000, 
                         "AIC" = 34.2232824,
                         "AICc" = 34.7687370)

    ## Input works for Stasis
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name, n.optima = n.optima))
        , precision(expected_input_parameters))

    ## Control works for Stasis
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps)
        , precision(expected_control_list$ndeps))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_up, method = "L-BFGS-B", lower = c(0, rep(NA, n.optima)), data.model.test = model_input, time.split = time.split, n.optima = n.optima)

    ## Get the arguments out
    output <- extract.argument(optimised_model, model_input, model_name, n.optima = n.optima)
    expect_equal(output, expected_output)
})

## Testing if Trend works
test_that("Trend function failed", {
    set.seed(1)

    model_name <- "Trend"
    model_fun <- optim.trend.ml

    model_input <- select.model.list(model_test_data)
    model_input <- pooled.variance(model_input, rescale.variance = TRUE)
    control.list <- list(fnscale = -1)

    expected_input_parameters <- c(2.592489e+00, 5.701158e-05, 1.218404e-02)
    expected_control_list <- list(fnscale = -1, ndeps = c(2.592489e-04, 5.701158e-09, 1.218404e-06 ))
    expected_output <- c("log_likelihood" = 12.77701989,
                         "ancestral_state" = 2.63316557,
                         "sigma_squared" = 0.00243161,
                         "trend" = 0.01211170,
                         "sample_size" = 25.000000000,
                         "n_parameters" = 3.000000000, 
                         "AIC" = -19.55403977,
                         "AICc" = -18.41118263)

    ## Input works for Trend
    expect_equal(
        precision(input_parameters <- get.input.parameters(model_input, model.name = model_name))
        , precision(expected_input_parameters))

    ## Control works for Trend
    control_up <- update.control.list(control.list, input_parameters)
    expect_is(control_up, "list")
    expect_equal(names(control_up), c("fnscale", "ndeps"))
    expect_equal(
        precision(control_up$ndeps)
        , precision(expected_control_list$ndeps))

    ## Run the model
    optimised_model <- stats::optim(input_parameters, fn = model_fun, control = control_up, method = "L-BFGS-B", lower = c(NA, 0, -100), data.model.test = model_input)

    ## Get the arguments out
    output <- extract.argument(optimised_model, model_input, model_name)
    expect_equal(output, expected_output)
})
