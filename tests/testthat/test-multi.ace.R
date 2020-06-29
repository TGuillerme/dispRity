context("multi.ace")

## Test
test_that("multi.ace works", {

    ## Sanitizing works
    set.seed(8) 
    matrix_test <- sim.morpho(rcoal(6), characters = 10, model = "ER", rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)
    tree_test <- rmtree(2, 6)
    matrix_complex <- matrix_test
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "-"
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "0%2"
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "?"

    results <- multi.ace(data = matrix_complex,
                        tree = tree_test, 
                        models = "ER", 
                        threshold = TRUE,
                        special.tokens = c("weird" = "%"),
                        special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                        brlen.multiplier = rnorm(10),
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list")
 

    expect_error(multi.ace(data = "matrix_complex",
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = "tree_test", 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = tree_test, 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = list(c(1,2,3)),
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = mean,
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = c("weird" = "%"),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = c(1,2,3),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = "FALSE",
                            parallel = FALSE,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = rtree,
                            output = "list"))
    expect_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "something"))


    expect_is(results, "list")
    expect_is(results[[1]], "list")
    expect_is(results[[1]][[1]], "character")
    # expect_equal(results[[1]][[1]], c("0/1/2", "0/1/2", "0/1", "0/1", "0/1", "0/1", "0/1", "0/1", "0/1/2", "0/1"))
    expect_equal(results[[1]][[1]], c("0", "0/1/2", "0/1", "0", "0", "0/1", "1", "0", "0", "0/1"))
    # expect_equal(results[[2]][[4]], c("0", "0", "0", "0", "0", "0/1", "1", "0", "0", "1"))
    expect_equal(results[[2]][[4]], c("0", "0", "0", "0", "NA", "0/1", "1", "0", "0", "1"))

    ## Warnings work
    set.seed(4) 
    matrix_test <- sim.morpho(rcoal(6), characters = 10, model = "ER", rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)
    tree_test <- rmtree(2, 6)
    matrix_complex <- matrix_test
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "-"
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "0%2"
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "?"
    warn <- capture_warnings(results <- multi.ace(data = matrix_complex,
                        tree = tree_test, 
                        models = "ER", 
                        threshold = TRUE,
                        special.tokens = c("weird" = "%"),
                        special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                        brlen.multiplier = rnorm(10),
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list"))
    expect_length(warn, 5)
    expect_equal(warn[1], "The characters 1, 4 are invariant (using the current special behaviours for special characters) and are simply duplicated for each node.")
    expect_equal(warn[2], "Impossible to fit the model for the following character(s): 1, 2, 3, 4, 5, 6, 7, 8.\nThe ancestral estimated values are set to uncertain (all states equiprobable).")
    expect_equal(warn[3], "number of items to replace is not a multiple of replacement length")
    expect_equal(warn[4], "Impossible to fit the model for the following character(s): 1, 2, 3, 4, 5, 6, 7, 8.\nThe ancestral estimated values are set to uncertain (all states equiprobable).")
    expect_equal(warn[5], "number of items to replace is not a multiple of replacement length")


    ## Outputs work
    ancestral_states <- multi.ace(matrix_test, tree_test, output = "list")
    expect_is(ancestral_states, "list")
    expect_equal(length(ancestral_states), 2)
    expect_equal(length(ancestral_states[[1]]), 5)
    expect_equal(length(ancestral_states[[1]][[1]]), 10)
    ancestral_states <- multi.ace(matrix_test, tree_test, output = "matrix")
    expect_is(ancestral_states, "list")
    expect_equal(length(ancestral_states), 2)
    expect_is(ancestral_states[[1]], "matrix")
    expect_equal(dim(ancestral_states[[1]]), c(5, 10))
    ancestral_states <- multi.ace(matrix_test, tree_test, output = "combined.list")
    expect_is(ancestral_states, "list")
    expect_equal(length(ancestral_states), 2)
    expect_equal(length(ancestral_states[[1]]), 11)
    expect_equal(length(ancestral_states[[1]][[1]]), 10)
    ancestral_states <- multi.ace(matrix_test, tree_test, output = "combined.matrix")
    expect_is(ancestral_states, "list")
    expect_equal(length(ancestral_states), 2)
    expect_is(ancestral_states[[1]], "matrix")
    expect_equal(dim(ancestral_states[[1]]), c(11, 10))
    ancestral_states <- multi.ace(matrix_test, tree_test, output = "dispRity")
    expect_is(ancestral_states, "list")
    expect_equal(length(ancestral_states), 2)
    expect_equal(names(ancestral_states), c("tips", "nodes"))
    expect_is(ancestral_states[[1]], "matrix")
    expect_equal(dim(ancestral_states[[1]]), c(6, 10))
    expect_is(ancestral_states[[2]][[1]], "matrix")
    expect_equal(dim(ancestral_states[[2]][[1]]), c(5, 10))    

    ## Parallel works
    expect_is(multi.ace(matrix_test, tree_test, parallel = TRUE), "list")
    expect_is(multi.ace(matrix_test, tree_test, parallel = 2), "list")


    ## Examples work
    set.seed(42)
    tree <- rcoal(15)
    my_rates = c(rgamma, rate = 10, shape = 5)
    matrix_simple <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates,
                                invariant = FALSE)
    multiple_trees <- rmtree(10, 15)
    matrix_complex <- matrix_simple
    matrix_complex[sample(1:length(matrix_complex), 50)] <- "-"
    matrix_complex[sample(1:length(matrix_complex), 50)] <- "0%2"
    matrix_complex[sample(1:length(matrix_complex), 50)] <- "?"
    my_spec_tokens <- c("weirdtoken" = "%")
    my_spec_behaviours <- list()
    my_spec_behaviours$inapplicable <- function(x,y) return(NA)
    my_spec_behaviours$missing      <- function(x,y) return(y)
    my_spec_behaviours$weirdtoken   <- function(x,y) return(c(1,2))
    branch_lengths <- rnorm(28)^2
    my_models <- c(rep("ER", 25), rep("SYM", 25))

    ## Test1
    test <- capture.output(ancestral_states <- multi.ace(matrix_complex, multiple_trees,
                                  verbose = TRUE,
                                  models = my_models,
                                  threshold = 0.95,
                                  special.tokens = my_spec_tokens,
                                  special.behaviours = my_spec_behaviours,
                                  brlen.multiplier = branch_lengths,
                                  output = "combined.matrix"))

    expect_equal(test,
              c("Preparing the data:.....Done." ,
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done.",
                "Running ancestral states estimations:" ,
                ".................................................. Done."))
})
