#context("multi.ace")
test_that("model internals works", {

    ## with corStruc
    test <- set.continuous.args.ace(method = "pic", model = "BM", scaled = 1, kappa = 2, corStruct = 3)
    expect_is(test, "list")
    expect_equal(names(test), c("type", "model", "scaled","kappa", "corStruct"))

    ## with models = "BM"
    ## with methods = "pic"
    test <- set.continuous.args.ace.models(models = "pic", n = 1)
    expect_is(test, "list")
    expect_equal(names(test[[1]]),  c("type", "model", "scaled","kappa"))
    expect_error(check.model.class(one_model = "ah", available_models = available_models_continuous))
    expect_equal(check.model.class(one_model = 1, available_models = available_models_continuous), "numeric")
})

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

    # results <- multi.ace(data = matrix_complex,
    #                     tree = tree_test, 
    #                     models = "ER", 
    #                     threshold = TRUE,
    #                     special.tokens = c("weird" = "%"),
    #                     special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
    #                     brlen.multiplier = rnorm(10),
    #                     verbose = FALSE,
    #                     parallel = FALSE,
    #                     output = "list") 
 
    error <- capture_error(multi.ace(data = "matrix_complex",
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "matrix must be of class matrix or list or data.frame.")

    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = "tree_test", 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = tree_test, 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "models must be of class character or list or matrix.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = list(c(1,2,3)),
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "threshold must be of class logical or numeric.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = mean,
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "special.tokens must be of class character.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = c("weird" = "%"),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "special.behaviours must be of class list.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = c(1,2,3),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "brlen.multiplier must contain 10 values (number of edges).")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = "FALSE",
                            parallel = FALSE,
                            output = "list"))
    expect_equal(error[[1]], "verbose must be of class logical.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = rtree,
                            output = "list"))
    expect_equal(error[[1]], "parallel must be of class logical or numeric or integer.")
    
    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "something"))
    expect_equal(error[[1]], "output option must be one of the following: matrix, list, combined, combined.list, combined.matrix, dispRity.")

    error <- capture_error(multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "list",
                            estimation.details = c("bob")))
    expect_equal(error[[1]], "estimation.details must be one of the following: success, Nstates, transition_matrix, loglikelihood, ancestral_likelihoods.")


    # expect_is(results, "list") #bug in macos
    # expect_is(results[[1]], "list") #bug in macos
    # expect_is(results[[1]][[1]], "character") #bug in macos
    # expect_equal(results[[1]][[1]], c("0", "0/1", "0/1", "0", "0", "1", "1", "0", "0", "0/1")) #bug in macos
    # # expect_equal(results[[1]][[1]], c("0", "0/1/2", "0/1", "0", "0", "0/1", "1", "0", "0", "0/1")) v. > 1.6.8
    # # expect_equal(results[[2]][[4]], c("0", "0", "0", "0", "0/1", "0/1", "1", "0", "0", "1"))
    # expect_equal(results[[2]][[4]], c("0", "0", "0", "0", "0", "0/1", "1", "0", "0", "1")) #bug in macos
    # # new version of castor...

    ## Some specific case
    list_matrix <- unlist(apply(matrix_test, 1, list), recursive = FALSE)
    results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]], 
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list")
    expect_is(results, "list")
    expect_is(results[[1]], "character")
    wrong_tree <- tree_test[[1]]
    wrong_tree$tip.label[2] <- "A"
    error <- capture_error(results <- multi.ace(
                        data = list_matrix,
                        tree = wrong_tree, 
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list"))
    expect_equal(error[[1]], "Some names in the data or the tree(s) are not matching.\nYou can use dispRity::clean.data(list_matrix, wrong_tree) to find out more.")

    ## Models handling
    specific_model <- matrix(c(0, 1, 1, 0), 2)
    results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        models = specific_model,
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list")
    expect_is(results, "list")
    expect_is(results[[1]], "character")
    specific_model <- replicate(10, matrix(c(0, 1, 1, 0), 2), simplify = FALSE)
    results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        models = specific_model,
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list")
    expect_is(results, "list")
    expect_is(results[[1]], "character")
    specific_model <- as.list(rep("SUEDE", 10))
    results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        models = specific_model,
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list")
    expect_is(results, "list")
    expect_is(results[[1]], "character")
    wrong_model <- list("SUEDE", matrix(c(0, 1, 1, 0), 2))
    error <- capture_error(results <- multi.ace(
                        data = list_matrix,
                        tree = tree_test[[1]],
                        models = wrong_model,
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list"))
    expect_equal(error[[1]], "models list must be the same length as the number of characters (10).")

    ## Castor options works well
    error <- capture_error(results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list",
                        options.args = list(2)))
    expect_equal(error[[1]], "options.args must be an unambiguous named list of options for castor::asr_mk_model() or ape::ace().")

    ## Threshold works well
    results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        models = specific_model,
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list",
                        threshold = FALSE)
    expect_is(results, "list")
    expect_is(results[[1]], "character")

    ## Special tokens check
    error <- capture_error(results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list",
                        special.tokens = c("something" = "@")))
    expect_equal(error[[1]], "special.tokens cannot contain the character '@' since it is reserved for the dispRity::char.diff function.")
    error <- capture_error(results <- multi.ace(data = list_matrix,
                        tree = tree_test[[1]],
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list",
                        special.tokens = c("missing" = "?", "something" = "?")))
    expect_equal(error[[1]], "special.tokens cannot contain duplicated tokens.")

    ## Multiple branch lengths modifiers (and good handling of NA special tokens)
    results <- multi.ace(data = list_matrix,
                        tree = tree_test,
                        models = specific_model,
                        brlen.multiplier = list(rep(1, 10), rep(2, 10)),
                        verbose = FALSE,
                        parallel = FALSE,
                        output = "list",
                        special.tokens = c("missing" = NA, "forgot" = "?"))
    expect_is(results, "list")
    expect_is(results[[1]], "list")
    expect_is(results[[1]][[1]], "character")

    ## Invariant characters
    invariant_matrix <- matrix_test
    invariant_matrix[,1:10] <- "0"
    error <- capture_error(multi.ace(data = invariant_matrix, tree = tree_test))
    expect_equal(error[[1]], "invariant_matrix contains only invariant characters.")

    invariant_matrix <- matrix_test
    invariant_matrix[,1:3] <- "0"
    warnings <- capture_warning(multi.ace(data = invariant_matrix, tree = tree_test, threshold = FALSE))
    expect_equal(warnings[[1]], "The characters 1, 2, 3 are invariant (using the current special behaviours for special characters) and are simply duplicated for each node.")
    expect_is(results, "list")
    expect_is(results[[1]], "list")
    expect_is(results[[1]][[1]], "character")

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
    ancestral_states <- multi.ace(matrix_test, tree_test, output = "combined")
    expect_is(ancestral_states, "list")
    expect_equal(length(ancestral_states), 2)
    expect_is(ancestral_states[[1]], "matrix")
    expect_equal(dim(ancestral_states[[1]]), c(11, 10))

    ## Parallel works
    expect_is(multi.ace(matrix_test, tree_test, parallel = 1), "list")
    test_verbose <- capture.output(test <- multi.ace(matrix_test, tree_test, parallel = 2, verbose = TRUE))
    expect_is(test, "list")
    expect_equal(test_verbose, c("Preparing the data:.....Done.", "Running the estimation for 2 trees using 2 cores...Done."))

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

    ## Output details
    set.seed(8) 
    matrix_test <- sim.morpho(rcoal(6), characters = 10, model = "ER", rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)
    tree_test <- rmtree(2, 6)
    matrix_complex <- matrix_test
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "-"
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "0%2"
    matrix_complex[sample(1:length(matrix_complex), 5)] <- "?"
    # results <- multi.ace(data = matrix_complex,
    #                         tree = tree_test, 
    #                         models = "ER", 
    #                         threshold = TRUE,
    #                         special.tokens = c("weird" = "%"),
    #                         special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
    #                         brlen.multiplier = rnorm(10),
    #                         verbose = FALSE,
    #                         parallel = FALSE,
    #                         output = "matrix",
    #                         estimation.details = c("loglikelihood", "transition_matrix"))
    # expect_is(results, "list")
    # expect_equal(names(results), c("estimations", "details"))
    # expect_is(results$estimations, "list")
    # expect_is(results$estimations[[1]], "matrix")
    # expect_is(results$details[[1]]$transition_matrix[[9]], "matrix")
    # expect_equal(rownames(results$details[[1]]$transition_matrix[[9]]), c("0","1","2"))
    # expect_is(results$details[[2]]$loglikelihood[[1]], "numeric")
 


    ## Test1
    # set.seed(3)
    # test <- capture.output(results <- multi.ace(data = matrix_complex,
    #                         tree = tree_test, 
    #                         models = "ER", 
    #                         threshold = FALSE,
    #                         special.tokens = c("weird" = "%"),
    #                         special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
    #                         brlen.multiplier = rnorm(10),
    #                         verbose = TRUE,
    #                         parallel = FALSE,
    #                         output = "matrix",
    #                         estimation.details = c("loglikelihood", "transition_matrix")))
    # expect_equal(test,
    #           c("Preparing the data:.....Done." ,
    #             "Running ancestral states estimations:....................Done."))

    # set.seed(3)
    # test <- capture.output(results <- multi.ace(data = matrix_complex,
    #                         tree = tree_test, 
    #                         models = "ER", 
    #                         threshold = TRUE,
    #                         special.tokens = c("weird" = "%"),
    #                         special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
    #                         brlen.multiplier = rnorm(10),
    #                         verbose = TRUE,
    #                         parallel = 2,
    #                         output = "matrix",
    #                         estimation.details = c("loglikelihood", "transition_matrix")))
    # expect_equal(length(test),2)
    # expect_is(results, "list")
    # expect_equal(names(results), c("estimations", "details"))
    # expect_is(results$estimations, "list")
    # expect_is(results$estimations[[1]], "matrix")
    # expect_is(results$details[[1]]$transition_matrix[[9]], "matrix")
    # expect_equal(rownames(results$details[[1]]$transition_matrix[[9]]), c("0","1","2"))
    # expect_is(results$details[[2]]$loglikelihood[[1]], "numeric")

})

test_that("multi.ace works with continuous and mix", {
    set.seed(1)
    ## The tree
    tree <- rcoal(15)
    tree <- makeNodeLabel(tree)
    ## The matrix
    data <- space.maker(elements = 15, dimensions = 5, distribution = rnorm, elements.name = tree$tip.label)

    ## Run the multi.ace on the continuous data
    expect_warning(test <- multi.ace(data = data, tree = tree, output = "combined.matrix", verbose = FALSE))

    ## Works well for continuous
    expect_is(test, "matrix")
    expect_equal(dim(test), c(15+14, 5))
    expect_equal(sort(rownames(test)), sort(c(tree$tip.label, tree$node.label)))
    expect_equal(unique(apply(test, 2, class)), "numeric")

    ## Mixed characters
    data <- as.data.frame(data)
    data <- cbind(data, "new_char" = as.character(sample(1:2, 15, replace = TRUE)))
    data <- cbind(data, "new_char2" = as.character(sample(1:2, 15, replace = TRUE)))

    ## Works well for mixed characters
    expect_warning(test <- multi.ace(data = data, tree = tree, output = "combined.matrix"))
    expect_is(test, "data.frame")
    expect_equal(dim(test), c(15+14, 7))
    expect_equal(sort(rownames(test)), sort(c(tree$tip.label, tree$node.label)))
    classes <- character()
    for(i in 1:ncol(test)) {
        classes[i] <- class(test[, i]) 
    }
    expect_equal(unique(classes), c("numeric", "character"))

    ## Works for parallel
    test <- multi.ace(data = data, tree = tree, parallel = 1)
    expect_is(test, "data.frame")
    expect_equal(dim(test), c(14, 7))
    expect_equal(sort(rownames(test)), sort(c(tree$node.label)))
    classes <- character()
    for(i in 1:ncol(test)) {
        classes[i] <- class(test[, i]) 
    }
    expect_equal(unique(classes), c("numeric", "character"))

    ## Works with invariant characters and absolute threshold model
    data <- cbind(data, "invar1" = as.character(rep(1, 15, replace = TRUE)))
    data <- cbind(data, "invar2" = as.character(rep(2, 15,  replace = TRUE)))
    expect_warning(test <- multi.ace(data = data, tree = tree, threshold = 0.75))
    expect_is(test, "data.frame")
    expect_equal(dim(test), c(14,9))
})
