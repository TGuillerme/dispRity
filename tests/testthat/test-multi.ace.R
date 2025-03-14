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
    expect_equal(error[[1]], "matrix must be of class matrix or list or data.frame or dispRity or multi.ace.")

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
    expect_equal(error[[1]], "output option must be one of the following: matrix, list, combined, combined.list, combined.matrix, dispRity, multi.ace.")

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
            
    ## Breakable because of castor innit
    results <- multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = FALSE,
                            parallel = FALSE,
                            output = "matrix",
                            estimation.details = c("loglikelihood", "transition_matrix"))
    expect_is(results, "list")
    expect_equal(names(results), c("estimations", "details"))
    expect_is(results$estimations, "list")
    expect_is(results$estimations[[1]], "matrix")
    expect_is(results$details[[1]][[1]]$transition_matrix, "matrix")
    expect_equal(rownames(results$details[[1]][[9]]$transition_matrix), c("0","1","2"))
    expect_is(results$details[[2]][[1]]$loglikelihood, "numeric")
         

    # Test1
    set.seed(3)
    test <- capture.output(results <- multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = FALSE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = TRUE,
                            parallel = FALSE,
                            output = "matrix",
                            estimation.details = c("loglikelihood", "transition_matrix")))
    expect_equal(test,
              c("Preparing the data:.....Done." ,
                "Running ancestral states estimations:....................Done."))

    set.seed(3)
    test <- capture.output(results <- multi.ace(data = matrix_complex,
                            tree = tree_test, 
                            models = "ER", 
                            threshold = TRUE,
                            special.tokens = c("weird" = "%"),
                            special.behaviours = list(weirdtoken = function(x,y) return(c(1,2))),
                            brlen.multiplier = rnorm(10),
                            verbose = TRUE,
                            parallel = 2,
                            output = "matrix",
                            estimation.details = c("loglikelihood", "transition_matrix")))
    expect_equal(length(test),2)
    expect_is(results, "list")
    expect_equal(names(results), c("estimations", "details"))
    expect_is(results$estimations, "list")
    expect_is(results$estimations[[1]], "matrix")
    expect_is(results$details[[1]][[9]]$transition_matrix, "matrix")
    expect_equal(rownames(results$details[[1]][[9]]$transition_matrix), c("0","1","2"))
    expect_is(results$details[[2]][[1]]$loglikelihood, "numeric")


    ## No match check
    tree <- makeNodeLabel(rcoal(10))
    matrix_continuous <- space.maker(elements = 9, dimensions = 3, distribution = rnorm, elements.names = tree$node.label)
    expect_warning(error <- capture_error(multi.ace(matrix_continuous, tree)))
    expect_equal(error[[1]], "No match between the tips in the tree and the rows in the data.")
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

test_that("sample.ace works", {
    ## Create a quick ace
    set.seed(1)
    ## The tree
    tree <- rcoal(15)
    tree <- makeNodeLabel(tree)
    ## The matrix
    data <- space.maker(elements = 15, dimensions = 5, distribution = rnorm, elements.name = tree$tip.label)
    ## The sampling function
    sample.fun <- list(fun = runif, param = list(min = min, max = max))

    expect_warning(ace <- ape::ace(x = data[,1], phy = tree))
    
    ## Sample the ace output
    set.seed(1)
    out <- sample.ace(ace, sample.fun, samples = 2)
    expect_equal(length(out), Nnode(tree))
    expect_equal(length(out[[1]]), 2)
    ## Sampling works
    for(i in 1:Nnode(tree)) {
        expect_true(all(out[[i]] > ace$CI95[i,1]))
        expect_true(all(out[[i]] < ace$CI95[i,2]))
    }
})

test_that("multi.ace works with sample", {
    set.seed(1)
    ## The tree
    tree <- rcoal(10)
    tree <- makeNodeLabel(tree)
    ## The matrix
    data <- data_continuous <- cbind(runif(10, 0, 1), runif(10, 10, 20), runif(10, 100, 200))
    rownames(data) <- tree$tip.label

    # Test with continuous
    expect_warning(test <- multi.ace(data = data, tree = tree, sample = 2, output = "combined.matrix", verbose = FALSE))
    expect_is(test, "list")
    ## Correct number of samples
    expect_equal(length(test), 2)
    ## Correct rows
    expect_equal(rownames(test[[1]]), c(tree$tip.label, tree$node.label))
    ## Correct character estimates
    expect_true(all(test[[1]][,1] < 2))
    expect_true(all(test[[1]][,2] < 50))
    expect_true(all(test[[1]][,2] > -5))
    expect_true(all(test[[1]][,3] > 100))

    ## test with sample.fun option
    error <- capture_error(test <- multi.ace(data = data, tree = tree, sample = 2, sample.fun = runif, output = "combined.matrix", verbose = FALSE))
    expect_equal(error[[1]], "sample.fun must be of class list.")
    sample.fun <- list(fun = rnorm, param = list(max = max, min = min))
    error <- capture_error(test <- multi.ace(data = data, tree = tree, sample = 2, sample.fun = sample.fun, output = "combined.matrix", verbose = FALSE))
    expect_equal(error[[1]], "The sample function is not formatted correctly and cannot generate a distribution.\nCheck the ?multi.ace manual for more details.")
    ## Works with a list of sample funs
    sample.fun <- list(
        list(fun = runif, param = list(max = max, min = min)),
        list(fun = runif, param = list(max = max, min = min)),
        list(fun = rnorm, param = list(mean = mean, sd = function(x)return(diff(range(x))/4))))
    expect_warning(test <- multi.ace(data = data, tree = tree, sample = 2, sample.fun = sample.fun, output = "combined.matrix", verbose = FALSE))
    expect_is(test, "list")
    ## But doesn't if sample fun is badly formated
    sample.fun <- list(
        list(fun = runif, param = list(max = max, min = min)),
        list(fun = rnorm, param = list(max = max, min = min)),
        list(fun = runif, param = list(mean = mean, sd = function(x)return(diff(range(x))/4))))
    error <- capture_error(test <- multi.ace(data = data, tree = tree, sample = 2, sample.fun = sample.fun, output = "combined.matrix", verbose = FALSE))
    expect_equal(error[[1]], "The following sample functions are not formated correctly and cannot generate a distribution: 2, 3.\nCheck the ?multi.ace manual for more details.")

    ## Test with discrete characters
    set.seed(8) 
    data <- data_discrete <- sim.morpho(tree, characters = 2, model = "ER", rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)
    data[,2] <- data_discrete[,2] <- as.character(sample(c(1,2,3), 10, replace = TRUE))

    test <- multi.ace(data = data, tree = tree, sample = 20, output = "combined.matrix", verbose = FALSE)
    expect_is(test, "list")
    ## Correct number of samples
    expect_equal(length(test), 20)
    ## Correct rows
    expect_equal(rownames(test[[1]]), c(tree$tip.label, tree$node.label))
    ## Correct character estimates
    expect_true(all(as.numeric(test[[1]][,1]) %in% c(0,1)))
    expect_true(all(as.numeric(test[[1]][,2]) %in% c(1,2,3)))

    ## test with an invariant
    data <- cbind(data, rep("0", 10))
    expect_warning(test <- multi.ace(data = data, tree = tree, sample = 20, output = "combined.matrix", verbose = FALSE))
    expect_is(test, "list")
    ## Correct number of samples
    expect_equal(length(test), 20)
    ## Correct rows
    expect_equal(rownames(test[[1]]), c(tree$tip.label, tree$node.label))
    ## Correct character estimates
    expect_true(all(as.numeric(test[[1]][,1]) %in% c(0,1)))
    expect_true(all(as.numeric(test[[1]][,2]) %in% c(1,2,3)))
    expect_true(all(test[[1]][,3] == "0"))

    ## Test with mixed characters
    data <- data.frame(data_discrete, data_continuous)
    expect_warning(test <- multi.ace(data = data, tree = tree, sample = 5, output = "matrix", verbose = FALSE))
    ## Correct number of samples
    expect_equal(length(test), 5)
    ## Correct rows
    expect_equal(rownames(test[[1]]), c(tree$node.label))
    ## Correct character estimates
    expect_true(all(as.numeric(test[[1]][,1]) %in% c(0,1)))
    expect_true(all(as.numeric(test[[1]][,2]) %in% c(1,2,3)))
    expect_true(all(test[[1]][,3] < 2))
    expect_true(all(test[[1]][,4] < 51))
    expect_true(all(test[[1]][,4] > -5))
    expect_true(all(test[[1]][,5] > 100))
})

test_that("multi.ace works with recycling", {
    set.seed(1)
    ## The tree
    tree <- rcoal(10)
    tree <- makeNodeLabel(tree)
    ## The matrix
    data <- data_continuous <- cbind(runif(10, 0, 1), runif(10, 10, 20), runif(10, 100, 200))
    rownames(data) <- tree$tip.label
    set.seed(8) 
    data <- data_discrete <- sim.morpho(tree, characters = 2, model = "ER", rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)
    data[,2] <- data_discrete[,2] <- as.character(sample(c(1,2,3), 10, replace = TRUE))
    data <- data.frame(data_discrete, data_continuous)
    data <- cbind(data, rep("0", 10))

    ## Generating the multi.ace
    expect_warning(test <- multi.ace(data = data, tree = tree, output = "multi.ace", verbose = FALSE, estimation.details = NULL, sample = 1))
    expect_is(test, c("dispRity", "multi.ace"))
    expect_equal(names(test), c("tree", "matrix", "discrete", "continuous", "invariants"))
    expect_is(test$tree, "multiPhylo")
    expect_is(test$matrix, "data.frame")
    expect_equal(names(test$discrete), c("estimates", "IDs", "special.tokens"))
    expect_equal(unname(test$discrete$IDs), c(1,2,6))
    expect_equal(length(test$discrete$estimates[[1]]), 2)
    expect_equal(names(test$discrete$estimates[[1]][[1]]), c("results", "success", "dropped", "details"))
    expect_equal(test$discrete$special.tokens, c("inapplicable" = "\\-", "missing" = "\\?", "polymorphism" = "\\&", "uncertainty" = "\\/"))
    expect_equal(names(test$continuous), c("estimates", "IDs"))
    expect_equal(unname(test$continuous$IDs), c(3,4,5))
    expect_equal(length(test$continuous$estimates[[1]]), 3)
    expect_equal(names(test$continuous$estimates[[1]][[1]]), c("resloglik", "ace", "sigma2", "CI95", "call"))
    expect_equal(names(test$invariants), c("n", "states", "IDs"))
    expect_equal(unname(test$invariants$n), 3)
    expect_equal(unname(test$invariants$states), list(0))
    expect_equal(unname(test$invariants$IDs), 6)

    ## test the printing
    expect_equal(capture.output(test), "Raw ancestral traits estimations for:\n 3 discrete (including 1 invariants) and 3 continuous characters across 1 tree for 10 taxa.\nYou can use the multi.ace function to resample them and transform them in different outputs.")


    ## Recycling the multi.ace
    tust <- multi.ace(test, output = "combined.matrix", sample = 1, threshold = TRUE)
    expect_equal(dim(tust), c(19, 6))

    ## Works with sampling
    tast <- multi.ace(test, output = "combined.matrix", sample = 10)
    expect_equal(length(tast), 10)
    expect_equal(unique(unlist(lapply(tast, dim))), c(19, 6))
})