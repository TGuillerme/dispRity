## Test
test_that("MCMCglmm.subsets and sauron.plot works", {
    ## Testing the mini chains pipeline
    load("covar_model_list.Rda")
    load("covar_char_data.Rda")
    load("covar_tree_data.Rda")

    ## Sanitizing
    ## data class
    error <- capture_error(MCMCglmm.subsets(data = "covar_char_data", posteriors = covar_model_list[[1]]))
    expect_equal(error[[1]], "data must be of class data.frame or matrix.")
    ## posteriors class
    error <- capture_error(MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list))
    expect_equal(error[[1]], "posteriors must be of class MCMCglmm.")
    ## group (optional)
    error <- capture_error(MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[1]], group = c(1,2,3)))
    expect_equal(error[[1]], "The group argument must be a named vector.")
    error <- capture_error(MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[1]], group = c("1" = 1, "2" = 2)))
    expect_equal(error[[1]], "The following groups cannot be found in the posteriors: 1, 2.\nCheck MCMCglmm(posteriors) for the available group names.")
    ## tree (optional)
    error <- capture_error(MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[1]], tree = "covar_tree_data"))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    ## rename.groups (optional)
    error <- capture_error(MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[1]], rename.groups = c("a", "b")))
    expect_equal(error[[1]], "The rename.groups argument must the same length as group argument (1).")

    ## Model 1: 1 group residual
    ## Data structure
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[1]])
    expect_is(test, "dispRity")
    expect_equal(length(test), 5)
    expect_equal(names(test), c("matrix", "tree", "call", "subsets", "covar"))
    expect_equal(dim(test$matrix[[1]]), c(359, 17))
    expect_equal(test$call$dimensions, c(1,2,3))
    expect_equal(test$call$subsets, "covar")
    expect_null(test$tree[[1]])
    expect_equal(names(test$subsets), "units")
    expect_equal(names(test$subsets[[1]]), "elements")
    expect_equal(names(test$covar), c("units"))
    expect_equal(length(test$covar), 1)
    expect_equal(length(test$covar[[1]]), 1000)
    expect_equal(names(test$covar[[1]][[1]]), c("VCV", "Sol"))

    ## Correct printing
    print_test <- capture.output(print.dispRity(test))
    expect_equal(print_test, c(" ---- dispRity object ---- ", "One covar matrix (units) with 359 elements in one matrix with 3 dimensions.", "Data is based on 1000 posterior samples."))

    # ## Default plot
    # expect_null(sauron.plot(test))
    # expect_null(sauron.plot(test, n = 3, ellipses = TRUE, main = "something", level = 0.5, legend = TRUE, legend.pos = c(0.5, -0.5), pch = 12))


    ## Model 2: 3 group residual
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[2]])
    expect_equal(length(test$covar), 3)
    expect_equal(names(test$covar), c("units:clade_1", "units:clade_2", "units:clade_3"))
    # expect_null(sauron.plot(test, col = rainbow(3), points = FALSE, ellipses = mean))

    ## Model 3: 2 group (res/rand)
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors =covar_model_list[[3]])
    expect_equal(length(test$covar), 2)
    expect_equal(names(test$covar), c("animal", "units"))
    print_test <- capture.output(print.dispRity(test))
    expect_equal(print_test, c(" ---- dispRity object ---- ", "2 covar subsets for 359 elements in one matrix with 3 dimensions:", "    animal, units.", "Data is based on 1000 posterior samples."))

    # expect_null(sauron.plot(test, col = rainbow(2), points = FALSE, major.axes = "all", n = 20, legend = TRUE))

    
    ## Model 4: 4 group (3/1)
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors =covar_model_list[[4]])
    expect_equal(length(test$covar), 4)
    expect_equal(names(test$covar), c("animal", "units:clade_1", "units:clade_2", "units:clade_3"))
    print_test <- capture.output(print.dispRity(test))
    expect_equal(print_test, c(" ---- dispRity object ---- ", "4 covar subsets for 359 elements in one matrix with 3 dimensions:", "    animal, units:clade_1, units:clade_2, units:clade_3.", "Data is based on 1000 posterior samples."))
    # expect_null(sauron.plot(test, col = rainbow(4), points = TRUE, major.axes = "all", n = 20, legend = TRUE, ellipses = mean, pch = 21))

    ## Model 5: 6 group (3/3)
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors =covar_model_list[[5]])
    expect_equal(length(test$covar), 6)
    expect_equal(names(test$covar), c("animal:clade_1", "animal:clade_2", "animal:clade_3", "units:clade_1", "units:clade_2", "units:clade_3"))
    print_test <- capture.output(print.dispRity(test))
    expect_equal(print_test, c(" ---- dispRity object ---- ", "6 covar subsets for 359 elements in one matrix with 3 dimensions:", "     animal:clade_1, animal:clade_2, animal:clade_3, units:clade_1, units:clade_2 ...", "Data is based on 1000 posterior samples."))
    # expect_null(sauron.plot(test, col = rainbow(6), points = FALSE, major.axes = mean, legend = TRUE, ellipses = mean, pch = 21))

    ## Model 6: 6 group (3/4)
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors =covar_model_list[[6]])
    expect_equal(length(test$covar), 7)
    expect_equal(names(test$covar), c("animal:clade_1", "animal:clade_2", "animal:clade_3", "animal", "units:clade_1", "units:clade_2", "units:clade_3"))
    print_test <- capture.output(print.dispRity(test))
    expect_equal(print_test, c(" ---- dispRity object ---- ", "7 covar subsets for 359 elements in one matrix with 3 dimensions:", "     animal:clade_1, animal:clade_2, animal:clade_3, animal, units:clade_1 ...", "Data is based on 1000 posterior samples."))
    # expect_null(sauron.plot(test, ellipses = mean))

    ## Model 7: 5 group (1/4)
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors =covar_model_list[[7]])
    expect_equal(length(test$covar), 5)
    expect_equal(names(test$covar), c("animal:clade_1", "animal:clade_2", "animal:clade_3", "animal", "units"))
    print_test <- capture.output(print.dispRity(test))
    expect_equal(print_test, c(" ---- dispRity object ---- ", "5 covar subsets for 359 elements in one matrix with 3 dimensions:", "    animal:clade_1, animal:clade_2, animal:clade_3, animal, units.", "Data is based on 1000 posterior samples."))
    
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors =covar_model_list[[7]], group = c(random = "animal", random = "animal:clade_1", random = "animal:clade_2", random = "animal:clade_3"), rename.groups = c("phylogeny", "clade_1", "clade_2", "clade_3"))
    expect_equal(size.subsets(test), c("phylogeny" = 359, "clade_1" = 160, "clade_2" = 97, "clade_3" = 102))
    expect_equal(length(test$covar), 4)
    expect_equal(names(test$covar), c("phylogeny", "clade_1", "clade_2", "clade_3"))
    # expect_null(sauron.plot(test, ellipses = mean, major.axes = "all", n = 100, col = c("grey","orange", "blue", "darkgreen"), legend = TRUE, points = TRUE, cex = 0.2))

    # Try with not all subsets selected on a big model
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[7]], group = c(random = "animal", residual = "units"))
    expect_equal(length(test$covar), 2)
    expect_equal(names(test$covar), c("animal", "units"))
    
    # Try with correct groups renaming
    test <- MCMCglmm.subsets(data = covar_char_data, posteriors = covar_model_list[[7]], rename.groups = c("clade_1", "clade_2", "clade_3", "phylo", "residual"))
    expect_equal(length(test$covar), 5)
    expect_equal(names(test$covar), c("clade_1", "clade_2", "clade_3", "phylo", "residual"))
})
