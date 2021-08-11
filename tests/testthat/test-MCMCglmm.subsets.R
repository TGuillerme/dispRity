## Test
test_that("MCMCglmm.subsets works", {
    ## Testing the mini chains pipeline
    load("covar_model_list.Rda")
    load("covar_char_data.Rda")
    load("covar_tree_data.Rda")


    # ## Sanitizing
    # expect_error(MCMCglmm.subsets(data = covar_char_data,
    #                               posteriors = covar_model_list[[5]],
    #                               tree = covar_tree_data))

    # ## Right output
    # expect_is(
    #     MCMCglmm.subsets()
    #     , "class")
    # expect_equal(
    #     dim(MCMCglmm.subsets())
    #     , dim)
})
