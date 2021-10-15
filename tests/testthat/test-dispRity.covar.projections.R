## Test
test_that("covar.projections.wrapper works", {
    data(charadriiformes)

    data <- MCMCglmm.subsets(
                      data          = charadriiformes$data,
                      posteriors    = charadriiformes$posteriors,
                      group         = MCMCglmm.levels(charadriiformes$posteriors)[1:4],
                      rename.groups = c("gulls", "plovers", "sandpipers", "phylogeny"))
    
    ## Sanitizing
    error <- capture_error(dispRity.covar.projections("data", type = "groups", base = "phylogeny", n = 3, major.axis = 1, level = 0.95, output = c("position"), verbose = TRUE))
    expect_equal(error[[1]], "data must be of class dispRity.")
    error <- capture_error(dispRity.covar.projections(data, type = 1, base = "phylogeny", n = 3, major.axis = 1, level = 0.95, output = c("position"), verbose = TRUE))
    expect_equal(error[[1]], "type must be must be one of the following: elements, groups.")
    error <- capture_error(dispRity.covar.projections(data, type = "groups", base = "haha", n = 3, major.axis = 1, level = 0.95, output = c("position"), verbose = TRUE))
    expect_equal(error[[1]], "Subset haha not found.")
    error <- capture_error(dispRity.covar.projections(data, type = "groups", n = 3, major.axis = 1, level = 0.95, output = c("possssition"), verbose = TRUE))
    expect_equal(error[[1]], "output must be must be one of the following: position, distance, degree.")



    ## Test between no base
    test <- dispRity.covar.projections(data, type = "groups", n = 7, verbose = FALSE)
    # expect_equal(names(test), c("position", "distance", "degree"))
    # expect_equal(names(test[[1]]), c("gulls:plovers", "gulls:sandpipers",  "gulls:phylogeny", "plovers:sandpipers",  "plovers:phylogeny", "sandpipers:phylogeny"))
    # expect_equal(dim(test[[1]][[1]]), c(1,7))
    
    # ## Test between average phylo base
    # test <- covar.projections.wrapper(data, type = "between", sample = mean, base = "phylogeny")
    # expect_equal(names(test), c("position", "distance", "degree"))
    # expect_equal(names(test[[1]]), c("gulls:phylogeny", "plovers:phylogeny", "sandpipers:phylogeny"))
    # expect_equal(dim(test[[1]][[1]]), c(1,1))

    # ## Test between all phylo bases
    # test <- covar.projections.wrapper(data, type = "between", sample = c(1,2,3,4,5), base = "phylogeny")
    # expect_equal(names(test), c("position", "distance", "degree"))
    # expect_equal(names(test[[1]]), c("gulls:phylogeny", "plovers:phylogeny", "sandpipers:phylogeny"))
    # expect_equal(dim(test[[1]][[1]]), c(1,5))

    # ## Test between average phylo base dispRity style
    # test <- covar.projections.wrapper(data, type = "between", n = 1, base = "phylogeny", dispRity.out = TRUE)
    # expect_equal(names(test), c("position", "distance", "degree"))
    # expect_equal(unique(unlist(lapply(test, class))), "dispRity")
    
    # ## Test within no base
    # test <- covar.projections.wrapper(data, type = "within", n = 5, measure = c("degree", "distance"))
    # expect_equal(names(test), c("degree", "distance"))
    # expect_equal(names(test[[1]]), c("gulls", "plovers", "sandpipers", "phylogeny"))
    # for(i in 1:length(test[[1]])) {
    #     expect_equal(dim(test[[1]][[i]]), c(unname(size.subsets(data)[i]),5))
    #     expect_equal(dim(test[[2]][[i]]), c(unname(size.subsets(data)[i]),5))
    # }

    # ## Test within average phylo base
    # test <- covar.projections.wrapper(data, type = "within", sample = mean, measure = c("position"), base = "phylogeny")
    # expect_equal(names(test), c("position"))
    # expect_equal(names(test[[1]]), c("gulls", "plovers", "sandpipers"))
    # for(i in 1:length(test[[1]])) {
    #     expect_equal(dim(test[[1]][[i]]), c(unname(size.subsets(data)[i]),1))
    # }

    # ## Test within all phylo bases
    # test <- covar.projections.wrapper(data, type = "within", measure = c("position"), base = "phylogeny")
    # expect_equal(names(test), c("position"))
    # expect_equal(names(test[[1]]), c("gulls", "plovers", "sandpipers"))
    # for(i in 1:length(test[[1]])) {
    #     expect_equal(dim(test[[1]][[i]]), c(unname(size.subsets(data)[i]),1000))
    # }

    # ## Test dispRity style
    # test <- covar.projections.wrapper(data, type = "within", n = 5, measure = c("degree", "distance"), dispRity.out = TRUE)
    # expect_equal(names(test), c("degree", "distance"))
    # expect_equal(unique(unlist(lapply(test, class))), "dispRity")
})

test_that("covar.projections.wrapper verbose works", {
    # ## Loading the MCMCglmm
    # load("../../../Data/processed/combined_results.rda")
    # ## Loading the tree and data
    # data(demo_data)
    # data(demo_tree)

    # data <- MCMCglmm.dispRity(
    #                   data          = demo_data,
    #                   posteriors    = combined_results,
    #                   group         = MCMCglmm.levels(combined_results)[1:4],
    #                   rename.groups = c("gulls", "plovers", "sandpipers", "phylogeny"))
    
    # ## Test between no base
    # verbose <- capture_messages(test <- covar.projections.wrapper(data, type = "between", n = 30, verbose = TRUE))
})