## Test
test_that("dispRity.covar.projections works", {

## Toggling nocov for bugs with covr
nocov <- TRUE

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

    ## warnings
    data_warn <- MCMCglmm.subsets(
                      data          = charadriiformes$data,
                      posteriors    = charadriiformes$posteriors,
                      group         = MCMCglmm.levels(charadriiformes$posteriors)[1:4],
                      rename.groups = c("gul:ls", "plovers", "sandpipers", "phylogeny"))

if(!nocov) {
    ## Warning bad naming
    warns <- capture_warnings(test <- dispRity.covar.projections(data_warn, type = "groups", n = 2, verbose = FALSE))
    expect_equal(warns[[1]], "The subset name: gul:ls was changed to gul;ls. The \":\" character is reserved for between groups comparisons.")

    ## Test between no base
    verb <- capture_messages(test <- dispRity.covar.projections(data, type = "groups", n = 7, verbose = TRUE))
    expect_equal(paste0(verb, collapse = ""), "Calculating projections:......Done.\n")
    expect_equal(names(test), c("position", "distance", "degree"))
    expect_equal(names(test[[1]]$disparity), c("gulls:plovers", "gulls:sandpipers",  "gulls:phylogeny", "plovers:sandpipers",  "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal(dim(test[[1]]$disparity[[1]]$elements), c(1,7))
    ## Correct output format (dispRity) 
    for(i in 1:3) {
        expect_equal(dim(summary(test[[i]])), c(6,8))
        expect_null(plot(test[[i]]))
    }
}

if(!nocov) {
    ## Test between average phylo base
    test <- dispRity.covar.projections(data, type = "groups", sample = mean, base = "phylogeny", output = c("degree", "position"))
    expect_equal(names(test), c("degree", "position"))
    expect_equal(names(test[[1]]$disparity), c("gulls:phylogeny", "plovers:phylogeny",  "sandpipers:phylogeny"))
    expect_equal(dim(test[[1]]$disparity[[1]]$elements), c(1,1))
}

if(!nocov) {   
    ## Test between all phylo bases
    test <- dispRity.covar.projections(data, type = "groups", sample = c(1,2,3,4,5), base = "phylogeny")
    expect_equal(names(test), c("position", "distance", "degree"))
    expect_equal(names(test[[1]]$disparity), c("gulls:phylogeny", "plovers:phylogeny", "sandpipers:phylogeny"))
    expect_equal(dim(test[[1]]$disparity[[1]]$elements), c(1,5))
}
    
    ## Test within no base
    verb <- capture_messages(test <- dispRity.covar.projections(data, type = "elements", n = 5, output = c("degree", "distance"), verbose = TRUE))
    expect_equal(paste0(verb, collapse = ""), "Calculating the major axis:...Done.\nCalculating projections:......Done.\n")
    expect_equal(names(test), c("degree", "distance"))
    expect_equal(names(test[[1]]$disparity), c("gulls", "plovers", "sandpipers", "phylogeny"))
    expect_equal(dim(test[[1]]$disparity[[1]]$elements), c(159,5))
    ## Correct output format (dispRity) 
    for(i in 1:2) {
        expect_equal(dim(summary(test[[i]])), c(4,7))
        expect_null(plot(test[[i]]))
    }

    ## Test within average phylo base
    test <- dispRity.covar.projections(data, type = "elements", sample = mean, output = c("position"), base = "phylogeny")
    expect_equal(names(test), c("position"))
    expect_equal(names(test[[1]]$disparity), c("gulls", "plovers", "sandpipers"))
    for(i in 1:length(test[[1]]$disparity)) {
        expect_equal(dim(test[[1]]$disparity[[i]]$elements), c(unname(size.subsets(data)[i]),1))
    }
})
