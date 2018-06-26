
context("adonis.dispRity")

test_that("Works with one or more groups", {
    ## Adonis with one groups 
    ## Generating a random character matrix
    character_matrix <- sim.morpho(rtree(20), 50, rates = c(rnorm, 1, 0))
    ## Calculating the distance matrix
    distance_matrix <- as.matrix(dist(character_matrix))
    ## Creating two random groups
    random_groups <- sample(nrow(distance_matrix), 20)
    random_groups <- list("group1" = random_groups[1:10],  "group2" = random_groups[11:20])
    ## Generating a dispRity object
    random_disparity <- custom.subsets(distance_matrix, random_groups)
    ## Running a default NPMANOVA
    set.seed(1)
    test1 <- adonis.dispRity(random_disparity)

    expect_is(test1, "adonis")
    expect_equal(as.character(test1$call), c("vegan::adonis", "matrix ~ group", "random_disparity", "euclidean"))
    expect_equal(test1$aov.tab$Df, c(1, 18, 19))
    # expect_equal(test1$aov.tab[[6]], c(0.116, NA, NA))

    ## Adonis with multiple groups 
    ## Creating a random matrix
    random_matrix <- matrix(data = rnorm(90), nrow = 10,
                            dimnames = list(letters[1:10]))
    ## Creating two groups with two states each
    groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5), rep(c(1,2), 5)), nrow = 10, ncol = 2, dimnames = list(letters[1:10], c("g1", "g2"))))

    ## Creating the dispRity object
    multi_groups <- custom.subsets(random_matrix, groups)

    ## Running the NPMANOVA
    set.seed(1)
    test2 <- adonis.dispRity(multi_groups, matrix ~ g1 + g2, warn = FALSE, permutations = 10, method = "manhattan")

    expect_is(test2, "adonis")
    expect_equal(as.character(test2$call), c("vegan::adonis", "dist(matrix) ~ g1 + g2", "multi_groups", "10", "manhattan"))
    expect_equal(test2$aov.tab$Df, c(1, 1, 7, 9))
    expect_equal(round(test2$aov.tab[[6]], digit =5), round(c(0.18182, 0.18182, NA, NA), digit = 5))


    ## Testing formula management
    expect_error(
        adonis.dispRity(random_disparity, bob ~ bib)
      )
    expect_error(
        adonis.dispRity(multi_groups, matrix ~ g1 + g8)
      )
    expect_error(
        adonis.dispRity(distance_matrix)
      )
    expect_error(
        adonis.dispRity(multi_groups, method = "bob")
      )
})

test_that("Give the same results as adonis", {

    ## Correct output results
    silent <- capture_warnings(library(vegan))
    data(dune)
    data(dune.env)
    ## Beta diversity distance matrix
    betad_vegan <- vegan::betadiver(dune, "z")
    betad_dispRity <- as.matrix(vegan::betadiver(dune, "z"))

    groups <- data.frame(dune.env$Management, row.names = rownames(betad_dispRity))
    colnames(groups) <- NULL

    betad_dispRity <- custom.subsets(betad_dispRity, group = groups)


    set.seed(1)
    test_vegan <- adonis(betad_vegan ~ Management, data = dune.env, permutations = 20)

    set.seed(1)
    test_dispRity <- adonis.dispRity(betad_dispRity, permutations = 20)

    expect_is(test_vegan, "adonis")
    expect_is(test_dispRity, "adonis")

    for(stat in 1:length(test_vegan$aov.tab)) {
        expect_equal(test_vegan$aov.tab[[stat]], test_dispRity$aov.tab[[stat]])
    }
})


test_that("Correct behaviour with palaeo data", {

    set.seed(1)
    ## Check if it works for mammals data set
    data(BeckLee_mat50)
    data(BeckLee_tree)
    ## Crown stem group
    crown_stem <- crown.stem(BeckLee_tree, inc.nodes = FALSE)
    ## Dummy group
    group_sample <- sample(1:50, 25)
    group1 <- rownames(BeckLee_mat50)[group_sample]
    group2 <- rownames(BeckLee_mat50)[-group_sample]
    dummy_group <- list(group1 = group1, group2 = group2)
    ## Equal groups
    equal_group <- list(groupA = rownames(BeckLee_mat50), groupB = rownames(BeckLee_mat50))

    ## Create the subsets
    subset_crown_stem <- custom.subsets(BeckLee_mat50, group = crown_stem)
    subset_dummy_group <- custom.subsets(BeckLee_mat50, group = dummy_group)
    subset_time <- chrono.subsets(BeckLee_mat50, BeckLee_tree, method = "discrete", inc.nodes = FALSE, time = c(100, 85, 65, 0))

    ## Calculate disparity
    set.seed(1)
    disp_crown_stem <- dispRity(subset_crown_stem, metric = c(median, centroids))
    disp_dummy_group <- dispRity(subset_dummy_group, metric = c(median, centroids))
    disp_time <- dispRity(subset_time, metric = c(median, centroids))

    test_crow_stem <- adonis.dispRity(disp_crown_stem, warn = FALSE)
    test_dummy_group <- adonis.dispRity(disp_dummy_group, warn = FALSE)

    ## Same Df but crow/stem is significant!    
    expect_lt(test_crow_stem$aov.tab[[6]][1], test_dummy_group$aov.tab[[6]][1])
    expect_equal(test_crow_stem$aov.tab$Df, test_dummy_group$aov.tab$Df)

    set.seed(1)
    expect_warning(test_disp_time1 <- adonis.dispRity(disp_time, warn = FALSE))
    set.seed(1)
    expect_warning(test_disp_time2 <- adonis.dispRity(disp_time, formula = matrix ~ time, warn = FALSE))
    set.seed(1)
    test_disp_time3 <- adonis.dispRity(disp_time, formula = matrix ~ chrono.subsets, warn = FALSE)

    ## test 1 and 2 are the same
    expect_equal(test_disp_time1$aov.tab[[6]][1], test_disp_time2$aov.tab[[6]][1])
    expect_equal(dim(test_disp_time3$aov.tab), c(5,6))
})
