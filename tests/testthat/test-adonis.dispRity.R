
#context("adonis.dispRity")

test_that("make.time.factor picks up time", {

    data(BeckLee_disparity)
    group_names <- "time"
    group_variables <- names(BeckLee_disparity$subsets)
    time_subsets <- TRUE
    pool_time <- TRUE

    predictors <- make.factors(BeckLee_disparity, group_names, group_variables, time_subsets, pool_time)
    expect_is(predictors, "data.frame")
    expect_equal(dim(predictors), c(99, 1))
    expect_true(any(is.na(predictors)))

    predictors <- make.factors(BeckLee_disparity, group_names, group_variables, time_subsets = FALSE, pool_time)
    expect_is(predictors, "data.frame")
    expect_equal(dim(predictors), c(1572, 1))
    expect_false(any(is.na(predictors)))


    predictors <- make.factors(BeckLee_disparity, group_names, group_variables, time_subsets = FALSE, pool_time = FALSE)
    expect_is(predictors, "data.frame")
    expect_equal(dim(predictors), c(1572, 1))
    expect_false(any(is.na(predictors)))


    predictors <- make.factors(BeckLee_disparity, group_names, group_variables, time_subsets = TRUE, pool_time = FALSE)
    expect_is(predictors, "data.frame")
    expect_equal(dim(predictors), c(99, 121))
    expect_false(any(is.na(predictors)))
})

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
    expect_warning(random_disparity <- custom.subsets(distance_matrix, random_groups))
    
    ## Some errors
    error <- capture_error(adonis.dispRity(make.dispRity(distance_matrix)))
    expect_equal(error[[1]], "make.dispRity(distance_matrix) must have subsets. Use custom.subsets() or chrono.subsets() to create some.")
    error <- capture_error((adonis.dispRity(random_disparity, matrix ~ time)))
    expect_equal(error[[1]], "random_disparity has no time subsets.\nImpossible to use the following formula: matrix ~ time")

    ## Running a default NPMANOVA
    set.seed(1)
    test1 <- adonis.dispRity(random_disparity)

    expect_is(test1, "anova.cca")
    expect_is(test1, "anova")
    expect_is(test1, "data.frame")
    expect_equal(attr(test1, "heading")[2], c("vegan::adonis2(formula = matrix ~ group, method = \"euclidean\")"))
    expect_equal(test1$Df, c(1, 18, 19))

    ## Adonis with multiple groups 
    ## Creating a random matrix
    random_matrix <- matrix(data = rnorm(90), nrow = 10,
                            dimnames = list(letters[1:10]))
    ## Creating two groups with two states each
    groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5), rep(c(1,2), 5)), nrow = 10, ncol = 2, dimnames = list(letters[1:10], c("g1", "g2"))))

    ## Creating the dispRity object
    multi_groups <- custom.subsets(random_matrix, groups)


    # YVAR <- formula[[2]]
    # lhs <- eval(YVAR, environment(formula), globalenv())

    ## Running the NPMANOVA
    set.seed(1)
    test2 <- adonis.dispRity(multi_groups, matrix ~ g1 + g2, warn = FALSE, permutations = 10, method = "manhattan")

    expect_is(test2, "anova.cca")
    expect_is(test2, "anova")
    expect_is(test2, "data.frame")
    expect_equal(attr(test2, "heading")[1], c("Permutation test for adonis under reduced model\nPermutation: free\nNumber of permutations: 10\n"))  
    expect_equal(attr(test2, "heading")[2], c("vegan::adonis2(formula = dist(matrix) ~ g1 + g2, permutations = 10, method = \"manhattan\")"))
    expect_equal(test2$Df, c(2, 7, 9))
    # expect_equal(round(test2$aov.tab[[6]], digit = 5), round(c(0.36364, 0.72727, NA, NA), digit = 5))

    ## Works well on non distance matrices

    ## Calculating the distance matrix (PCO)
    distance_matrix <- cmdscale(as.matrix(dist(character_matrix)), k = 19)
    ## Generating a dispRity object
    random_disparity <- custom.subsets(distance_matrix, random_groups)    
    expect_warning(tests <- adonis.dispRity(random_disparity))
    expect_is(tests, "anova.cca")
    expect_is(tests, "anova")
    expect_is(tests, "data.frame")

    ## Testing formula management
    error <- capture_error(adonis.dispRity(random_disparity, bob ~ bib))
    expect_equal(error[[1]], "Formula must be of type: matrix ~ predictor(s) (where matrix is the response).")
    error <- capture_error(adonis.dispRity(multi_groups, matrix ~ g1 + g8))
    expect_equal(error[[1]], "Predictor g8 not found in multi_groups subsets.")
    error <- capture_error(adonis.dispRity(distance_matrix))
    expect_equal(error[[1]], "data must be of class dispRity.")
    error <- capture_error(adonis.dispRity(multi_groups, method = "bob"))
    expect_equal(error[[1]], "method must be one of the following: manhattan, euclidean, canberra, bray, kulczynski, jaccard, gower, altGower, morisita, horn, mountford, raup, binomial, chao, cao, mahalanobis.")
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

    expect_warning(betad_dispRity <- custom.subsets(betad_dispRity, group = groups))

    set.seed(1)
    test_vegan <- adonis2(betad_vegan ~ Management, data = dune.env, permutations = 20)

    set.seed(1)
    test_dispRity <- adonis.dispRity(betad_dispRity, permutations = 20)

    expect_is(test_vegan, "anova.cca")
    expect_is(test_vegan, "anova")
    expect_is(test_vegan, "data.frame")
    expect_is(test_dispRity, "anova.cca")
    expect_is(test_dispRity, "anova")
    expect_is(test_dispRity, "data.frame")

    for(stat in 1:length(test_vegan)) {
        expect_equal(test_vegan[[stat]], test_dispRity[[stat]])
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
    expect_lt(test_crow_stem$"Pr(>F)"[1], test_dummy_group$"Pr(>F)"[1])
    expect_equal(test_crow_stem$Df, test_dummy_group$Df)

    set.seed(1)
    expect_warning(test_disp_time1 <- adonis.dispRity(disp_time, warn = FALSE))
    set.seed(1)
    # expect_warning(test_disp_time2 <- adonis.dispRity(disp_time, formula = matrix ~ time, warn = FALSE))
    set.seed(1)
    test_disp_time3 <- adonis.dispRity(disp_time, formula = matrix ~ chrono.subsets, warn = FALSE)

    ## test 1 and 2 are the same
    expect_equal(test_disp_time1$"Pr(>F)"[1], test_disp_time3$"Pr(>F)"[1])
})
