#TESTING print.dispRity

#context("print.dispRity") 

## Loading the data
data("disparity")
data("BeckLee_mat50")
data("BeckLee_mat99")
data("BeckLee_tree")


test_that("normal printing", {
    ## Empty object
    test <- make.dispRity()
    expect_equal(capture.output(test), "Empty dispRity object.")
    expect_equal(length(capture.output(print(test, all = TRUE))), 16)

    ## Empty object with a matrix
    test <- make.dispRity(data = matrix(1))
    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "Contains a matrix 1x1."
    ))

    test <- make.dispRity(data = matrix(10, nrow = 5), tree = rtree(5))
    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "Contains a matrix 5x1 and a tree."
    ))

    test <- make.dispRity(data = list(matrix(10, nrow = 5), matrix(10, nrow = 5)), tree = rmtree(5, 5))
    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "Contains 2 matrices 5x1 and 5 trees."
    ))


    ## Time subsets
    test <- chrono.subsets(BeckLee_mat50, time = c(100, 90, 50), method = "discrete", tree = BeckLee_tree)

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "2 discrete time subsets for 50 elements in one matrix with 1 phylogenetic tree",
        "    100 - 90, 90 - 50."
    ))

    test <- chrono.subsets(BeckLee_mat99, time = c(100,90,80,70,50,60,40), method = "continuous", tree = BeckLee_tree, model = "ACCTRAN")

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "7 continuous (acctran) time subsets for 99 elements in one matrix with 1 phylogenetic tree",
        "     100, 90, 80, 70, 50 ..."
    ))

    ## Custom subsets
    expect_warning(test <- custom.subsets(matrix(data = rnorm(90), nrow = 10), list(c(1:4), c(5:10))))

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "2 customised subsets for 10 elements in one matrix:",
        "    1, 2."
    ))

    ## Bootstrapped data
    test <- boot.matrix(BeckLee_mat50)

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "50 elements in one matrix with 48 dimensions.",
        "Rows were bootstrapped 100 times (method:\"full\")."
    ))

    ## Bootstrapped + subsets
    test <- boot.matrix(chrono.subsets(BeckLee_mat50, time = c(100, 90, 50), method = "discrete", tree = BeckLee_tree))

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "2 discrete time subsets for 50 elements in one matrix with 48 dimensions with 1 phylogenetic tree",
        "    100 - 90, 90 - 50.",
        "Rows were bootstrapped 100 times (method:\"full\")."
    ))

    ## Disparity only    
    test <- dispRity(BeckLee_mat50, metric = mean)

    expect_equal(capture.output(test), 
        c(
        " ---- dispRity object ---- ",
        "50 elements in one matrix with 48 dimensions.",
        "Disparity was calculated as: mean."
    ))

    ## Bootstrapped + subsets + rarefaction + disparity

    expect_equal(capture.output(disparity), 
        c(
        " ---- dispRity object ---- ",
        "7 continuous (acctran) time subsets for 99 elements in one matrix with 97 dimensions with 1 phylogenetic tree",
        "     90, 80, 70, 60, 50 ...",
        "Rows were bootstrapped 100 times (method:\"full\") and rarefied to 20, 15, 10, 5 elements.",
        "Disparity was calculated as: c(median, centroids)."
    ))

    ## Fully rarefied
    expect_equal(capture.output(dispRity(boot.matrix(BeckLee_mat50, rarefaction = TRUE), metric = mean)),
        c(
        " ---- dispRity object ---- ",
        "50 elements in one matrix with 48 dimensions.",
        "Rows were bootstrapped 100 times (method:\"full\") and fully rarefied.",
        "Disparity was calculated as: mean."
    ))

    ## Multiple matrices
    set.seed(42)
    one_matrix <- matrix(1, 5, 10, dimnames = list(c(1:5)))
    data <- list(one_matrix, one_matrix, one_matrix)

    ## Works with level one
    expect_equal(capture.output(dispRity(data, metric = c(sum))),c(
        " ---- dispRity object ---- ",
        "5 elements in 3 matrices with 10 dimensions.",
        "Disparity was calculated as: c(sum)."
        ))
})

test_that("randtest printing", {
    set.seed(1)
    obs_disparity <- dispRity(BeckLee_mat50, metric = ellipsoid.volume, dimensions = c(1:5))
    expect_warning(test <- null.test(obs_disparity, replicates = 100, null.distrib = rnorm))

    expect_equal(capture.output(test),
        c("Monte-Carlo test", "Call: [1] \"dispRity::null.test\"",
        "", "Observation: 0.02463073 ", 
        "", "Based on 100 replicates", 
        "Simulated p-value: 0.07920792 ", "Alternative hypothesis: two-sided ", 
        "", "    Std.Obs Expectation    Variance ",
        "  -1.747348    4.976530    8.031281 "))

  
    ## Running the test on multiple subsets (may take some time!)
    ## Generating the subsets
    groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12),
         rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
    customised_subsets <- custom.subsets(BeckLee_mat50, groups)
    ## Bootstrapping the data
    bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 100)
    ## Calculating variances of each dimension
    sum_variances <- dispRity(bootstrapped_data, metric = c(sum, variances))
    ## Testing against normal distribution
    expect_warning(results <- null.test(sum_variances, replicates = 100, null.distrib = rnorm))

    expect_equal(capture.output(print.dispRity(results)),
        c(
        "$V1.1"                               ,
        "Monte-Carlo test"                    ,
        "Call: [1] \"dispRity::null.test\""   ,
        ""                                    ,
        "Observation: 2.113905 "              ,
        ""                                    ,
        "Based on 100 replicates"             ,
        "Simulated p-value: 0.00990099 "      ,
        "Alternative hypothesis: two-sided "  ,
        ""                                    ,
        "    Std.Obs Expectation    Variance ",
        " -32.757510   47.847500    1.949166 ",
        ""                                    ,
        "$V1.2"                               ,
        "Monte-Carlo test"                    ,
        "Call: [1] \"dispRity::null.test\""   ,
        ""                                    ,
        "Observation: 2.337814 "              ,
        ""                                    ,
        "Based on 100 replicates"             ,
        "Simulated p-value: 0.00990099 "      ,
        "Alternative hypothesis: two-sided "  ,
        ""                                    ,
        "    Std.Obs Expectation    Variance ",
        "  -33.76072    48.22930     1.84774 ",
        ""                                    ,
        "$V1.3"                               ,
        "Monte-Carlo test"                    ,
        "Call: [1] \"dispRity::null.test\""   ,
        ""                                    ,
        "Observation: 2.492183 "              ,
        ""                                    ,
        "Based on 100 replicates"             ,
        "Simulated p-value: 0.00990099 "      ,
        "Alternative hypothesis: two-sided "  ,
        ""                                    ,
        "    Std.Obs Expectation    Variance ",
        " -32.643250   48.010800    1.944426 ",
        ""                                    ,
        "$V1.4"                               ,
        "Monte-Carlo test"                    ,
        "Call: [1] \"dispRity::null.test\""   ,
        ""                                    ,
        "Observation: 2.576456 "              ,
        ""                                    ,
        "Based on 100 replicates"             ,
        "Simulated p-value: 0.00990099 "      ,
        "Alternative hypothesis: two-sided "  ,
        ""                                    ,
        "    Std.Obs Expectation    Variance ",
        " -32.035871   48.119200    2.020995 ",
        ""
        )
    )
})

test_that("dtt printing", {
    set.seed(1)
    ## Loading geiger's example data set
    data <- matrix(rnorm(20), ncol = 2)
    rownames(data) <- paste0("t", 1:10)
    tree <- rtree(10)

    ## The average squared pairwise distance metric (used in geiger::dtt)
    average.sq <- function(X) mean(pairwise.dist(X)^2)
    ## Calculate the disparity of the dataset using dtt.dispRity
    dispRity_dtt <- dtt.dispRity(data = data, metric = average.sq,
                                 tree = tree, nsim = 10)

    print_dtt <- capture.output(dispRity_dtt)
    
    set.seed(1)
    dispRity_dtt_raw <- dtt.dispRity(data = data, metric = average.sq,
                                     tree = tree, nsim = 0)
    print_dtt_raw <- capture.output(dispRity_dtt_raw)

    expect_equal(print_dtt,
        c(
        "Disparity-through-time test (modified from geiger::dtt)" ,
        "Call: dtt.dispRity(data = data, metric = average.sq, tree = tree, nsim = 10, model = \"BM\", alternative = \"two-sided\") ",
        "",
        "Observation: 0.100298654808419",
        "",
        "Model: BM",
        "Based on 10 replicates",
        "Simulated p-value: 1",
        "Alternative hypothesis: two-sided",
        "",
        "    Mean.dtt Mean.sim_MDI  var.sim_MDI ",
        "  0.65302742   0.08453948   0.03519623 ",
        "",
        "Use plot.dispRity() to visualise." 
        ))


    expect_equal(print_dtt_raw[c(length(print_dtt_raw)-1,length(print_dtt_raw))],
        c(
        # "$dtt"                                                                                      ,
        # " [1] 1.0000000 0.7108704 0.8137332 1.1194885 1.1752659 1.3945462 2.2877953 1.8151213"      ,
        # " [9] 0.6232065 0.0000000"                                                                  ,
        # ""                                                                                          ,
        # "$times"                                                                                    ,
        # "                 11        12        19        17        13        18        14        16 ",
        # "0.0000000 0.0000000 0.1450459 0.4944510 0.5212973 0.6062471 1.2191329 1.3625423 1.7906876 ",
        # "       15 "                                                                                ,
        # "2.3279070 "                                                                                ,
        # ""                                                                                          ,
        "- attr(*, \"class\") = \"dispRity\" \"dtt\""                                               ,
        "Use plot.dispRity to visualise." 
        ))
})

test_that("print.dispRity with model.test data", {
    load("model_test_data.rda")

    ## Run two models (silent)
    models <- list("BM", "OU")
    set.seed(42)
    tested_models <- model.test(model_test_data, models, time.split = 65, fixed.optima = TRUE, verbose = FALSE)
    print_model.test <- capture.output(tested_models)

    expect_equal(print_model.test,
        c("Disparity evolution model fitting:",
         "Call: model.test(data = model_test_data, model = models, time.split = 65, fixed.optima = TRUE, verbose = FALSE) ",
         "",
         "        aicc delta_aicc weight_aicc",
         "BM -32.69195   0.000000   0.7856167",
         "OU -30.09454   2.597407   0.2143833",
         "",
         "Use x$full.details for displaying the models details",
         "or summary(x) for summarising them."
        ))


    ## Testing normal model
    model_simulation_empty <- model.test.sim(sim = 10, model = "BM")
    print_model.sim1 <- capture.output(model_simulation_empty)

    expect_equal(print_model.sim1,
        c("Disparity evolution model simulation:",
          "Call: model.test.sim(sim = 10, model = \"BM\") ",
          "",
          "Model simulated (10 times):",
          "[1] \"BM\"",
          ""
        ))

    ## Testing inherited model
    set.seed(42)
    model_simulation_inherit <- model.test.sim(sim = 10, model = tested_models)
    print_model.sim2 <- capture.output(model_simulation_inherit)

    expect_equal(print_model.sim2,
        c("Disparity evolution model simulation:",
          "Call: model.test.sim(sim = 10, model = tested_models) ",
          "",
          "Model simulated (10 times):",
          "    aicc log.lik param ancestral state sigma squared",
          "BM -32.7   18.62     2           2.967         0.002",
          "",
          "Rank envelope test:",
          " p-value of the global test: 0.1818182 (ties method: erl)",
          " p-interval                : (0, 0.6363636)" 
        ))
})

test_that("dispRitreats verbose", {
    ## Testing the placeholder trigger
    data(disparity)
    output <- capture_output(print(disparity))
    expect_equal(output, " ---- dispRity object ---- \n7 continuous (acctran) time subsets for 99 elements in one matrix with 97 dimensions with 1 phylogenetic tree\n     90, 80, 70, 60, 50 ...\nRows were bootstrapped 100 times (method:\"full\") and rarefied to 20, 15, 10, 5 elements.\nDisparity was calculated as: c(median, centroids).")
    disparity$call$dispRitreats <- TRUE
    output <- capture_output(print(disparity))
    expect_equal(output, " ---- dispRity object ---- \n7 continuous (acctran) time subsets for 99 elements in one matrix with 97 dimensions with 1 phylogenetic tree\n     90, 80, 70, 60, 50 ...\nRows were bootstrapped 100 times (method:\"full\") and rarefied to 20, 15, 10, 5 elements.\nDisparity was calculated as: c(median, centroids).\nDisparity was calculated from treats simulated data.")    
})