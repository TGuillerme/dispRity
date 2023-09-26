## Simple examples
set.seed(1)
data(BeckLee_tree)
data(BeckLee_mat99)
data(BeckLee_mat50)
data(BeckLee_ages)
nonode_tree <- BeckLee_tree
nonode_tree$node.labels <- NULL

## Base examples
disparity_base <- dispRity(BeckLee_mat50, metric = centroids)
disparity_group <- dispRity(custom.subsets(BeckLee_mat50, group = crown.stem(BeckLee_tree, inc.nodes = FALSE)), tree = nonode_tree, metric = centroids)

## Create a list of trees and matrices
trees_list <- replicate(4, rcoal(30), simplify = FALSE)
class(trees_list) <- "multiPhylo"
matrices_list <- space.maker(elements = 30, dimensions = 5, distribution = rnorm, elements.names = trees_list[[1]]$tip.label, replicates = 3)
matrices_groups <- custom.subsets(data = matrices_list, group = list("group1" = trees_list[[1]]$tip.label[ 1:20],
                                                                     "group2" = trees_list[[1]]$tip.label[21:30]))
matrix_groups <- custom.subsets(data = matrices_list[[1]], group = list("group1" = trees_list[[1]]$tip.label[ 1:20],
                                                                     "group2" = trees_list[[1]]$tip.label[21:30]))

test_that("sanitizing works", {
    expect_warning(disparity_time <- dispRity(chrono.subsets(BeckLee_mat50, FADLAD = BeckLee_ages, tree = BeckLee_tree, time = 5, method = "discrete"), metric = centroids, tree = nonode_tree))

    ## Sanitizing
    ## data
    wrong_dimensions <- dispRity(BeckLee_mat50, metric = c(sum, variances))
    error <- capture_error(pgls.dispRity(data = "wrong_dimensions", tree = BeckLee_tree))
    expect_equal(error[[1]], "data must be of class dispRity.")
    error <- capture_error(pgls.dispRity(data = wrong_dimensions, tree = BeckLee_tree))
    expect_equal(error[[1]], "Impossible to run a univariate pgls on wrong_dimensions because doesn't contain a dimension level-2 metric. See ?dispRity.metric for more info.")
    
    ## tree
    wrong_tree <- rtree(50)
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = "wrong_tree"))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    expect_warning(error <- capture_error(pgls.dispRity(data = disparity_base, tree = wrong_tree)))
    expect_equal(error[[1]], "The data is not matching the tree labels (you can use ?clean.data to match both data and tree).")
    error <- capture_error(pgls.dispRity(data = disparity_base))
    expect_equal(error[[1]], "No tree was found in the provided data and none was provided through the tree argument.")


    ## formula
    wrong_formula <- dispoority ~ 1
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = nonode_tree, formula = wrong_formula))
    expect_equal(error[[1]], "The response term of the formula must be 'disparity'.")
    expect_equal(get.formula(disparity_base), disparity ~ 1)
    expect_equal(get.formula(disparity_group), disparity ~ group)
    error <- capture_error(get.formula(disparity_time))
    expect_equal(error[[1]], "Some groups have overlapping elements.")
    disparity_time2 <- dispRity(chrono.subsets(BeckLee_mat50, tree = BeckLee_tree, time = c(140, 66, 0), method = "discrete"), metric = centroids, tree = nonode_tree)
    error <- capture_error(get.formula(disparity_time2))
    expect_equal(error[[1]], "It is currently not possible to apply an phylogenetic linear model on dispRity data with time series.")

    ## model
    error <- capture_error(pgls.dispRity(data = disparity_base, tree = nonode_tree, model = "SM"))
    expect_equal(error[[1]], "model must be one of the following: BM, OUrandomRoot, OUfixedRoot, lambda, kappa, delta, EB, trend.")
})

test_that("get.data works", {

    tree <- nonode_tree

    ## Base test (no group)
    data <- add.tree(disparity_base, tree, replace = TRUE)
    test <- get.pgls.data(data)
    expect_equal(length(test), 1)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(50, 1))
    expect_equal(colnames(test[[1]][[1]]), c("disparity"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]]))

    ## Test with group test
    data <- add.tree(disparity_group, tree, replace = TRUE)
    test <- get.pgls.data(data)
    expect_equal(length(test), 1)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(50, 2))
    expect_equal(colnames(test[[1]][[1]]), c("disparity", "group"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Test with multiple matrices
    data <- dispRity(data = matrices_list,  tree = trees_list[[1]], metric = centroids)
    test <- get.pgls.data(data)
    expect_equal(length(test), 3)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(30, 1))
    expect_equal(colnames(test[[1]][[1]]), c("disparity"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Test with multiple matrices (grouped)
    data <- dispRity(data = matrices_groups,  tree = trees_list[[1]], metric = centroids)
    test <- get.pgls.data(data)
    expect_equal(length(test), 3)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(30, 2))
    expect_equal(colnames(test[[1]][[1]]), c("disparity", "group"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Test with multiple trees
    data <- dispRity(data = matrices_list[[1]],  tree = trees_list, metric = centroids)
    test <- get.pgls.data(data)
    expect_equal(length(test) , 4)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(30, 1))
    expect_equal(colnames(test[[1]][[1]]), c("disparity"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Test with multiple trees (grouped)
    data <- dispRity(data = matrix_groups,  tree = trees_list, metric = centroids)
    test <- get.pgls.data(data)
    expect_equal(length(test) , 4)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(30, 2))
    expect_equal(colnames(test[[1]][[1]]), c("disparity", "group"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Test with the same number of trees and matrices
    data <- dispRity(data = matrices_list,  tree = trees_list[1:3], metric = centroids)
    test <- get.pgls.data(data)
    expect_equal(length(test), 3)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(30, 1))
    expect_equal(colnames(test[[1]][[1]]), c("disparity"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Test with the same number of trees and matrices (grouped)
    data <- dispRity(data = matrix_groups,  tree = trees_list[1:3], metric = centroids)
    test <- get.pgls.data(data)
    expect_equal(length(test), 3)
    expect_is(test[[1]]$data, "data.frame")
    expect_is(test[[1]]$phy, "phylo")
    expect_equal(dim(test[[1]][[1]]), c(30, 2))
    expect_equal(colnames(test[[1]][[1]]), c("disparity", "group"))
    expect_equal(rownames(test[[1]][[1]]), rownames(data$matrix[[1]])[unlist(lapply(data$subsets, `[[`, "elements"))])

    ## Error (not the same numbers)
    data <- dispRity(data = matrices_groups,  tree = trees_list, metric = centroids)
    error <- capture_error(test <- get.pgls.data(data))
    expect_equal(error[[1]], "Data must either same number of matrices (12) and trees (4) or just one tree or matrix combined with respectively with multiple matrices or trees.")
})

test_that("pgls works", {
    ## Testing super simple
    test <- pgls.dispRity(disparity_base, tree = nonode_tree)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")

    ## Testing group example
    test <- pgls.dispRity(disparity_group, tree = nonode_tree)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ group and model = BM")

    ## Wrapped up test
    disparity <- dispRity(BeckLee_mat50, metric = centroids, tree = BeckLee_tree)
    test <- test.dispRity(disparity, test = pgls.dispRity)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")

    ## Create a list of trees and matrices
    trees_list <- replicate(10, rcoal(30), simplify = FALSE)
    class(trees_list) <- "multiPhylo"
    matrices_list <- space.maker(elements = 30, dimensions = 5, distribution = rnorm, elements.names = trees_list[[1]]$tip.label, replicates = 3)

    ## Disparity for one matrix and one tree
    data <- dispRity(data = matrices_list[[1]], tree = trees_list[[1]], metric = edge.length.tree)
    test <- pgls.dispRity(data)
    expect_is(test, "phylolm")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")

    ## Disparity for one matrix but multiple trees
    data <- dispRity(data = matrices_list[[1]], tree = trees_list, metric = centroids)
    test <- pgls.dispRity(data)
    expect_is(test, c("dispRity", "pgls.dispRity"))
    expect_equal(length(test), 10)
    expect_is(test[[1]], "phylolm")
    expect_equal(test[[1]]$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")
})

test_that("options parsed correctly", {
    ## Test with different formulas
    test <- pgls.dispRity(disparity_group, tree = nonode_tree, formula = disparity ~ 1)
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM")

    ## Test with different model
    test <- pgls.dispRity(disparity_group, tree = nonode_tree, model = "EB")
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ group and model = EB")

    ## Test with additional argument
    test <- pgls.dispRity(disparity_group, tree = nonode_tree, starting.value = 0)
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ group and model = BM")

    ## Test with optim parameters
    test <- pgls.dispRity(disparity_group, tree = nonode_tree, optim = list(method = "Nelder-Mead"))
    expect_equal(test$call, "dispRity interface of phylolm using: formula = disparity ~ group and model = BM")
})

test_that("associated S3s work", {

    ## print
    ## normal test
    set.seed(1)
    data <- dispRity(data = matrices_list, tree = trees_list[1:3], metric = centroids)
    test <- pgls.dispRity(data)
    text <- capture.output(test)
    expect_equal(text, c(
        "phylolm test (pgls) applied to 3 disparity estimates"               ,
        "using the formula: disparity ~ 1 and the model: BM"                 ,
        ""                                                                   ,
        "         median       sd"                                           ,
        "aic    103.6818 30.51387"                                           ,
        "logLik -49.8409 15.25694"                                           ,
        ""                                                                   ,
        "Parameter estimate(s) using ML:"                                    ,
        "         median       sd"                                           ,
        "sigma2 13.92685 8.516613"                                           ,
        ""                                                                   ,
        "Coefficients:"                                                      ,
        "              median        sd"                                     ,
        "(Intercept) 2.109776 0.1719198"                                     ,
        ""                                                                   ,
        "You can access individual models by using their index (e.g. x[[1]])",
        "or summarise and plot all models using summary(x) or plot(x)." )
    )
    ## Summary works too
    text <- capture.output(summary(test))
    expect_equal(text, c(
        ""                                                                                   ,
        "Call:"                                                                              ,
        "[1] \"dispRity interface of phylolm using: formula = disparity ~ 1 and model = BM\"",
        "[2] \"The statistics are calculated based on the median estimates of 3 models.\"   ",
        ""                                                                                   ,
        "   AIC logLik "                                                                     ,
        "103.68 -49.84 "                                                                     ,
        ""                                                                                   ,
        "Raw residuals:"                                                                     ,
        "     Min       1Q   Median       3Q      Max "                                      ,
        "-0.97989 -0.41799 -0.06967  0.36557  1.11200 "                                      ,
        ""                                                                                   ,
        "Mean tip height: 2.282328"                                                          ,
        "Parameter estimate(s) using ML:"                                                    ,
        "sigma2: 13.92685 "                                                                  ,
        ""                                                                                   ,
        "Coefficients:"                                                                      ,
        "            Estimate StdErr t.value p.value"                                        ,
        "(Intercept)   2.1098 2.6163  0.8064  0.4266"                                        ,
        ""                                                                                   ,
        "R-squared:     0\tAdjusted R-squared:     0 ")
    )
    ## Plotting!
    expect_null(plot(test, xlab = "hahah", col = "blue"))

    ## grouped test
    set.seed(1)
    data <- dispRity(data = matrices_groups, tree = trees_list[1:3], metric = centroids)
    test <- pgls.dispRity(data)
    text <- capture.output(test)
    expect_equal(text, c(
        "phylolm test (pgls) applied to 3 disparity estimates"               ,
        "using the formula: disparity ~ group and the model: BM"             ,
        ""                                                                   ,
        "         median       sd"                                           ,
        "aic    102.3646 29.07677"                                           ,
        "logLik -48.1823 14.53838"                                           ,
        ""                                                                   ,
        "Parameter estimate(s) using ML:"                                    ,
        "       median       sd"                                             ,
        "sigma2 12.469 7.370084"                                             ,
        ""                                                                   ,
        "Coefficients:"                                                      ,
        "                 median        sd"                                  ,
        "(Intercept)  2.03348693 0.3301852"                                  ,
        "groupgroup2 -0.04111237 0.4920793"                                  ,
        ""                                                                   ,
        "You can access individual models by using their index (e.g. x[[1]])",
        "or summarise and plot all models using summary(x) or plot(x)."  )
    )
    ## Summary works too
    text <- capture.output(summary(test))
    expect_equal(text, c(
        ""                                                                                       ,
        "Call:"                                                                                  ,
        "[1] \"dispRity interface of phylolm using: formula = disparity ~ group and model = BM\"",
        "[2] \"The statistics are calculated based on the median estimates of 3 models.\"       ",
        ""                                                                                       ,
        "   AIC logLik "                                                                         ,
        "102.36 -48.18 "                                                                         ,
        ""                                                                                       ,
        "Raw residuals:"                                                                         ,
        "     Min       1Q   Median       3Q      Max "                                          ,
        "-1.09212 -0.26948 -0.05383  0.40231  0.76508 "                                          ,
        ""                                                                                       ,
        "Mean tip height: 2.282328"                                                              ,
        "Parameter estimate(s) using ML:"                                                        ,
        "sigma2: 12.469 "                                                                        ,
        ""                                                                                       ,
        "Coefficients:"                                                                          ,
        "             Estimate    StdErr t.value p.value"                                        ,
        "(Intercept)  2.033487  2.591239  0.7848  0.4392"                                        ,
        "groupgroup2 -0.041112  0.369139 -0.1114  0.9121"                                        ,
        ""                                                                                       ,
        "R-squared: 0.003123\tAdjusted R-squared: -0.03248 ")
    )
    expect_null(plot(test))
})


