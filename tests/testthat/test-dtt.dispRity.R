#TESTING dtt.dispRity

#context("dtt.dispRity")

test_that("dispRity and dtt give the same results", {

    library(geiger)
    geo = get(data(geospiza))

    ## avg.sq equivalent
    average.sq <- function(X) mean(pairwise.dist(X)^2)

    ## Simple disparity calculation
    expect_equal(
        geiger::disparity(data = geo$dat, index = "avg.sq")
        ,
        summary(dispRity(geo$dat, metric = average.sq), digits = 10)$obs
        )

    ## Disparity per clades
    expect_warning(
        geiger_disp <- geiger::disparity(phy = geo$phy, data = geo$dat, index = "avg.sq")
        )

    data_cleaned <- clean.data(geo$dat, geo$phy)
    dispRity_disp <- summary(dispRity(custom.subsets(data_cleaned$data, data_cleaned$tree), metric = average.sq), digits = 10)$obs

    expect_equal(as.vector(geiger_disp), as.vector(dispRity_disp))


    geiger_data <- get(data(geospiza))

    ## Calculate the disparity of the dataset using dtt::geiger
    set.seed(1)
    warn <- capture_warning(geiger::dtt(phy = geiger_data$phy, data = geiger_data$dat, nsim = 100, plot = FALSE, calculateMDIp = TRUE))
    expect_equal(as.character(warn), "simpleWarning in treedata(phy, data): The following tips were not found in 'data' and were dropped from 'phy':\n\tolivacea\n")
    expect_warning(geiger_dtt <- geiger::dtt(phy = geiger_data$phy, data = geiger_data$dat, nsim = 100, plot = FALSE, calculateMDIp = TRUE))

    ## The average squared pairwise distance metric (used in geiger::dtt)
    average.sq <- function(X) mean(pairwise.dist(X)^2)

    ## Calculate the disparity of the dataset using dtt.dispRity
    set.seed(1)
    expect_warning(dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 100))
    plot(dispRity_dtt)
    expect_error(dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = var, tree = geiger_data$phy, nsim = 100))

    ## Error when recycling dispRity objects
    data(disparity)
    expect_error(dtt.dispRity(data = disparity, metric = var, tree = BeckLee_tree, nsim = 10))

    ## Error when providing wrong dimensions metric
    error <- capture_error(dtt.dispRity(data = geiger_data$dat, metric = var, tree = geiger_data$phy, nsim = 100))
    expect_equal(error[[1]], "var metric must contain at least a dimension-level 1 or a dimension-level 2 metric.\nFor more information, see ?make.metric.")

    ## Tree has no root time
    data(BeckLee_tree)
    data(BeckLee_mat50)
    dummy <- BeckLee_tree 
    dummy$root.time <- NULL

    tree_short <- drop.tip(BeckLee_tree, tip = BeckLee_tree$tip.label[1])

    warning <- capture_warning(dtt.dispRity(data = BeckLee_mat50, metric = c(sum, variances), tree = tree_short, nsim = 2))
    expect_equal(warning[[1]], "The following element(s) was not present in the tree: Daulestes.")

    test <- dtt.dispRity(data = BeckLee_mat50, metric = c(sum, variances), tree = BeckLee_tree, nsim = 2)
    expect_is(test, c("dispRity", "dtt"))



    ## Same output
    expect_equal(length(geiger_dtt)+2, length(dispRity_dtt))
    expect_equal(unlist(lapply(geiger_dtt[-5], length)), unlist(lapply(dispRity_dtt[-c(5,6,7)], length)))

    ## dispRity object
    # data(disparity)
    # data(BeckLee_tree)
    # expect_warning(dtt_data <- dtt.dispRity(data = disparity, metric = average.sq, tree = BeckLee_tree, nsim = 5))
    # expect_is(dtt_data, "dispRity", "dtt")

    ## p-values
    geiger_data$phy <- drop.tip(geiger_data$phy, "olivacea")
    ## two-sided
    set.seed(1)
    dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 100, alternative = "two-sided")
    expect_equal(dispRity_dtt$p_value, 0.81)
    ## lesser
    set.seed(1)
    dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 100, alternative = "lesser")
    expect_equal(dispRity_dtt$p_value, 0.54)
    ## two-sided
    set.seed(1)
    dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 100, alternative = "greater")
    expect_equal(dispRity_dtt$p_value, 0.46)

    ## But also works without testing
    dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 0)
    expect_is(dispRity_dtt, c("dispRity", "dtt"))
    expect_equal(names(dispRity_dtt), c("dtt", "times"))
})