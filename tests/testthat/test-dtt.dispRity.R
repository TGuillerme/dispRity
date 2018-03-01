#TESTING dtt.dispRity

context("dtt.dispRity")

test_that("dispRity and dtt give the same results", {

    library(geiger)
    geo = get(data(geospiza))

    ## avg.sq equivalent
    average.sq <- function(X) mean(pairwise.dist(X)^2)

    ## Simple disparity calculation
    expect_equal(
        disparity(data = geo$dat, index = "avg.sq")
        ,
        summary(dispRity(geo$dat, metric = average.sq), digits = 10)$obs
        )

    ## Disparity per clades
    expect_warning(
        geiger_disp <- disparity(phy = geo$phy, data = geo$dat, index = "avg.sq")
        )

    data_cleaned <- clean.data(geo$dat, geo$phy)
    dispRity_disp <- summary(dispRity(custom.subsets(data_cleaned$data, data_cleaned$tree), metric = average.sq), digits = 10)$obs

    expect_equal(as.vector(geiger_disp), as.vector(dispRity_disp))

    ## .dtt function from: https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R
    # expect_equal(
    #     dispRity_dtt <- .dtt.dispRity(data_cleaned$tree, data_cleaned$data, metric = average.sq)
    #     ,
    #     geiger_dtt <- .dtt(data_cleaned$tree, data_cleaned$data, disp = "avg.sq")
    #     )

    # expect_warning(geiger_dtt <- dtt(phy = geo$phy, data = geo$dat, nsim = 0, plot = FALSE, index = "avg.sq"))

    # dtt.dispRity.modif <- geiger::dtt
    # body(dtt.dispRity.modif)[[2]] <- substitute(metric <- index)
    # body(dtt.dispRity.modif)[[4]] <- substitute(dtt.data <- .dtt.dispRity(td$phy, td$data, metric = metric))

    # expect_warning(dispRity_dtt <- dtt.dispRity.modif(phy = geo$phy, data = geo$dat, nsim = 0, plot = FALSE, index = average.sq))

    # expect_equal(geiger_dtt[[1]], dispRity_dtt[[1]])
    # expect_equal(geiger_dtt[[2]], dispRity_dtt[[2]])


    geiger_data <- get(data(geospiza))

    ## Calculate the disparity of the dataset using dtt::geiger
    set.seed(1)
    expect_warning(geiger_dtt <- dtt(phy = geiger_data$phy, data = geiger_data$dat, nsim = 100, plot = FALSE, calculateMDIp = TRUE))

    ## The average squared pairwise distance metric (used in geiger::dtt)
    average.sq <- function(X) mean(pairwise.dist(X)^2)

    ## Calculate the disparity of the dataset using dtt.dispRity
    set.seed(1)
    expect_warning(dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 100))
    plot(dispRity_dtt)
     
    ## Same output
    expect_equal(length(geiger_dtt), length(dispRity_dtt))
    expect_equal(unlist(lapply(geiger_dtt[-5], length)), unlist(lapply(dispRity_dtt[-5], length)))

})