#TESTING plot.dispRity

context("plot.dispRity (non-graphic)") 
## Loading the data
load("disparity")


#######################
#Testing
#######################

## Default settings
test_that("set.default works", {
    sum_data <- summary(disparity)
    expect_error(
        set.default("1", disparity, elements = FALSE, ylim = "default", xlab = "default", ylab = "default", col = "default", rarefaction = FALSE)
        )
    expect_error(
        set.default(sum_data, "1", elements = FALSE, ylim = "default", xlab = "default", ylab = "default", col = "default", rarefaction = FALSE)
        )
    test <- set.default(sum_data, disparity, elements = FALSE, ylim = "default", xlab = "default", ylab = "default", col = "default", rarefaction = FALSE)
    expect_equal(
        round(test[[1]], 5)
        , round(c(0.90160, 2.01858), 5))
    expect_equal(
        test[[2]]
        , "Series")
    expect_equal(
        test[[3]]
        , "c(median, centroids)")
    expect_equal(
        test[[4]]
        , c("black", "#BEBEBE", "#D3D3D3"))

    test <- set.default(sum_data, disparity, elements = TRUE, ylim = c(1,2), xlab = "Bla", ylab = c("Bli", "Blu"), col = c("pink", "knip"), rarefaction = FALSE)
    expect_equal(
        round(test[[1]], 5)
        , round(c(1, 2), 5))
    expect_equal(
        test[[2]]
        , "Bla")
    expect_equal(
        test[[3]]
        , c("Bli", "Blu"))
    expect_equal(
        test[[4]]
        , c("pink", "knip"))
})

function(summarised_data, data, elements, ylim, xlab, ylab, col, rarefaction)


#Extract summary
#extract.summary(summarised_data, what=2, which.rare=3))


#Get series
#get.series
