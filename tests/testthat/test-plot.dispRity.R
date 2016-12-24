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

test_that("extract.from.summary works", {
    sum_data <- summary(disparity)
    expect_error(
        extract.from.summary("sum_data", what = 4, rarefaction = FALSE)
        )
    expect_null(
        extract.from.summary(sum_data, what = "4", rarefaction = FALSE)
        )
    expect_equal(
        length(extract.from.summary(sum_data, what = 4, rarefaction = 99))
        , 0)
    expect_equal(
        length(extract.from.summary(sum_data, what = 4, rarefaction = FALSE))
        , length(extract.from.summary(sum_data, what = "rows", rarefaction = FALSE)))
    expect_equal(
        extract.from.summary(sum_data, what = 1, rarefaction = FALSE)
        , as.factor(c(90,80,70,60,50,40,30)))
    expect_equal(
        extract.from.summary(sum_data, what = 2, rarefaction = 10)
        ,rep(10, 7))
    expect_equal(
        extract.from.summary(sum_data, what = 3, rarefaction = 5)
        ,as.numeric(rep(NA, 7)))
    expect_equal(
        extract.from.summary(sum_data, what = 4, rarefaction = FALSE)
        ,c(1.780,1.812,1.874,1.877,1.907,1.878,1.818))
})


#Extract summary
#extract.summary(summarised_data, what=2, which.rare=3))


#Get series
#get.series
