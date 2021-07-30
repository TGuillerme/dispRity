## Test
test_that("select.axes works", {

    ## Basic test
    data(demo_data)
    data <- demo_data$wright$matrix[[1]]
    group <- unlist(demo_data$wright$subsets, recursive = FALSE)
    group <- lapply(group, c)

    ## Simple test
    test <- select.axes(data, group)
    expect_is(test, c("dispRity", "axes"))
    expect_equal(names(test), c("dimensions", "dim.list", "var", "scaled.var", "cumsum.var", "call"))
    expect_is(test[[1]], "integer")
    expect_is(test[[2]], "list")
    expect_is(test[[3]], "list")
    expect_is(test[[3]][[1]], "numeric")

    ## Recycling test
    test <- select.axes(demo_data$healy, inc.threshold = FALSE)
    expect_is(test, c("dispRity", "axes"))
    expect_equal(names(test), c("dimensions", "dim.list", "var", "scaled.var", "cumsum.var", "call"))
    expect_is(test[[1]], "integer")
    expect_is(test[[2]], "list")
    expect_is(test[[3]], "list")
    expect_is(test[[3]][[1]], "numeric")

    ## No group
    test <- select.axes(demo_data$beck$matrix[[1]])
    expect_is(test, c("dispRity", "axes"))
    expect_equal(names(test), c("dimensions", "dim.list", "var", "scaled.var", "cumsum.var", "call"))
    expect_is(test[[1]], "integer")
    expect_is(test[[2]], "list")
    expect_is(test[[3]], "list")
    expect_is(test[[3]][[1]], "numeric")
})

## Test print
test_that("print.select.axes works", {

})

## Test summary
test_that("summary.select.axes works", {

})

## Test plot
test_that("plot.select.axes works", {

})