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
    ## random
    test_mat <- matrix(rnorm(120), 60, 2, dimnames = list(c(1:60)))
    x <- select.axes(test_mat)
    test <- capture_output(print.dispRity(x))
    expect_equal(test, "The first 2 dimensions are needed to express at least 95% of the variance in the whole trait space.\nYou can use x$dimensions to select them or use plot(x) and summary(x) to summarise them.")
    x <- select.axes(test_mat, group = list("A" = 1:10, "B" = 11:20, "C" = 1:30, "D" = 31:40, "E" = 10:50, "F" = c(15:35)))
    test <- capture_output(print.dispRity(x))
    expect_equal(test, "The first 2 dimensions are needed to express at least 95% of the variance in the 7 following groups: A, B, C, D, E, ...\nYou can use x$dimensions to select them or use plot(x) and summary(x) to summarise them.")
    ## Recycling
    data(demo_data)
    x <- select.axes(demo_data$healy, threshold = 0.99)
    test <- capture_output(print.dispRity(x))
    expect_equal(test, "The first 6 dimensions are needed to express at least 99% of the variance in the following groups: ectotherms, endotherms, whole_space.\nYou can use x$dimensions to select them or use plot(x) and summary(x) to summarise them.")
})

## Test summary
test_that("summary.select.axes works", {

    ## random
    test_mat <- matrix(rnorm(120), 60, 2, dimnames = list(c(1:60)))
    data <- select.axes(test_mat)
    test <- summary(data)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(1, ncol(test_mat)*2))
    expect_equal(rownames(test), "whole_space")
    expect_equal(colnames(test), c("dim.1.var", "dim.1.sum", "dim.2.var", "dim.2.sum"))
    
    data <- select.axes(test_mat, group = list("A" = 1:10, "B" = 11:20, "C" = 1:30, "D" = 31:40, "E" = 10:50, "F" = c(15:35)))
    test <- summary(data)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(7, ncol(test_mat)*2))
    expect_equal(rownames(test), c(LETTERS[1:6], "whole_space"))
    expect_equal(colnames(test), c("dim.1.var", "dim.1.sum", "dim.2.var", "dim.2.sum"))

    ## Recycling
    data(demo_data)
    data <- select.axes(demo_data$healy, threshold = 0.99)
    test <- summary(data)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(3, ncol(demo_data$healy$matrix[[1]])*2))
    expect_equal(rownames(test), c("ectotherms", "endotherms", "whole_space"))
    expect_equal(colnames(test), paste0(rep(paste0("PC",1:6), each = 2), c(".var", ".sum")))
})

## Test plot
test_that("plot.select.axes works", {
    ## random
    test_mat <- matrix(rnorm(120), 60, 2, dimnames = list(c(1:60)))
    data <- select.axes(test_mat)
    expect_null(plot(data))
    
    data <- select.axes(test_mat, group = list("A" = 1:10, "B" = 11:20, "C" = 1:30, "D" = 31:40, "E" = 10:50, "F" = c(15:35)))
    expect_null(test <- plot(data, main = c(letters[1:3]), col = c("blue", "orange", "green"), xlim = 30, ylab = "aha"))
    
    ## Recycling
    data(demo_data)
    data <- select.axes(demo_data$beck, threshold = 0.5)
    expect_null(plot(data, ylim = c(0,1)))
})