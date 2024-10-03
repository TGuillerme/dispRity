#TESTING make.metric

#context("make.metric")

#Loading the data
#load("test_data.rda")
#data<-test_data$ord_data_tips

test_that("check.metric works", {
    # expect_error(check.metric(1))
    expect_equal(check.metric(sum), "summary.metric")
    expect_equal(check.metric(var), "class.metric")
#    error <- capture_error(check.metric(check.metric))
#    expect_equal(error[[1]], "Invalid metric.")
})

test_that("Output is correct", {
    expect_error(
        make.metric("a")
        )
    expect_error(
        make.metric(1)
        )
    expect_error(
        make.metric(function(x)as.character(x))
        )

    fun <- function(x, arg) {
        if(arg == TRUE){
            return(mean(x))
        } else {
            return(mean(x))
        }
    }

    expect_error(
        make.metric(fun, silent = FALSE)
        )

    fun2 <- function(x) {
        return("try-error")
    }
    expect_error(
        make.metric(fun2, silent = FALSE)
        )


    ## Verbose
    test <- function(x) as.character(x)
    error <- capture_error(make.metric(test, verbose = TRUE))
    expect_equal(error[[1]],
        "The provided metric function generated an error or a warning!\nDoes the following work?\n    test(matrix(rnorm(20), 5,4))\nThe problem may also come from the optional arguments (...) in test."
        )

    error <- capture_error(make.metric(test, silent = FALSE))
    expect_equal(error[[1]],
        "The provided metric function generated an error or a warning!\nDoes the following work?\n    test(matrix(rnorm(20), 5,4))\nThe problem may also come from the optional arguments (...) in test."
        )

    expect_error(
        make.metric(lapply)
        )

    expect_equal(make.metric(mean, silent=TRUE), list(type = "level1", tree = FALSE, dist.help = NULL, reduce.dist = NULL))
    expect_equal(make.metric(ranges, silent=TRUE), list(type = "level2", tree = FALSE, dist.help = NULL, reduce.dist = NULL))
    expect_equal(make.metric(var, silent=TRUE), list(type = "level3", tree = FALSE, dist.help = NULL, reduce.dist = NULL))
    expect_equal(
        make.metric(function(x)mean(var(x)), silent=TRUE)$type, "level1"
        )
    expect_equal(
        make.metric(function(x)variances(var(x)), silent=TRUE)$type, "level2"
        )
    expect_equal(
        make.metric(function(x)var(var(x)), silent=TRUE)$type, "level3"
        )
    expect_equal(
        make.metric(function(x)sd(variances(var(x))), silent=TRUE)$type, "level1"
        )

    ## Same with data.dim
    expect_equal(
        make.metric(function(x)variances(var(x)), silent=TRUE, data.dim = c(3, 2))$type, "level2"
        )
    expect_equal(
        make.metric(function(x)var(var(x)), silent=TRUE, data.dim = c(33, 5))$type, "level3"
        )
    expect_equal(
        make.metric(function(x)sd(variances(var(x))), silent=TRUE, data.dim = c(3, 10))$type, "level1"
        )

    expect_equal(capture.output(make.metric(var)),
        c("var outputs a matrix object.", "var is detected as being a dimension-level 3 function.", "Additional dimension-level 2 and/or 1 function(s) will be needed."))

    expect_equal(capture.output(make.metric(sd)), 
        c("sd outputs a single value.", "sd is detected as being a dimension-level 1 function."))

    expect_equal(capture.output(make.metric(variances)), 
        c("variances outputs a matrix object.", "variances is detected as being a dimension-level 2 function."))

    error <- capture_error(make.metric(make.metric))
    expect_equal(error[[1]], "The provided metric function generated an error or a warning!\nDoes the following work?\n    make.metric(matrix(rnorm(20), 5,4))\nThe problem may also come from the optional arguments (...) in make.metric.")

    ## With between.groups
    between.groups.metric <- function(matrix, matrix2) return(42)
    between.groups.metric2 <- function(matrix, matrix2, option = TRUE) return(c(1,2,3,4))

    expect_equal(make.metric(between.groups.metric, silent = TRUE)$type, "level1")
    expect_equal(make.metric(between.groups.metric, silent = TRUE, check.between.groups = TRUE), list("type" = "level1", "between.groups" = TRUE, "tree" = FALSE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(between.groups.metric2, option = FALSE, silent = TRUE)$type, "level2")
    expect_equal(make.metric(between.groups.metric2, option = "bla", silent = TRUE, check.between.groups = TRUE), list("type" = "level2", "between.groups" = TRUE, "tree" = FALSE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(mean, silent = TRUE, check.between.groups = TRUE), list("type" = "level1", "between.groups" = FALSE, "tree" = FALSE, "dist.help" = NULL, "reduce.dist" = NULL))

    ## Metrics with tree or phy argument
    between.groups.metric <- function(matrix, matrix2, tree = TRUE) return(c(1,2,3,4))
    between.groups.metric2 <- function(matrix, matrix2, phy = TRUE) return(c(1,2,3,4))
    normal.metric <- function(matrix, tree) return(42)
    normal.metric2 <- function(matrix, phy) return(42)
    expect_equal(make.metric(normal.metric, tree = rtree(5), silent = TRUE), list(type = "level1", tree = TRUE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(normal.metric2, phy = rtree(5), silent = TRUE), list(type = "level1", tree = FALSE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(between.groups.metric, tree = rtree(5), silent = TRUE, check.between.groups = TRUE), list("type" = "level2", "between.groups" = TRUE, tree = TRUE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(between.groups.metric2, phy = rtree(5), silent = TRUE, check.between.groups = TRUE), list("type" = "level2", "between.groups" = TRUE, tree = FALSE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(normal.metric, silent = TRUE), list(type = "level1", tree = TRUE, "dist.help" = NULL, "reduce.dist" = NULL))
    expect_equal(make.metric(normal.metric2, silent = TRUE), list(type = "level1", tree = FALSE, "dist.help" = NULL, "reduce.dist" = NULL))
    
})