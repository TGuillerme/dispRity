context("reduce.matrix")

## Test
test_that("reduce.matrix works", {

    set.seed(1)
    ## A 10*5 matrix
    matrix <- matrix(rnorm(50), 10, 5)
    na_matrix <- matrix
    ## Making sure some rows don't overlap
    na_matrix[1, 1:2] <- NA
    na_matrix[2, 3:5] <- NA
    ## Adding 50% NAs
    na_matrix[sample(1:50, 25)] <- NA

    ## Sanitizing
    expect_error(reduce.matrix("na_matrix", distance = "gower", by.row = TRUE, verbose = FALSE))
    expect_error(reduce.matrix(na_matrix, distance = "glower", by.row = TRUE, verbose = FALSE))
    expect_error(reduce.matrix(na_matrix, distance = "gower", by.row = 1, verbose = FALSE))
    expect_error(reduce.matrix(na_matrix, distance = "gower", by.row = TRUE, verbose = "a"))

    ## Testing the different options
    expect_is(reduce.matrix(as.data.frame(matrix), distance = "gower", by.row = TRUE, verbose = FALSE), "list")
    test_null <- reduce.matrix(matrix, distance = "gower", by.row = TRUE, verbose = FALSE)
    expect_warning(test_row <- reduce.matrix(na_matrix, distance = "gower", by.row = TRUE, verbose = FALSE))
    expect_warning(test_col <- reduce.matrix(na_matrix, distance = "gower", by.row = FALSE, verbose = FALSE))

    ## Right class
    expect_is(test_null , "list")
    expect_is(test_row , "list")
    expect_is(test_col , "list")
    expect_equal(names(test_null), c("rows.to.remove", "cols.to.remove"))
    expect_equal(names(test_row), c("rows.to.remove", "cols.to.remove"))
    expect_equal(names(test_col), c("rows.to.remove", "cols.to.remove"))

    ## Right outputs
    silent <- lapply(test_null, expect_null)
    expect_null(test_row[[2]])
    expect_null(test_col[[1]])
    expect_equal(test_row[[1]], c("9", "1"))
    expect_equal(test_col[[2]], c("3"))

    ## Verbose test
    expect_warning(test.verbose <- capture_messages(reduce.matrix(na_matrix, distance = "gower", by.row = TRUE, verbose = TRUE)))
    expect_equal(paste0(test.verbose, collapse = ""), "Searching for row(s) to remove:...Done.\n")


    ## Flipped!
    expect_warning(test_row <- reduce.matrix(t(na_matrix), distance = "gower", by.row = TRUE, verbose = FALSE))
    expect_warning(test_col <- reduce.matrix(t(na_matrix), distance = "gower", by.row = FALSE, verbose = FALSE))

    ## Right class
    expect_is(test_row , "list")
    expect_is(test_col , "list")
    expect_equal(names(test_row), c("rows.to.remove", "cols.to.remove"))
    expect_equal(names(test_col), c("rows.to.remove", "cols.to.remove"))

    ## Right outputs
    expect_null(test_row[[2]])
    expect_null(test_col[[1]])
    expect_equal(test_row[[1]], c("3"))
    expect_equal(test_col[[2]], c("9", "1"))
})
