context("serial metrics architecture")

## Base test
test_that("serial metrics works", {

    set.seed(1)
    big_matrix <- matrix(rnorm(60), ncol = 5, nrow = 12, dimnames = list(letters[1:12], 1:5))
    matrix1 <- big_matrix[1:3,]
    matrix2 <- big_matrix[4:7,]
    matrix3 <- big_matrix[-c(1:7),]
    overlap_matrix <- big_matrix[3:8,]

    ## Variance/covar difference
    serial.test.level3 <- function(matrix, matrix2) {
        return(var(matrix) - var(matrix2)) 
    }
    ## col variances differences
    serial.test.level2 <- function(matrix, matrix2) {
        return(variances(matrix) - variances(matrix2))
    }
    ## sd (var) differences
    serial.test.level1 <- function(matrix, matrix2) {
        return(sd(variances(matrix)) - sd(variances(matrix2)))
    }

    ## metrics work
    expect_equal(dim(serial.test.level3(matrix1, matrix2)), c(5, 5))
    expect_equal(length(serial.test.level2(matrix1, matrix2)), 5) 
    expect_equal_round(serial.test.level1(matrix1, matrix2), 0.5622892, 7)

    ## Custom dispRity
    test_groups <- custom.subsets()

    ## dispRity works with manual series entered

    ## dispRity works with automatic custom/chrono detections

    ## dispRity works with with detecting wrong serial metric

    

})

