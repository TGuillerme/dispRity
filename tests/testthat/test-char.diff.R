context("char.diff")

test_that("char.diff pair", {
    A <- c(1,0,0,0,0)
    B <- c(0,1,1,1,1)

    #Difference is 0
    expect_equal(char.diff(list(A,B)), 0)
    #Difference is triangular
    expect_equal(char.diff(list(A,B)), char.diff(list(A,B)))

    C <- c(1,1,0,0,0)

    #Difference is 0.4
    expect_equal(char.diff(list(A,C)), 0.4)
    #Difference is triangular
    expect_equal(char.diff(list(A,C)), char.diff(list(C,A)))

    D <- c(0,1,1,0,0)

    #Difference is 0.4
    expect_equal(char.diff(list(A,D)), 0.8)
    #Difference is triangular
    expect_equal(char.diff(list(A,D)), char.diff(list(D,A)))

    E <- c(1,0,0,1,1)

    #Difference is equal to D
    expect_equal(char.diff(list(D,E)), 0)
    #Difference is triangular (with D)
    expect_equal(char.diff(list(A,E)), char.diff(list(A,D)))
})

## Matrices                     #A,B,C,D,E
matrix_binary <- matrix(data = c(1,0,1,0,1,
                                 0,1,1,1,0,
                                 0,1,0,1,0,
                                 0,1,0,0,1,
                                 0,1,0,0,1), ncol = 5, byrow = TRUE)
colnames(matrix_binary) <- LETTERS[1:5]

                               #A,B,C,D,E,F,G
matrix_multi <- matrix(data = c(1,2,0,0,1,2,1,
                                2,3,1,2,2,0,2,
                                0,4,2,1,1,2,2,
                                0,4,0,0,0,1,0,
                                0,4,0,0,0,1,0), ncol = 7, byrow = TRUE)
colnames(matrix_multi) <- LETTERS[1:7]

                                #A,B,C,D
matrix_simple <- matrix(data = c(1,NA,3,NA,
                                 7,7,2,2,
                                 1,1,1,0), ncol = 3, byrow = FALSE)
colnames(matrix_simple) <- LETTERS[1:3]

test_that("char.diff matrix", {
    tests <- list()
    expect_warning(tests[[1]] <- char.diff(matrix_simple))
    expect_warning(tests[[2]] <- char.diff(matrix_multi))
    expect_warning(tests[[3]] <- char.diff(matrix_binary))

    expect_dims <- list(c(3,3), c(7,7), c(5,5))
    expect_diff <- list(c(0.0, 0.0, 1.0, 0.0, 0.0, 0.5, 1.0, 0.5, 0.0), 
                        c(0.0, 0.0, 0.8, 0.8, 0.4, 0.4, 0.4, 0.0, 0.0, 0.8, 0.8, 0.4, 0.4, 0.4, 0.8, 0.8, 0.0, 0.0, 0.8, 0.8, 0.8, 0.8, 0.8, 0.0, 0.0, 0.8, 0.8, 0.8, 0.4, 0.4, 0.8, 0.8, 0.0, 0.0, 0.4, 0.4, 0.4, 0.8, 0.8, 0.0, 0.0, 0.4, 0.4, 0.4, 0.8, 0.8, 0.4, 0.4, 0.0),
                        c(0.0, 0.0, 0.4, 0.8, 0.8, 0.0, 0.0, 0.4, 0.8, 0.8, 0.4, 0.4, 0.0, 0.8, 0.8, 0.8, 0.8, 0.8, 0.0, 0.0, 0.8, 0.8, 0.8, 0.0, 0.0))

    for(test in 1:length(tests)) {
        expect_equal(
            dim(tests[[test]])
            , expect_dims[[test]])
        expect_equal(
            as.vector(tests[[test]])
            , expect_diff[[test]])
    }
})