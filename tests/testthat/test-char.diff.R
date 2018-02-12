context("char.diff")

test_that("internal function char.diff", {

    A <- c(1,0,0,0,0)
    B <- c(0,1,1,1,1)
    C <- c(9,8,8,8,8)

    ## Convert character works (translate digits in letters)
    expect_equal(convert.character(A), c("B", rep("A", 4)))

    ## Normalise character works (translate digits in normalised way)
    expect_true(all(is.na(normalise.character(A, c("a","b")))))
    expect_equal(normalise.character(A, c(0,1)), normalise.character(B, c(1,0)))
    expect_equal(normalise.character(A, c(0,1)), normalise.character(C, c(9,8)))
    expect_equal(normalise.character(B, c(1,0)), normalise.character(C, c(9,8)))

    ## char.diff_R is the same as char.diff for a list
    matrix <- list(A, B)
    expect_equal(char.diff_R(matrix[[1]], matrix[[2]]), char.diff(list(A,B)))

    ## char.diff works with characters
    matrix <- list(as.character(A), as.character(B))
    expect_equal(char.diff_R(matrix[[1]], matrix[[2]]), char.diff(matrix))

})

test_that("char.diff pair", {
    A <- c(1,0,0,0,0)
    B <- c(0,1,1,1,1)

    ## Difference is 0
    expect_equal(char.diff(list(A,B)), 0)
    ## Difference is triangular
    expect_equal(char.diff(list(A,B)), char.diff(list(A,B)))

    C <- c(1,1,0,0,0)

    ## Difference is 0.4
    expect_equal(char.diff(list(A,C)), 0.4)
    ## Difference is triangular
    expect_equal(char.diff(list(A,C)), char.diff(list(C,A)))

    D <- c(0,1,1,0,0)

    ## Difference is 0.4
    expect_equal(char.diff(list(A,D)), 0.8)
    ## Difference is triangular
    expect_equal(char.diff(list(A,D)), char.diff(list(D,A)))

    E <- c(1,0,0,1,1)

    ## Difference is equal to D
    expect_equal(char.diff(list(D,E)), 0)
    ## Difference is triangular (with D)
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

    ## Converting matrices into numeric
    set.seed(1)
    matrix_alpha <- matrix(sample(c(0,1), 100, replace = TRUE), 10)
    matrix_alpha <- apply(matrix_alpha, 2, as.character)
    capture_warnings(test <- char.diff(matrix_alpha))
    expect_is(test, c("matrix", "char.diff"))
})


test_that("char.diff plot functions", {

    ## Getting the max/min x/y from a density
    set.seed(1)
    density <- density(rnorm(20))
    expect_equal(round(get.max.x(density), 5), round(2.860749, 5))
    expect_equal(round(get.min.x(density), 5), round(-3.480168, 5))
    expect_equal(round(get.max.y(density), 5), round(0.4420556, 5))
    expect_equal(round(get.min.y(density), 5), round(0.0005316588, 5))

    ## Getting columns with not enough data (TRUE if <= 2 data)
    expect_true(select.nas(c(NA, NA, NA, NA)))
    expect_true(select.nas(c(1, NA, NA, NA)))
    expect_true(select.nas(c(1, 2, NA, NA)))
    expect_false(select.nas(c(1,2,3,NA)))

})


test_that("char.diff plot (graphic)", {

    ## Pairwise comparisons in a morphological matrix
    morpho_matrix <- matrix(sample(c(0,1), 100, replace = TRUE), 10)

    ## Plotting a matrix
    capture_warnings(test <- plot.char.diff(morpho_matrix))
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## Plotting the density profile of a char.diff object
    capture_warnings(char.diff_matrix <- char.diff(morpho_matrix))
    test <- plot(char.diff_matrix, type = "density")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
})