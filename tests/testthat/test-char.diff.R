context("char.diff")

test_that("char.diff pair", {
    A <- c(1,0,0,0,0)
    B <- c(0,1,1,1,1)

    ## Difference is 0
    expect_warning(expect_equal(char.diff(list(A,B)), 0))
    ## Difference is triangular
    expect_warning(expect_equal(char.diff(list(A,B)), char.diff(list(A,B))))

    C <- c(1,1,0,0,0)

    ## Difference is 0.25
    expect_warning(expect_equal(char.diff(list(A,C)), 0.25))
    ## Difference is triangular
    expect_warning(expect_equal(char.diff(list(A,C)), char.diff(list(C,A))))

    D <- c(0,1,1,0,0)

    ## Difference is 0.5
    expect_warning(expect_equal(char.diff(list(A,D)), 0.5))
    ## Difference is triangular
    expect_warning(expect_equal(char.diff(list(A,D)), char.diff(list(D,A))))

    E <- c(1,0,0,1,1)

    ## Difference is equal to D
    expect_warning(expect_equal(char.diff(list(D,E)), 0))
    ## Difference is triangular (with D)
    expect_warning(expect_equal(char.diff(list(A,E)), char.diff(list(A,D))))
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
    expect_warning(tests[[1]] <- round(char.diff(matrix_simple), digits = 7))
    expect_warning(tests[[2]] <- round(char.diff(matrix_multi), digits = 7))
    expect_warning(tests[[3]] <- round(char.diff(matrix_binary), digits = 7))

    expect_dims <- list(c(3,3), c(7,7), c(5,5))
    expect_diff <- list(c(0.0, 0.0, 1.0, 0.0, 0.0, 0.3333333, 1.0, 0.3333333, 0.0), 
                        c(0.00, 0.00, 0.50, 0.50, 0.25, 0.25, 0.25, 0.00, 0.00, 0.50, 0.50, 0.25, 0.25, 0.25, 0.50, 0.50, 0.00, 0.00, 0.75, 0.75, 0.75, 0.50, 0.50, 0.00, 0.00, 0.75, 0.75, 0.75, 0.25, 0.25, 0.75, 0.75, 0.00, 0.00, 0.25, 0.25, 0.25, 0.75, 0.75, 0.00, 0.00, 0.25, 0.25, 0.25, 0.75, 0.75, 0.25, 0.25, 0.00),
                        c(0.00, 0.00, 0.25, 0.50, 0.50, 0.00, 0.00, 0.25, 0.50, 0.50, 0.25, 0.25, 0.00, 0.75, 0.75, 0.50, 0.50, 0.75, 0.00, 0.00, 0.50, 0.50, 0.75, 0.00, 0.0)
                        )

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
    expect_warning(test <- char.diff(matrix_alpha))
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

    ## Errors
    expect_error(plot.char.diff("bob", type = "density"))
    expect_warning(expect_error(plot.char.diff(morpho_matrix, type = "density", legend.pos = 1)))
    expect_warning(expect_error(plot.char.diff(morpho_matrix, type = "density", main = c("main", "bob"))))
    expect_warning(expect_error(plot.char.diff(morpho_matrix, type = "density", col = "blue")))
    expect_warning(expect_error(plot.char.diff(morpho_matrix, type = "matrix", col = "blue")))
    expect_warning(expect_error(plot.char.diff(morpho_matrix, type = "density", xlab = c("main", "bob"))))
    expect_warning(expect_error(plot.char.diff(morpho_matrix, type = "density", ylab = c("main", "bob"))))


    ## Plotting a matrix
    capture_warnings(test <- plot.char.diff(morpho_matrix))
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## Plotting the density profile of a char.diff object
    capture_warnings(char.diff_matrix <- char.diff(morpho_matrix))
    test <- plot(char.diff_matrix, type = "density")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## With NA
    morpho_matrix[, 1] <- NA
    warn <- capture_warnings(test <- plot.char.diff(morpho_matrix))
    expect_equal(warn, "NAs introduced by coercion")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
    warn <- capture_warnings(test <- plot.char.diff(morpho_matrix, type = "density"))
    expect_equal(warn, "NAs introduced by coercion")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
})

test_that("different methods", {
    # Testing
    matrix <- do.call(cbind, list(A = c(0, 1, 0, 1, 1), #1, 2, 1, 2, 2
                                  B = c(0, 1, 1, 1, 1), #1, 2, 2, 2, 2
                                  C = c(0, 2, 2, 2, 2), #1, 2, 2, 2, 2
                                  D = c(0, 1, 2, 3, 4), #1, 2, 3, 4, 5
                                  E = c(1, 2, 1, 2, 2)))#1, 2, 1, 2, 2

    ## Hamming differences
    expect_warning(test_hamming <- round(char.diff(matrix, method = "hamming"), 2))
    expect_equal(test_hamming["A", "B"], test_hamming["B", "A"])
    expect_equal(test_hamming["A", "B"], 0.25)
    expect_equal(test_hamming["A", "C"], 0.25)
    expect_equal(test_hamming["A", "D"], 0.75)
    expect_equal(test_hamming["A", "E"], 0)
    expect_equal(test_hamming["B", "C"], 0)
    expect_equal(test_hamming["B", "D"], 0.75)
    expect_warning(test_hamming_untrans <- round(char.diff(matrix, method = "hamming", translate = FALSE), 2))
    expect_equal(test_hamming_untrans["A", "B"], test_hamming_untrans["B", "A"])
    expect_equal(test_hamming_untrans["A", "B"], round(1/5, 2))
    expect_equal(test_hamming_untrans["A", "C"], round(4/5, 2))
    expect_equal(test_hamming_untrans["A", "D"], round(3/5, 2))
    expect_equal(test_hamming_untrans["A", "E"], round(5/5, 2))
    expect_equal(test_hamming_untrans["B", "C"], round(4/5, 2))
    expect_equal(test_hamming_untrans["B", "D"], round(3/5, 2))


    ## Gower differences
    expect_warning(test_gower <- round(char.diff(matrix, method = "gower"), 2))
    expect_equal(test_gower["A", "B"], test_gower["B", "A"])
    expect_equal(test_gower["A", "B"], 0.25)
    expect_equal(test_gower["A", "C"], 0.25)
    expect_equal(test_gower["A", "D"], 1.75)
    expect_equal(test_gower["A", "E"], 0)
    expect_equal(test_gower["B", "C"], 0)
    expect_equal(test_gower["B", "D"], 1.5)

    matrix <- do.call(cbind, list(A = c(0, 1, 0, 1, 1), #1, 2, 1, 2, 2
                                  B = c(0, 1, 1, 1, 1), #1, 2, 2, 2, 2
                                  C = c(0, 2, 2, 2, 2), #1, 2, 2, 2, 2
                                  D = c(0, 1, 2, 3, 4), #1, 2, 3, 4, 5
                                  E = c(1, 2, 1, 2, 2)))#1, 2, 1, 2, 2

    expect_warning(test_gower_untrans <- round(char.diff(matrix, method = "gower", translate = FALSE), 2))
    expect_equal(test_gower_untrans["A", "B"], test_gower_untrans["B", "A"])
    expect_equal(test_gower_untrans["A", "B"], round(1/5, 2))
    expect_equal(test_gower_untrans["A", "C"], round(5/5, 2))
    expect_equal(test_gower_untrans["A", "D"], round(7/5, 2))
    expect_equal(test_gower_untrans["A", "E"], round(5/5, 2))
    expect_equal(test_gower_untrans["B", "C"], round(4/5, 2))
    expect_equal(test_gower_untrans["B", "D"], round(6/5, 2))


    ## Manhattan
    test_manhattan <- round(char.diff(matrix, method = "manhattan"), 2)
    expect_equal(test_manhattan["A", "B"], test_manhattan["B", "A"])
    expect_equal(test_manhattan["A", "B"], 1)
    expect_equal(test_manhattan["A", "C"], 5)
    expect_equal(test_manhattan["A", "D"], 7)
    expect_equal(test_manhattan["A", "E"], 5)
    expect_equal(test_manhattan["B", "C"], 4)
    expect_equal(test_manhattan["B", "D"], 6)

    ## Euclidean
    test_euclidean <- round(char.diff(matrix, method = "euclidean"), 2)
    expect_equal(test_euclidean["A", "B"], test_euclidean["B", "A"])
    expect_equal(test_euclidean["A", "B"], 1)
    expect_equal(test_euclidean["A", "C"], 2.65)
    expect_equal(test_euclidean["A", "D"], 4.12)
    expect_equal(test_euclidean["A", "E"], 2.24)
    expect_equal(test_euclidean["B", "C"], 2)
    expect_equal(test_euclidean["B", "D"], 3.74)
})
