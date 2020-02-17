context("char.diff")

test_that("translate.xyz works", {

    special.tokens <- c("\\/", "\\&", "\\?")

    expect_equal(translate.xyz(c("0"), special.tokens),
                 as.character(c(1)))
    expect_equal(translate.xyz(c("1024"), special.tokens),
                 as.character(c(1)))
    expect_equal(translate.xyz(c("0", "1"), special.tokens),
                 as.character(c(1, 2)))
    expect_equal(translate.xyz(c("77", "1/77", "?", "9"), special.tokens),
                 c("1", "2/1", "?", "3"))
})

test_that("convert.bitwise works", {
    ## Full characters
    expect_equal(convert.bitwise(0), 1)
    expect_equal(convert.bitwise("0"), 1)
    expect_equal(convert.bitwise(1), 2)
    expect_equal(convert.bitwise("1"), 2)
    expect_equal(convert.bitwise(2), 4)
    expect_equal(convert.bitwise("2"), 4)
    expect_equal(convert.bitwise(3), 8)
    expect_equal(convert.bitwise("3"), 8)
    expect_equal(convert.bitwise(4), 16)
    expect_equal(convert.bitwise("4"), 16)

    ## Special tokens
    special.tokens <- c(missing = "\\?", inapplicable = "\\-", polymorphism = "\\&", uncertainty = "\\/")
    ## Special behaviours
    special.behaviours <- list(
        missing = function(x,y) return(as.integer(y)),
        inapplicable = function(x,y) return(as.integer(-1)),
        polymorphism = function(x,y) return(as.integer(strsplit(x, split = "\\&")[[1]])),
        uncertainty = function(x,y) return(as.integer(strsplit(x, split = "\\/")[[1]]))
        )
    all_states <- c(0,1,2,3)
    expect_warning(expect_equal(convert.bitwise("0/1/2", special.tokens, special.behaviours, all_states), 7))
    expect_warning(expect_equal(convert.bitwise("0&1", special.tokens, special.behaviours, all_states), 3))
    expect_warning(expect_equal(convert.bitwise("-", special.tokens, special.behaviours, all_states), 0))
    expect_warning(expect_equal(convert.bitwise("?", special.tokens, special.behaviours, all_states), 15))

    ## Add some weird token
    special.tokens["weird"] <- "\\§"
    special.behaviours$weird <- function(x,y) return(as.integer(1000))
    expect_warning(expect_equal(convert.bitwise("1§2", special.tokens, special.behaviours, all_states), 2^1000))
})


test_that("convert.character works", {

    special.tokens <- c(missing = "\\?", inapplicable = "\\-", polymorphism = "\\&", uncertainty = "\\/")
    ## Special behaviours
    special.behaviours <- list(
        missing = function(x,y) return(as.integer(y)),
        inapplicable = function(x,y) return(as.integer(-1)),
        polymorphism = function(x,y) return(as.integer(strsplit(x, split = "\\&")[[1]])),
        uncertainty = function(x,y) return(as.integer(strsplit(x, split = "\\/")[[1]]))
        )

    simple_character <- c("0", "0", "1", "0", "0")
    expect_equal(
        convert.character(c("0", "0", "1", "0", "0"), special.tokens, special.behaviours)
        ,c("0" = 1, "0" = 1, "1" = 2, "0" = 1, "0" = 1))
    expect_equal(
        unname(convert.character(c("0", "0", "10", "0", "0"), special.tokens, special.behaviours))
        ,c(1, 1, 1024, 1, 1))
    expect_warning(expect_equal(
        convert.character(c("0", "0", "1", "?", "0"), special.tokens, special.behaviours)
        ,c("0" = 1, "0" = 1, "1" = 2, "?" = 3, "0" = 1)))
    expect_warning(expect_equal(
        convert.character(c("0", "0", "1", "?", "2"), special.tokens, special.behaviours)
        ,c("0" = 1, "0" = 1, "1" = 2, "?" = 7, "2" = 4)))
    expect_warning(expect_equal(
        convert.character(c("0/1", "0", "1", "?", "2"), special.tokens, special.behaviours)
        ,c("0/1" = 3, "0" = 1, "1" = 2, "?" = 7, "2" = 4)))
    expect_warning(expect_equal(
        convert.character(c("0/1", "-", "1", "?", "2"), special.tokens, special.behaviours)
        ,c("0/1" = 3, "-" = 0, "1" = 2, "?" = 7, "2" = 4)))
    expect_warning(expect_equal(
        convert.character(c("0/1", "-", "1&3", "?", "3"), special.tokens, special.behaviours)
        ,c("0/1" = 3, "-" = 0, "1&3" = 10, "?" = 11, "3" = 8)))
})


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
    expect_warning(tests[[1]] <- round(char.diff(matrix_simple, special.tokens = c("missing" = NA)), digits = 7))
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
    warn <- capture_warnings(test <- plot.char.diff(morpho_matrix))
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## Plotting the density profile of a char.diff object
    warn <- capture_warnings(char.diff_matrix <- char.diff(morpho_matrix))
    test <- plot(char.diff_matrix, type = "density")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## With NA
    morpho_matrix[, 1] <- NA
    warn <- capture_warnings(test <- plot.char.diff(morpho_matrix))
    expect_equal(warn[2], "NAs introduced by coercion")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
    warn <- capture_warnings(test <- plot.char.diff(morpho_matrix, type = "density"))
    expect_equal(warn[2], "NAs introduced by coercion")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
})


