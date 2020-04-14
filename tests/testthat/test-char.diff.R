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
    ## Special tokens
    special.tokens <- c(missing = "\\?", inapplicable = "\\-", polymorphism = "\\&", uncertainty = "\\/")
    ## Special behaviours
    special.behaviours <- list(
        missing = function(x,y) return(y),
        inapplicable = function(x,y) return(NA),
        polymorphism = function(x,y) return(strsplit(x, split = "\\&")[[1]]),
        uncertainty = function(x,y) return(strsplit(x, split = "\\/")[[1]])
        )
    expect_equal(convert.bitwise("0/1/2", special.tokens, special.behaviours), 7)
    expect_equal(convert.bitwise("0&1", special.tokens, special.behaviours), 3)
    expect_true(is.na(convert.bitwise("-", special.tokens, special.behaviours)))
    expect_equal(convert.bitwise("?", special.tokens, special.behaviours), 0)

    ## Add some weird token
    special.tokens["weird"] <- "\\%"
    special.behaviours$weird <- function(x,y) return(as.integer(1000))
    expect_equal(convert.bitwise("1%2", special.tokens, special.behaviours), 2^1000)


    ## More tests!

    special.tokens <- c(missing = "\\?", inapplicable = "\\-", polymorphism = "\\&", uncertainty = "\\/")
    ## Special behaviours
    special.behaviours <- list(
        missing = function(x,y) return(y),
        inapplicable = function(x,y) return(NA),
        polymorphism = function(x,y) return(strsplit(x, split = "\\&")[[1]]),
        uncertainty = function(x,y) return(strsplit(x, split = "\\/")[[1]])
        )

    simple_character <- c("0", "0", "1", "0", "0")
    expect_equal(
        convert.bitwise(c("0", "0", "1", "0", "0"), special.tokens, special.behaviours)
        ,c(1, 1,  2,  1,  1))
    expect_equal(
        convert.bitwise(c("0", "0", "10", "0", "0"), special.tokens, special.behaviours)
        ,c(1, 1, 1024, 1, 1))
    expect_equal(
        convert.bitwise(c("0", "0", "1", "?", "0"), special.tokens, special.behaviours)
        ,c(1, 1, 2, 3, 1))
    expect_equal(
        convert.bitwise(c("0", "0", "1", "?", "2"), special.tokens, special.behaviours)
        ,c(1, 1, 2, 7, 4))
    expect_equal(
        convert.bitwise(c("0/1", "0", "1", "?", "2"), special.tokens, special.behaviours)
        ,c(3, 1, 2, 7, 4))
    expect_equal(
        convert.bitwise(c("0/1", "-", "1", "?", "2"), special.tokens, special.behaviours)
        ,c(3, NA, 2, 7, 4))
    expect_equal(
        convert.bitwise(c("0/1", "-", "1&3", "?", "3"), special.tokens, special.behaviours)
        ,c(3, NA, 10, 11, 8))

})


test_that("char.diff pair", {
    A <- c(1,0,0,0,0)
    B <- c(0,1,1,1,1)

    ## Difference is 0
    expect_equal(char.diff(list(A,B)), 0)
    expect_equal(char.diff(list(A,B), translate = FALSE), 1)
    expect_equal(char.diff(list(A,B), order = TRUE), 0)
    expect_equal(char.diff(list(A,B), translate = FALSE, order = TRUE), 1)
    ## Difference is triangular
    expect_equal(char.diff(list(A,B)), char.diff(list(A,B)))

    C <- c(1,1,0,0,0)

    ## Difference is 0.25
    expect_equal(char.diff(list(A,C)), 0.25)
    expect_equal(char.diff(list(A,C), translate = FALSE), 0.2)
    expect_equal(char.diff(list(A,C), order = TRUE), 0.25)
    expect_equal(char.diff(list(A,C), translate = FALSE, order = TRUE), 0.2)

    ## Difference is triangular
    expect_equal(char.diff(list(A,C)), char.diff(list(C,A)))

    D <- c(0,1,1,0,0)

    ## Difference is 0.5
    expect_equal(char.diff(list(A,D)), 0.5)
    expect_equal(char.diff(list(A,D), translate = FALSE), 0.6)
    expect_equal(char.diff(list(A,D), order = TRUE), 0.5)
    expect_equal(char.diff(list(A,D), translate = FALSE, order = TRUE), 0.6)

    ## Difference is triangular
    expect_equal(char.diff(list(A,D)), char.diff(list(D,A)))

    E <- c(1,0,0,1,1)

    ## Difference is equal to D
    expect_equal(char.diff(list(D,E)), 0)
    expect_equal(char.diff(list(E,D), translate = FALSE), 1)
    expect_equal(char.diff(list(E,D), order = TRUE), 0)
    expect_equal(char.diff(list(E,D), translate = FALSE, order = TRUE), 1)
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
    tests[[1]] <- round(char.diff(matrix_simple), digits = 7)
    tests[[2]] <- round(char.diff(matrix_multi), digits = 7)
    tests[[3]] <- round(char.diff(matrix_binary), digits = 7)

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
    test <- char.diff(matrix_alpha)
    expect_is(test, c("matrix", "char.diff"))
})

test_that("char.diff NA, translate and order function works", {

    matrix_simple <- matrix(data = c(1,NA,3,NA,
                                     7,7,2,2,
                                     1,1,1,0), ncol = 3, byrow = FALSE)
    colnames(matrix_simple) <- LETTERS[1:3]

    ## Correct NA behaviour
    test_NA1 <- round(char.diff(matrix_simple), 5)
    test_NA2 <- round(char.diff(matrix_simple,
                          special.behaviours = list(missing = function(x,y) return(as.integer(y))),
                          special.tokens = c(missing = NA)), 5)

    expect_equal(as.vector(test_NA1), c(0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.33333, 1.00000, 0.33333, 0.00000))
    expect_equal(as.vector(test_NA2), c(0.00000, 0.00000, 0.33333, 0.00000, 0.00000, 0.33333, 0.33333, 0.33333, 0.00000))

    ## NA + translate
    test_tr1 <- round(char.diff(matrix_simple, translate = FALSE), 2)
    expect_equal(as.vector(test_tr1), c(0.0, 1.0, 0.5, 1.0, 0.0, 1.0, 0.5, 1.0, 0.0))
    test_tr2 <- char.diff(matrix_simple, translate = FALSE,
                          special.behaviours = list(missing = function(x,y) return(as.integer(y))),
                          special.tokens = c(missing = NA))
    expect_equal(as.vector(test_tr2), c(0.0, 1.0, 0.5, 1.0, 0.0, 1.0, 0.5, 1.0, 0.0))

    ## NA + translate + order
    test_ord1 <- round(char.diff(matrix_simple, translate = TRUE, order = TRUE), 5)
    expect_equal(as.vector(test_ord1), c(0.00000, 0.00000, 1.00000, 0.00000, 0.00000, 0.33333, 1.00000, 0.33333, 0.00000))
    test_ord2 <- round(char.diff(matrix_simple, translate = FALSE, order = TRUE), 5)
    expect_equal(as.vector(test_ord2), c(0.00, 3.50, 1.00, 3.50, 0.00, 3.75, 1.00, 3.75, 0.00))
    test_ord3 <- char.diff(matrix_simple, translate = FALSE, order = TRUE,
                          special.behaviours = list(missing = function(x,y) return(as.integer(y))),
                          special.tokens = c(missing = NA))
    expect_equal(as.vector(test_ord3), c(0.00, 3.00, 0.75, 3.00, 0.00, 3.75, 0.75, 3.75, 0.00))
})


test_that("order works as a logical vector", {

    matrix_multi <- matrix(data = c(1,2,0,0,1,2,1,
                                    2,3,1,2,2,0,2,
                                    0,4,2,1,1,2,2,
                                    0,4,0,0,0,1,0,
                                    0,4,0,0,0,1,0), ncol = 7, byrow = TRUE)
    colnames(matrix_multi) <- LETTERS[1:7]
    rownames(matrix_multi) <- letters[1:5]

    ## Handling dimnames correctly
    test1 <- char.diff(matrix_multi, by.col = TRUE)
    test2 <- char.diff(matrix_multi, by.col = FALSE)
    expect_equal(dim(test1), c(7, 7))
    expect_equal(dim(test2), c(5, 5))
    expect_equal(colnames(test1), LETTERS[1:7])
    expect_equal(colnames(test2), letters[1:5])

    ## Handling ordering errors
    error <- capture_error(char.diff(matrix_multi, by.col = TRUE, order = c(T, T, T, T, T, F, T, F)))
    expect_equal(error[[1]], "order must be of the same length as the number of rows in the matrix (5).")
    error <- capture_error(char.diff(matrix_multi, by.col = FALSE, order = c(T, T, T, T, T, F, T, F)))
    expect_equal(error[[1]], "order must be of the same length as the number of columns in the matrix (7).")

    ## Handling multi ordering
    # char.diff(matrix_multi, by.col = TRUE, order = c(T, T, T, T, F, F, F))


})


test_that("Test other distances", {
    ## Raw
    expect_equal(char.diff(list(c(1,1,1,1),c(1,1,1,1)), translate = FALSE, method = "manhattan"), 0)
    expect_equal(char.diff(list(c(0,1,0,1),c(1,0,1,0)), translate = FALSE, method = "manhattan"), 4)
    expect_equal(char.diff(list(c(0,1,0,1),c(1,0,1,0)), translate = TRUE, method = "manhattan"), 0)
    expect_equal(char.diff(list(c(NA,NA,NA,1),c(1,1,1,1)), method = "manhattan"), 0)
    ## Comparable
    expect_equal(char.diff(list(c(1,1,1,1),c(1,1,1,1)), method = "comparable"), 4)
    expect_equal(char.diff(list(c(NA,NA,NA,1),c(1,1,1,1)), method = "comparable"), 1)
    ## Euclidean
    expect_equal(char.diff(list(c(0,1,0,1),c(1,0,1,0)), translate = FALSE, method = "euclidean"), sqrt(1+1+1+1))
    expect_equal(char.diff(list(c(0,1,0,1),c(1,0,1,0)), translate = TRUE, method = "euclidean"), 0)
    ## Ordered
    expect_equal(char.diff(list(c(0,1,0,1),c(1,0,1,0)), translate = FALSE, method = "euclidean"), sqrt(1+1+1+1))
    expect_equal(char.diff(list(c(0,1,0,1),c(1,0,1,0)), translate = TRUE, method = "euclidean"), 0)
    ## Test maximum
    expect_equal(char.diff(list(c(0,1,1,1), c(0,1,1,1)), method = "maximum"), 0)
    expect_equal(char.diff(list(c(0,1,1,1), c(0,1,1,3)), method = "maximum"), 1)
    expect_equal(char.diff(list(c(1,1), c(1,3)), method = "maximum", by.col = FALSE, order = TRUE, translate = FALSE), 2)
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
    expect_error(plot.char.diff(morpho_matrix, type = "density", legend.pos = 1))
    expect_error(plot.char.diff(morpho_matrix, type = "density", main = c("main", "bob")))
    expect_error(plot.char.diff(morpho_matrix, type = "density", col = "blue"))
    expect_error(plot.char.diff(morpho_matrix, type = "matrix", col = "blue"))
    expect_error(plot.char.diff(morpho_matrix, type = "density", xlab = c("main", "bob")))
    expect_error(plot.char.diff(morpho_matrix, type = "density", ylab = c("main", "bob")))


    ## Plotting a matrix
    test <- plot.char.diff(morpho_matrix)
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## Plotting the density profile of a char.diff object
    char.diff_matrix <- char.diff(morpho_matrix)
    test <- plot(char.diff_matrix, type = "density")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")

    ## With NA
    morpho_matrix[, 1] <- NA
    test <- plot.char.diff(morpho_matrix)
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
    test <- plot.char.diff(morpho_matrix, type = "density")
    expect_equal(names(test), c("rect", "text"))
    expect_equal(unique(unlist(lapply(test, lapply, class))), "numeric")
})



test_that("char.diff give the same results as Claddis::MorphDistMatrix", {

    library(Claddis)

    ## The distance test wrappers
    claddis.test.wrapper <- function(matrix, transform = "none") {
        results <- list()
        results[[1]] <- Claddis::MorphDistMatrix(matrix, Distance = "GC", TransformDistances = transform)$ComparableCharacterMatrix
        results[[2]] <- Claddis::MorphDistMatrix(matrix, Distance = "GC", TransformDistances = transform)$DistanceMatrix
        results[[3]] <- Claddis::MorphDistMatrix(matrix, Distance = "MORD", TransformDistances = transform)$DistanceMatrix
        names(results) <- c("comparable", "gower", "mord")
        return(results)
    }
    dispRity.test.wrapper <- function(matrix, transform = "none") {
        results <- list()
        results[[1]] <- char.diff(matrix, method = "comparable", translate = FALSE, by.col = FALSE, special.behaviours = list("missing" = function(x,y) return(y)))
        class(results[[1]]) <- "matrix"
        results[[2]] <- char.diff(matrix, method = "hamming", translate = FALSE, by.col = FALSE, special.behaviours = list("missing" = function(x,y) return(y)))
        class(results[[2]]) <- "matrix"
        results[[3]] <- char.diff(matrix, method = "mord", translate = FALSE, by.col = FALSE, special.behaviours = list("missing" = function(x,y) return(y)))
        class(results[[3]]) <- "matrix"

        names(results) <- c("comparable", "gower", "mord")
        return(results)
    }

    ## Test wrapper
    run.test <- function(matrix, Claddis_data, verbose = FALSE) {

        if(missing(Claddis_data)) {
            if(length(grep("?", matrix)) > 0) {
                tmp_matrix <- ifelse(matrix == "?", NA, matrix)
            } else {
                tmp_matrix <- matrix
            }
            if(length(grep("-", tmp_matrix)) > 0) {
                tmp_matrix <- ifelse(tmp_matrix == "-", NA, tmp_matrix)
            } else {
                tmp_matrix <- tmp_matrix
            }
            Claddis_data <- Claddis::MakeMorphMatrix(CharacterTaxonMatrix = tmp_matrix)
        }

        Claddis_start <- Sys.time()
        Claddis_results <- claddis.test.wrapper(Claddis_data)
        Claddis_end <- Sys.time()
        dispRity_start <- Sys.time()
        dispRity_results <- dispRity.test.wrapper(matrix)
        dispRity_end <- Sys.time()

        expect_equal(Claddis_results$comparable, dispRity_results$comparable)
        expect_equal(Claddis_results$gower, dispRity_results$gower)
        expect_equal(Claddis_results$mord, dispRity_results$mord)

        if(verbose) {
            cat("time increase factor: ")
            cat((Claddis_end-Claddis_start)[[1]]/(dispRity_end-dispRity_start)[[1]])
            cat("\ndispRity run time: ")
            print(dispRity_end-dispRity_start)
            cat("\nCladdis run time: ")
            print(Claddis_end-Claddis_start)
            cat("\n")
        }
        return(list("Claddis" = Claddis_results, "dispRity" = dispRity_results))
    }

    results <- run.test(Claddis::Michaux1989$Matrix_1$Matrix)
    results <- run.test(Claddis::Gauthier1986$Matrix_1$Matrix, Claddis::Gauthier1986)

    # # Import complex matrix from MammalDisparity project
    # source("~/Projects/MammalDisparity/Functions/read.nexus.data.R") ## While waiting for ape 5.4
    # matrix <- do.call(rbind, read.nexus.data("~/Projects/MammalDisparity/Data/Morphology/227t_682c_morphology.nex"))
    # matrix_2 <- matrix[-1,]
    # rownames(matrix_2) <- paste0(rownames(matrix_2), "_1")
    # matrix <- rbind(matrix, matrix_2)
    # results <- run.test(matrix, verbose = TRUE)
})