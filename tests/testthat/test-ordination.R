# TESTING ordination

#context("ordinations")


# Loading Claddis

test_that("Claddis.ordination works", {
    
    ## Making michaux's dataset
    data <- list()
    data$topper <- list(NULL)
    names(data$topper) <- "step_matrices"
    data$topper$header <- "!File downloaded from graemetlloyd.com"
    data$matrix_1$block_name <- NA
    data$matrix_1$datatype <- "STANDARD"
    data$matrix_1$matrix <- matrix(as.character(c(rep(0,11),
                                                c(0,0,1,1,1,0,0,0,1,1,0),
                                                c(1,1,0,1,0,1,0,0,0,0,1),
                                                c(1,1,1,1,0,0,1,1,1,0,0))
                                                ), ncol = 11, byrow = TRUE)
    rownames(data$matrix_1$matrix) <- c("Ancilla", "Turrancilla", "Ancillista", "Amalda")
    data$matrix_1$ordering <- rep("unord", 11)
    data$matrix_1$character_weights <- rep(1, 11)
    data$matrix_1$minimum_values <- rep(0, 11)
    data$matrix_1$maximum_values <- rep(1, 11)
    data$matrix_1$characters$symbols <- c("0", "1")
    data$matrix_1$characters$missing <- c("?")
    data$matrix_1$characters$gap <- c("-")
    class(data) <- "cladisticMatrix"

    ## Sanitizing
    error <- capture_error(Claddis.ordination(matrix(5), distance = "mord", k = 2))
    expect_equal(error[[1]], "data does not contain a matrix.\nUse Claddis::read_nexus_matrix to generate the proper data format.")
    error <- capture_error(Claddis.ordination(data, distance = "bob", k = 2))
    expect_equal(error[[1]], "distance argument must be one of the following: gc, ged, red, mord.")
    error <- capture_error(Claddis.ordination(data, distance = "mord", k = 10))
    expect_equal(error[[1]], "k cannot be greater than the number of rows in data - 1 (data has 3 rows).")
    data_na <- data
    data_na$matrix_1$matrix[1,] <- NA
    error <- capture_error(Claddis.ordination(data_na))
    expect_equal(error[[1]], "The generate distance matrix using \"mord\" distance from data_na contains NA and cannot be ordinated.")
    data_bug <- data
    data_bug$matrix_1$matrix <- NULL
    error <- capture_error(Claddis.ordination(data_bug, distance = "mord"))
    expect_equal(error[[1]], "data does not contain a matrix.\nUse Claddis::read_nexus_matrix to generate the proper data format.")

    test <- Claddis.ordination(data, add = FALSE, distance_transformation = "none")
    expect_equal(dim(test), c(4,3))
    expect_equal(rownames(test), c("Ancilla", "Turrancilla", "Ancillista", "Amalda"))
})

test_that("Claddis.ordination works with new reader", {
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=5 NCHAR=5;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
         t1  11010
         t2  02120
         t3  12100
         t4  01111
         t5  00101
    ;
    END;", file = "morpho_matrix.nex")

    ## Ordinating the matrix (using a distance matrix)
    # test <- Claddis.ordination("morpho_matrix.nex")
    # expect_is(test, "matrix")
    # expect_equal(dim(test), c(5, 4))
    # expect_equal(sum(test), 3.538836e-15)

    ## Only converting the nexus matrix into a Claddis format
    Claddis_data <- Claddis.ordination("morpho_matrix.nex", distance = NULL)
    expect_is(Claddis_data, "cladisticMatrix")
    expect_equal(names(Claddis_data), c("topper", "matrix_1"))
    expect_is(Claddis_data$matrix_1$matrix, "matrix")

    expect_true(file.remove("morpho_matrix.nex"))
})

test_that("geomorph.ordination works", {

    # library(geomorph)

    ## Internal: make.groups.factors
    set.seed(1)
    one_factor_list <- as.factor(sample(LETTERS[1:2], 10, replace = TRUE))
    expect_equal(make.groups.factors(one_factor_list), list(A = c(1,3,4,6,7,8), B = c(2,5,9,10)))

    set.seed(1)
    array <- array(rnorm(100), c(5,2,10))
    dummy_procrustes <- list(coords = array)
    class(dummy_procrustes) <- "gpagen"
    dummy_geomorph_df <- list(coords = array, factor1 = as.factor(sample(LETTERS[1:2], 10, replace = TRUE)), factor2 = as.factor(c(rep(1, 5), rep(2, 5))))
    class(dummy_geomorph_df) <- "geomorph.data.frame"

    ## Sanitizing
    expect_error(geomorph.ordination(array))
    expect_error(geomorph.ordination(list(coords = array)))
    expect_warning(expect_error(geomorph.ordination(dummy_procrustes, center = "no")))
    dummy_procrustes2 <- dummy_procrustes
    dummy_procrustes2$coords <- NULL
    expect_error(geomorph.ordination(dummy_procrustes2))

    ## Procrustes to ordination
    test <- geomorph.ordination(dummy_procrustes)
    expect_equal(dim(test), c(10,10))
    expect_equal(colnames(test), paste0("PC", 1:10))

    ## Procrustes without ordination
    test <- geomorph.ordination(dummy_procrustes, ordinate = FALSE)
    expect_equal(dim(test), c(10,10))
    
    ## geomorph.data.frame to ordination
    test <- geomorph.ordination(dummy_geomorph_df)
    expect_is(test, "dispRity")
    expect_equal(length(test$subsets), 4)
    expect_equal(
        names(test$subsets)
        ,c("factor1.A", "factor1.B", "factor2.1", "factor2.2")
        )
    expect_equal(
        as.vector(unlist(lapply(test$subsets, lapply, length)))
        , c(4,6,5,5)
        )
    
    dummy_geomorph_df2 <- dummy_geomorph_df
    dimnames(dummy_geomorph_df2$coords)[[3]] <- letters[1:10]

    test <- geomorph.ordination(dummy_geomorph_df2)
    expect_equal(dimnames(test$matrix[[1]])[[1]], letters[1:10])
    expect_equal(dimnames(test$matrix[[1]])[[2]], paste0("PC", 1:10))


    ## Properly inherits the dimnames
    attr(array, "dimnames")[[3]] <- letters[1:10]
    dummy_procrustes <- list(coords = array)
    class(dummy_procrustes) <- "gpagen"
    dummy_geomorph_df <- list(coords = array, factor1 = as.factor(sample(LETTERS[1:2], 10, replace = TRUE)), factor2 = as.factor(c(rep(1, 5), rep(2, 5))))
    class(dummy_geomorph_df) <- "geomorph.data.frame"
    test <- geomorph.ordination(dummy_geomorph_df)
    expect_equal(rownames(test$matrix[[1]]), letters[1:10])

    ## No factor
    dummy_geomorph_df <- list(coords = array, factor1 = sample(LETTERS[1:2], 10, replace = TRUE), factor2 = c(rep(1, 5), rep(2, 5)))
    class(dummy_geomorph_df) <- "geomorph.data.frame"
    expect_warning(geomorph.ordination(dummy_geomorph_df), "Attempting to coerce variables in dummy_geomorph_df as factor")
})
