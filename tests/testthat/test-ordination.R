# TESTING ordination

context("ordinations")


# Loading Claddis

test_that("Claddis.ordination works", {
    
    library(Claddis)

    data <- Claddis::Michaux1989

    ## Sanitizing
    expect_error(Claddis.ordination(matrix(5), distance = "MORD", k = 2))
    expect_error(Claddis.ordination(data, distance = "bob", k = 2))
    expect_error(Claddis.ordination(data, distance = "MORD", k = 10))
    expect_error(Claddis.ordination(Claddis::Day2016)) #NA
    data_bug <- data
    data_bug$Matrix_1$Matrix <- NULL
    expect_error(Claddis.ordination(data_bug, distance = "MORD")) #NA

    test <- Claddis.ordination(data, add = FALSE, TransformDistances = "none")
    expect_equal(dim(test), c(4,3))
    expect_equal(rownames(test), c("Ancilla", "Turrancilla", "Ancillista", "Amalda"))
})
    
test_that("read.nexus.data works with polymorphisms", {

    ## Update read.nexus.data (temporary)
    # read.nexus.poly <- ape::read.nexus.data
    # body(read.nexus.poly)[[18]][[4]][[11]][[3]][[2]] <- body(read.nexus.poly)[[18]][[4]][[11]][[4]][[3]] <- substitute(tsp <- get.polymorphism(strsplit(Seq, NULL)[[1]]))

    ## Normal morphological matrix
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
        t1  012
        t2  ?01
    ;
    END;", file = "read.nexus.data_test.nex")
    ## Normal behaviour (no polymorphisms)
    test <- read.nexus.poly("read.nexus.data_test.nex")
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(test[1], list("t1" = c("0", "1", "2")))
    expect_equal(test[2], list("t2" = c("?", "0", "1")))

    ## Correct matrix with polymorphisms
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
        t1  {01}(012){2}
        t2  ?01
    ;
    END;", file = "read.nexus.data_test.nex")
    ## Correct behaviour with polymorphisms
    test <- read.nexus.poly("read.nexus.data_test.nex")
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(test[1], list("t1" = c("0/1", "0/1/2", "2")))
    expect_equal(test[2], list("t2" = c("?", "0", "1")))

    ## Correct matrix with polymorphisms (non numeric)
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT=DNA MISSING=? GAP=- ;
    MATRIX
        t1  {AC}(ACG){T}
        t2  AAA
    ;
    END;", file = "read.nexus.data_test.nex")
    ## Correct behaviour with polymorphisms
    test <- read.nexus.poly("read.nexus.data_test.nex")
    expect_is(test, "list")
    expect_equal(length(test), 2)
    expect_equal(test[1], list("t1" = c("a/c", "a/c/g", "t")))
    expect_equal(test[2], list("t2" = c("a", "a", "a")))

    ## Errors due to missing brackets polymorphisms
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
        t1  {01(012){2}
        t2  ?01
    ;
    END;", file = "read.nexus.data_test.nex")
    error_msg <- capture_error(read.nexus.poly("read.nexus.data_test.nex"))
    expect_equal(error_msg[[1]], "missing closing bracket for a polymorphism at position 1")

    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
        t1  0(012{2}
        t2  ?01
    ;
    END;", file = "read.nexus.data_test.nex")
    error_msg <- capture_error(read.nexus.poly("read.nexus.data_test.nex"))
    expect_equal(error_msg[[1]], "missing closing bracket for a polymorphism at position 2")

    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
        t1  01{2
        t2  ?01
    ;
    END;", file = "read.nexus.data_test.nex")
    error_msg <- capture_error(read.nexus.poly("read.nexus.data_test.nex"))
    expect_equal(error_msg[[1]], "missing closing bracket for a polymorphism at position 3")

    ## Normal error (extra weird character)
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=2 NCHAR=3;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
        t1  012}
        t2  ?01
    ;
    END;", file = "read.nexus.data_test.nex")
    error_msg <- capture_output(capture_error(read.nexus.poly("read.nexus.data_test.nex")))
    expect_equal(error_msg[[1]], "t1 has 4 characters")

    expect_true(file.remove("read.nexus.data_test.nex"))
})

test_that("Claddis.ordination works with new reader", {
    cat(
    "#NEXUS
    BEGIN DATA;
    DIMENSIONS  NTAX=5 NCHAR=5;
    FORMAT SYMBOLS= \" 0 1 2\" MISSING=? GAP=- ;
    MATRIX
         t1  {01}1010
         t2  02120
         t3  1210(01)
         t4  01111
         t5  00101
    ;
    END;", file = "morpho_matrix.nex")

    ## Ordinating the matrix (using a distance matrix)
    expect_warning(test <- Claddis.ordination("morpho_matrix.nex"))
    expect_is(test, "matrix")
    expect_equal(dim(test), c(5, 4))
    expect_equal(sum(test), -5.278371e-16)

    ## Only converting the nexus matrix into a Claddis format
    expect_warning(Claddis_data <- Claddis.ordination("morpho_matrix.nex", distance = NULL))
    expect_is(Claddis_data, "list")
    expect_equal(names(Claddis_data), c("Topper", "Matrix_1"))
    expect_is(Claddis_data$Matrix_1$Matrix, "matrix")
    expect_equal(Claddis_data$Matrix_1$Matrix, do.call(rbind, read.nexus.poly("morpho_matrix.nex")))

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

    ## Procrustes to ordination
    test <- geomorph.ordination(dummy_procrustes)
    expect_equal(dim(test), c(10,10))
    expect_equal(colnames(test), paste0("PC", 1:10))

    ## Procrustes without ordination
    test <- geomorph.ordination(dummy_procrustes, ordinate = FALSE)
    expect_equal(dim(test), c(10,10))
    
    ## geomorph.data.frame to ordination
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
