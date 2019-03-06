## TESTING ordination

context("ordinations")


#Loading Claddis
# if(!require(Claddis)) #devtools::install_github("TGuillerme/Claddis")
# library(Claddis)

test_that("Claddis.ordination works", {
    
    data <- Claddis::Michaux1989

    ## Sanitizing
    expect_error(Claddis.ordination(matrix(5), distance = "MORD", k = 2))
    expect_error(Claddis.ordination(data, distance = "bob", k = 2))
    expect_error(Claddis.ordination(data, distance = "MORD", k = 10))
    expect_error(Claddis.ordination("blob", distance = "MORD", k = 2))
    expect_error(Claddis.ordination(Day2016)) #NA

    test <- Claddis.ordination(data, add = FALSE, TransformDistances = "none")
    expect_equal(dim(test), c(4,3))
    expect_equal(rownames(test), c("Ancilla", "Turrancilla", "Ancillista", "Amalda"))
})



test_that("geomorph.ordination works", {

    ## Internal: make.groups.factors
    set.seed(1)
    one_factor_list <- as.factor(sample(LETTERS[1:2], 10, replace = TRUE))
    expect_equal(make.groups.factors(one_factor_list), list(A = c(1,2,5,10), B = c(3,4,6,7,8,9)))

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
    expect_true(all(test == geomorph::two.d.array(dummy_procrustes$coords)))

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
        , c(6,4,5,5)
        )
    
    dummy_geomorph_df2 <- dummy_geomorph_df
    dimnames(dummy_geomorph_df2$coords)[[3]] <- letters[1:10]

    test <- geomorph.ordination(dummy_geomorph_df2)
    expect_equal(dimnames(test$matrix)[[1]], letters[1:10])
    expect_equal(dimnames(test$matrix)[[2]], paste0("PC", 1:10))


    ## Properly inherits the dimnames
    attr(array, "dimnames")[[3]] <- letters[1:10]
    dummy_procrustes <- list(coords = array)
    class(dummy_procrustes) <- "gpagen"
    dummy_geomorph_df <- list(coords = array, factor1 = as.factor(sample(LETTERS[1:2], 10, replace = TRUE)), factor2 = as.factor(c(rep(1, 5), rep(2, 5))))
    class(dummy_geomorph_df) <- "geomorph.data.frame"
    test <- geomorph.ordination(dummy_geomorph_df)
    expect_equal(rownames(test$matrix), letters[1:10])

    ## No factor
    dummy_geomorph_df <- list(coords = array, factor1 = sample(LETTERS[1:2], 10, replace = TRUE), factor2 = c(rep(1, 5), rep(2, 5)))
    class(dummy_geomorph_df) <- "geomorph.data.frame"
    expect_warning(geomorph.ordination(dummy_geomorph_df), "Attempting to coerce variables in dummy_geomorph_df as factor")

})
