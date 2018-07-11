## TESTING ordination

context("ordinations")


#Loading Claddis
if(!require(Claddis)) devtools::install_github("TGuillerme/Claddis")

test_that("Claddis.support works", {

    ## Errors
    expect_error(Claddis.ordination(matrix(1)))
    expect_error(Claddis.ordination(list(matrix(1))))
    expect_error(Claddis.ordination(Michaux1989, distance = "blob"))

    expect_equal(MorphDistMatrix(Michaux1989), MorphDistMatrix.support(Michaux1989))
    expect_equal(MorphDistMatrix(Gauthier1986), MorphDistMatrix.support(Gauthier1986))

    #Testing with a more complex (bigger) matrix
    #Some random matrix
    set.seed(1)
    matrix <- matrix(sample(c("0", "1", "2", "3", "0&1", "0&2", "1&2", "0&1&2"), 5000, replace=TRUE, prob=c(0.425, 0.42, 0.6, 0.6, 0.01, 0.01, 0.01, 0.005))
    , nrow=50, ncol=100, dimnames=list(c(1:50)))
    #Adding 25% of missing characters
    matrix[sample(1:5000, 200)] <- NA

    #ordering
    ordering <- sample(c("unord", "ord"), 100, replace=TRUE, prob=c(0.9,0.1))

    #weights
    weights <- sample(c(1:3), 100, replace=TRUE, prob=c(0.85,0.1,0.05))

    #Function for making a morph.matrix like object
    make.nexus<-function(matrix, header, ordering, weights) {
        nexus<-list()
        nexus$header<-header
        nexus$matrix<-matrix
        nexus$ordering<-ordering
        nexus$weights<-weights
        nexus$max.vals<-as.numeric(apply(matrix, 2, max, na.rm=TRUE))
        nexus$min.vals<-as.numeric(apply(matrix, 2, min, na.rm=TRUE))
        return(nexus)
    }

    morph.matrix <- make.nexus(matrix, header="example", ordering, weights)

    expect_equal(MorphDistMatrix(morph.matrix), MorphDistMatrix.support(morph.matrix))


    ##MorphDistMatrix.support INIT

    ## Error
    expect_error(MorphDistMatrix.support(morph.matrix, transform.proportional.distances = "bob"))

    ## Transformation works
    test1 <- MorphDistMatrix.support(Michaux1989, distance = c("Gower", "Max"), transform.proportional.distances = "sqrt")
    test2 <- MorphDistMatrix.support(Michaux1989, distance = c("Gower", "Max"), transform.proportional.distances = "none")
    expect_is(test1, "list")
    expect_is(test2, "list")
    expect_equal(lapply(test1, class), list("gower.dist.matrix" = "matrix", "max.dist.matrix" = "matrix"))
    expect_equal(lapply(test2, class), list("gower.dist.matrix" = "matrix", "max.dist.matrix" = "matrix"))
    expect_equal(test1[[1]][,1], test1[[2]][,1])
    expect_equal(test2[[1]][,1], test2[[2]][,1])
    expect_false(all(test2[[1]][,1] == test1[[1]][,1]))



    ##
    matrix_all <- MorphDistMatrix.support(morph.matrix)
    expect_is(matrix_all, "list")
    expect_equal(names(matrix_all), c("raw.dist.matrix", "GED.dist.matrix", "gower.dist.matrix", "max.dist.matrix", "comp.char.matrix"))

    matrix_Raw <- MorphDistMatrix.support(morph.matrix, distance = "Raw")
    expect_is(matrix_Raw, "matrix")

    matrix_GED <- MorphDistMatrix.support(morph.matrix, distance = "GED")
    expect_is(matrix_GED, "matrix")

    matrix_Gower <- MorphDistMatrix.support(morph.matrix, distance = "Gower")
    expect_is(matrix_Gower, "matrix")

    matrix_Max <- MorphDistMatrix.support(morph.matrix, distance = "Max")
    expect_is(matrix_Max, "matrix")

    matrix_Comp <- MorphDistMatrix.support(morph.matrix, distance = "Comp")
    expect_is(matrix_Comp, "matrix")

})

# Testing if each individual matrix is corrects
test_that("Claddis.support works individually", {
    results_base <- MorphDistMatrix(Michaux1989)
    results_fast <- MorphDistMatrix.support(Michaux1989)
    results_raw <- MorphDistMatrix.support(Michaux1989, distance = "Raw")
    results_GED <- MorphDistMatrix.support(Michaux1989, distance = "GED")
    results_Gower <- MorphDistMatrix.support(Michaux1989, distance = "Gower")
    results_Max <- MorphDistMatrix.support(Michaux1989, distance = "Max")
    results_Comp <- MorphDistMatrix.support(Michaux1989, distance = "Comp")
    expect_equal(results_base$raw.dist.matrix, results_raw)
    expect_equal(results_base$GED.dist.matrix, results_GED)
    expect_equal(results_base$gower.dist.matrix, results_Gower)
    expect_equal(results_base$max.dist.matrix, results_Max)
    expect_equal(results_base$comp.char.matrix, results_Comp)
})


test_that("Claddis.ordination works", {
    
    data_matrix <- matrix(c("0", "0", "1", "1", "0", "0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "1", "0", "1", "0", "0", "0", "0", "1", "0", "1", "0", "0", "1", "0", "0", "0", "1", "0", "1", "0", "1", "0", "1", "0", "0", "0", "0", "1", "0"), byrow = FALSE, nrow = 4)
    rownames(data_matrix) <- c("Ancilla", "Turrancilla", "Ancillista", "Amalda")
    data <- list(header = "", matrix = data_matrix, ordering = rep("unord", 11), weights = rep(1, 11), max.vals = rep(1, 11), min.vals = rep(0, 11), step.matrices = NULL, symbols = c("0", "1"))

    ## Sanitizing
    expect_error(Claddis.ordination(matrix(5), distance = "Gower", transform = "none", k = 2))
    expect_error(Claddis.ordination(data, distance = "bob", transform = "none", k = 2))
    expect_error(Claddis.ordination(data, distance = "Gower", transform = 1, k = 2))
    expect_error(Claddis.ordination(data, distance = "Gower", transform = "none", k = 10))
    expect_error(Claddis.ordination("blob", distance = "Gower", transform = "none", k = 10))

    test <- Claddis.ordination(data, add = FALSE)
    expect_equal(dim(test), c(4,3))
    expect_equal(rownames(test), c("Ancilla", "Turrancilla", "Ancillista", "Amalda"))
    expect_equal(round(as.vector(test), 5) , round(c(7.252259e-17, -5.106645e-01, 5.106645e-01, -3.207162e-16, 4.154578e-01, -4.566150e-16, -8.153839e-16, -4.154578e-01, 2.534942e-01, -2.534942e-01, -2.534942e-01, 2.534942e-01), 5))
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

})
