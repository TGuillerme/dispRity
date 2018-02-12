#TESTING morpho.utilities

context("morpho.utilities")


## Internals
test_that("state.selector", {
    set.seed(1)
    character <- sample((1:10), 10, replace = TRUE)
    expect_equal(state.selector(character), c(1, 3, 4, 6, 7, 9, 10))
})


test_that("inap.character", {
    set.seed(1)
    target <- as.character(sample(c(0,1), 10, replace = TRUE))
    pattern <- as.character(sample(c(0,1), 10, replace = TRUE))
    expect_equal(inap.character(target, pattern), c("-", "-", "1", "-", "0", "-", "1", "1", "-", "0"))
})

test_that("mapply.inap.character", {
    set.seed(1)
    matrix <- matrix(as.character(sample(c(0,1), 20, replace = TRUE)), ncol = 2)
    inap_char <- mapply.inap.character(1,2, matrix, invariant = TRUE)
    expect_is(inap_char, "character")
    expect_equal(length(inap_char), nrow(matrix))
    expect_equal(inap_char, inap.character(matrix[,1], matrix[,2]))
})

test_that("select.clade", {
    set.seed(1)
    test_tree <- rtree(20, br = NULL)
    clade <- select.clade(test_tree)
    expect_is(clade, "character")
    expect_equal(length(clade), 8)
    expect_equal(clade, c("t15", "t3", "t6", "t2", "t16", "t12", "t1", "t18"))
})

test_that("inap.clade", {
    set.seed(1)
    test_tree <- rtree(20, br = NULL)
    matrix <- matrix(as.character(sample(c(0,1), 40, replace = TRUE)), ncol = 2)
    rownames(matrix) <- test_tree$tip.label
    target_character <- matrix[,1]
    set.seed(1)
    grade <- inap.clade(target_character, test_tree)
    clade <- inap.clade(target_character, test_tree)
    expect_is(grade, "character")
    expect_is(clade, "character")
    expect_equal(length(grade), length(clade))
    expect_equal(length(grade), Ntip(test_tree))
    expect_equal(as.vector(grade), c("-", "-", "-", "-", "-", "-", "0", "1", "0", "1", "1", "1", "1", "1", "1", "0", "0", "1", "-", "-"))
    expect_equal(as.vector(clade), c("1", "0", "0", "1", "1", "1", "0", "1", "0", "1", "1", "1", "1", "1", "1", "-", "-", "1", "1", "0"))
})

test_that("lapply.inap.clade", {
    set.seed(1)
    test_tree <- rtree(20, br = NULL)
    matrix <- matrix(as.character(sample(c(0,1), 40, replace = TRUE)), ncol = 2)
    expect_is(lapply.inap.clade(2, matrix, test_tree, invariant = TRUE), "character")
})



# get.contrast.matrix
test_that("get.contrast.matrix", {
    #Errors
    expect_error(
        get.contrast.matrix(mean)
        )
    # A simple 2 by 2 matrix (0 1)
    expect_equal(
        dim(get.contrast.matrix(matrix(data = c(0,1,0,1), ncol = 2 ))), c(2,2)
        )
    expect_true(
        all(get.contrast.matrix(matrix(data = c(0,1,0,1), ncol = 2 )) == matrix(data = c(1,0,0,1), ncol = 2 ))
        )

    # A 2 by 2 with ?
    expect_equal(
        dim(get.contrast.matrix(matrix(data = c("A","B","A","?"), ncol = 2 ))), c(3,2)
        )
    expect_true(
        all(get.contrast.matrix(matrix(data = c("A","B","A","?"), ncol = 2 )) == matrix(data = c(1,0,0,1,1,1), ncol = 2 , byrow=TRUE))
        )

    # A "complex" one with inapplicables
    expect_equal(
        dim(get.contrast.matrix(matrix(data = c("A","0","-","?", "!", "A"), ncol =3 ))), c(5,4)
        )
    expect_true(
        all(get.contrast.matrix(matrix(data = c("A","0","-","?", "!", "A"), ncol =3 )) == matrix(data = c(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1, 1,1,1,1), ncol = 4 , byrow=TRUE))
        )
})


# apply.NA
test_that("apply.NA", {

    ## Matrix
    set.seed(4)
    tree <- rcoal(15)
    matrix <- sim.morpho(tree, characters = 100, model = "ER",states = c(0.85, 0.15), rates = c(rgamma, rate = 10, shape = 5), invariant = FALSE)
    tests <- list()

    ## Errors
    expect_error(apply.NA("matrix", 4))
    expect_error(apply.NA(matrix, "4"))
    expect_error(apply.NA(matrix, 51))
    expect_error(apply.NA(matrix, c("1", "character")))
    expect_error(apply.NA(matrix, c("clade", "character")))
    expect_error(apply.NA(matrix, c("clade", "character"), "tree"))
    expect_error(apply.NA(matrix, c("clade", "character"), rtree(5)))


    apply.NA(matrix, 49)

    ## Apply NAs
    tests[[1]] <- apply.NA(matrix, 10)
    tests[[2]] <- apply.NA(matrix, c(rep("character", 10)))
    tests[[3]] <- apply.NA(matrix, c(rep("clade", 10)), tree)
    tests[[4]] <- apply.NA(matrix, c(rep("clade", 5), rep("character", 5)), tree)

    ## tests working
    for(test in 1:length(tests)) {
        expect_is(tests[[test]], "matrix")
        expect_equal(
            sort(unique(as.vector(tests[[test]])))
            , c("-", "0", "1", "2"))
        expect_equal(
            length(which(apply(tests[[test]], 2, function(x) "-" %in% x)) == TRUE)
            , 10)
    }


    matrix_small <- sim.morpho(rtree(5), characters = 100, model = "ER",states = c(0.85, 0.15), rates = c(rgamma, rate = 10, shape = 5), invariant = TRUE)

    set.seed(1)
    warn <- capture_warnings(test <- apply.NA(matrix_small, 49, invariant = FALSE))
    expect_equal(warn, "21 characters are now invariant due inapplicable data.")

})