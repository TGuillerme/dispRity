#TESTING morpho.utilities

context("morpho.utilities")

#get.contrast.matrix
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
    expect_error(apply.NA(matrix, c("1", "character")))
    expect_error(apply.NA(matrix, c("clade", "character")))
    expect_error(apply.NA(matrix, c("clade", "character"), "tree"))
    expect_error(apply.NA(matrix, c("clade", "character"), rtree(5)))

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
})
