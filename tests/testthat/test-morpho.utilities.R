#TESTING cust.series

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
