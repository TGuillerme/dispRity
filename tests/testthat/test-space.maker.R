#TEST space.maker

context("space.maker")

# #Testing sample.distribution
# test_that("sample.distribution works", {
#     #errors
#     expect_warning(
#         expect_error(
#             sample.distribution("a", c(runif,1,2))
#             )
#         )
#     expect_error(
#         sample.distribution(1, "c(runif,1,2)")
#         )
#     expect_error(
#         sample.distribution(1, c(aov,1,2))
#         )

#     #Returns the right number of values
#     expect_equal(
#         length(sample.distribution(1, c(runif))), 1
#         )
#     expect_equal(
#         length(sample.distribution(1, c(runif, 1, 2))), 1
#         )
#     expect_equal(
#         length(sample.distribution(1000, c(runif, 1, 2))), 1000
#         )

#     #Returns values in the range
#     expect_equal(
#         length(sample.distribution(1, c(runif))), 1
#         )
#     expect_less_than(
#         max(sample.distribution(1000, c(runif, 1,2))), 2.0000000001
#         )
#     expect_more_than(
#         min(sample.distribution(1000, c(runif, 1,2))), 0.9999999999
#         )
# })

#Testing space.maker
test_that("space.maker works", {
    #errors
    expect_error(
        space.maker("a", 2, rnorm)
        )
    expect_error(
        space.maker(10, "a", rnorm)
        )
    expect_error(
        space.maker(10, 2, "a")
        )
    # expect_error(
    #     space.maker(2, 10, rnorm)
    #     )
    expect_error(
        space.maker(-2, 2, rnorm)
        )
    expect_error(
        space.maker(10, 2, c(rnorm, "a"))
        )
    #working
    expect_is(
        space.maker(5, 3, rnorm)
        , "matrix")    
    expect_equal(
        dim(space.maker(5, 3, rnorm))
        , c(5,3))    
    expect_equal(
        dim(space.maker(5, 3, rnorm, list(list(mean = 30, sd = 50))))
        , c(5,3)) 
    expect_equal(
        dim(space.maker(5, 3, c(rnorm, runif, rgamma), list(list(mean = 30, sd = 50), NULL, list(shape = 0.25))))
        , c(5,3)) 
})


#Testing space.maker
test_that("correlation works", {
    #Cor matrix
    set.seed(1)
    cor_pre <- matrix(cbind(1,0.8,0.2, 0.8,1,0.7, 0.2,0.7,1), nrow = 3)
    space_cor <- space.maker(1000, 3, rnorm, cor.matrix = cor_pre)
    cor_post <- cor(space_cor)

    #dim
    expect_equal(
        ncol(space_cor)
        , 3)
    expect_equal(
        nrow(space_cor)
        , 1000)
    expect_equal(
        round(cor_post, digit = 1)
        ,cor_pre)
})
