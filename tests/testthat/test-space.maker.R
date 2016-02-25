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
    expect_error(
        space.maker(2, 10, rnorm)
        )
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
