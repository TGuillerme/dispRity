#TESTING bhatt.coeff

context("bhatt.coeff")

#Test
test_that("bhatt.coeff works", {
    #Errors
    expect_error(
        bhatt.coeff("a", rnorm(10))
        )
    expect_error(
        bhatt.coeff(rnorm(10), "b")
        )
    expect_error(
        bhatt.coeff(rnorm(10))
        )
    expect_error(
        bhatt.coeff(rnorm(10), rnorm(10), bw = "a")
        )
    expect_error(
        bhatt.coeff(rnorm(10), rnorm(10), bw = c(1,2))
        )

    #Works well
    set.seed(1)
    disA <- rnorm(100)
    disB <- rnorm(100)
    disC <- rnorm(100, mean = 20, sd = 0.01)
    expect_equal(
        bhatt.coeff(disA, disA)
        ,1)
    expect_equal(
        bhatt.coeff(disA, disC)
        ,0)
    expect_equal(
        round(bhatt.coeff(disA, disB), 3)
        ,0.958)
    expect_equal(
        round(bhatt.coeff(disA, disB, bw = bw.nrd), 3)
        ,0.968)
    expect_equal(
        round(bhatt.coeff(disA, disB, bw = bw.SJ, nb = 20, method = "ste"), 3)
        ,0.931)

    set.seed(1)
    expect_equal(
        round(bhatt.coeff(rnorm(1000), rnorm(1000), bw = 10.5), digit = 3)
        ,1)

})
