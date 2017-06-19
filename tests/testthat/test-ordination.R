## TESTING ordination

context("ordinations")

test_that("Claddis.ordination works", {
    
    require(Claddis)
    data <- Michaux1989    

    ## Sanitizing
    expect_error(Claddis.ordination(matrix(5), distance = "Gower", transform = "none", k = 2))
    expect_error(Claddis.ordination(data, distance = "bob", transform = "none", k = 2))
    expect_error(Claddis.ordination(data, distance = "Gower", transform = 1, k = 2))
    expect_error(Claddis.ordination(data, distance = "Gower", transform = "none", k = 10))

    test <- Claddis.ordination(data)
    expect_equal(dim(test), c(4,3))
    expect_equal(rownames(test), c("Ancilla", "Turrancilla", "Ancillista", "Amalda"))
    expect_equal(round(as.vector(test), 5) , round(c(7.252259e-17, -5.106645e-01, 5.106645e-01, -3.207162e-16, 4.154578e-01, -4.566150e-16, -8.153839e-16, -4.154578e-01, 2.534942e-01, -2.534942e-01, -2.534942e-01, 2.534942e-01), 5))
})
