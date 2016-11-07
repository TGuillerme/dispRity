#TEST sim.morpho

context("sim.morpho")

#Testing sample.distribution
test_that("sample.distribution works", {
    #errors
    expect_warning(
        expect_error(
            sample.distribution("a", c(runif,1,2))
            )
        )
    expect_error(
        sample.distribution(1, "c(runif,1,2)")
        )
    expect_error(
        sample.distribution(1, c(aov,1,2))
        )

    #Returns the right number of values
    expect_equal(
        length(sample.distribution(1, c(runif))), 1
        )
    expect_equal(
        length(sample.distribution(1, c(runif, 1, 2))), 1
        )
    expect_equal(
        length(sample.distribution(1000, c(runif, 1, 2))), 1000
        )

    #Returns values in the range
    expect_equal(
        length(sample.distribution(1, c(runif))), 1
        )
    expect_lt(
        max(sample.distribution(1000, c(runif, 1,2))), 2.0000000001
        )
    expect_gt(
        min(sample.distribution(1000, c(runif, 1,2))), 0.9999999999
        )
})

#Testing proportional.distribution
test_that("proportional.distribution works", {
    #errors
    expect_warning(
        expect_error(
            proportional.distribution("a", runif)
            )
        )
    expect_error(
        proportional.distribution(4, "runif")
        )
    expect_error(
        proportional.distribution(4, runif, "a")
        )

    #sum(results) = 1
    expect_equal(
        sum(proportional.distribution(4, runif)), 1
        )
    expect_equal(
        sum(proportional.distribution(100, runif)), 1
        )
    expect_equal(
        sum(proportional.distribution(4, runif, 1000, 2000)), 1
        )
    expect_equal(
        sum(proportional.distribution(4, rnorm)), 1
        )
})

#Testing gen.seq.HKY.binary
test_that("gen.seq.HKY.binary works", {
    #errors
    expect_error(
        gen.seq.HKY.binary("a", c(runif, 2, 2), c(runif, 1, 1))
        )
    expect_error(
        gen.seq.HKY.binary(5, c(runif, 2, 2), c(runif, 1, 1))
        )
    expect_error(
        gen.seq.HKY.binary(rtree(5), runif, c(runif, 1, 1))
        )
    expect_error(
        gen.seq.HKY.binary(rtree(5), c(runif, 1, 1), runif)
        )

    #results is a vector of length 5 (characters)
    expect_equal(
        length(gen.seq.HKY.binary(rtree(5), c(runif, 2, 2), c(runif, 1, 1))), 5
        )
    expect_is(
        gen.seq.HKY.binary(rtree(5), c(runif, 2, 2), c(runif, 1, 1)), "character"
        )
    set.seed(1)
    expect_equal(
        unique(as.vector(gen.seq.HKY.binary(rtree(5), c(runif, 2, 2), c(runif, 1, 1)))), c("1", "0")
        )
})

#Testing k.sampler
test_that("k.sampler works", {
    #binary states (most of the cases)
    expect_equal(
        k.sampler("a"), 2
        )
    expect_equal(
        k.sampler(1), 2
        )
    expect_equal(
        k.sampler(0.5), 2
        )

    #multistates (up to 4 states)
    set.seed(1)
    expect_equal(
        sort(unique(replicate(100, k.sampler(c(0.34, 0.33, 0.33))))), c(2,3,4)
        )
    #Proportion respected
    set.seed(1)
    test <- replicate(10000, k.sampler(c(0.80, 0.15, 0.05)))
    expect_equal(
        sort(unique(test)), c(2,3,4)
        )
    expect_equal(
        length(which(test == 2))/10000, 0.7932
        )
    expect_equal(
        length(which(test == 3))/10000, 0.1535
        )
    expect_equal(
        length(which(test == 4))/10000, 0.0533
        )
})


#Testing rTraitDisc.mk
test_that("rTraitDisc.mk works", {
    #errors
    expect_error(
        rTraitDisc.mk("a", c(runif,1,1), c(runif,2,2), c(0.5, 0.5))
        )
    expect_error(
        rTraitDisc.mk(rtree(5), c(runif,1,1), rates = "a", c(0.5, 0.5))
        )
})

#Testing is.invariant
test_that("is.invariant works", {
    #errors
    expect_error(
        is.invariant(mean)
        )
    #true or false
    expect_true(
        is.invariant(rep("a", 5))
        )
    expect_false(
        is.invariant(c(rep("A", 5), "b"))
        )
})
