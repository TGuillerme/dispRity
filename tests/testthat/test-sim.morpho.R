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

    ## Internal HKY.seq.generator
    set.seed(1)
    tree <- rcoal(15)
    HKY_seq <- HKY.seq.generator(tree, substitution = c(runif, 2, 2), rate = c(rgamma, rate = 10, shape = 5))
    expect_is(HKY_seq, "seqgen")
    expect_equal(length(as.list(HKY_seq)), Ntip(tree) + 1)

    ## Character selector
    char_seq <- character.selector(HKY_seq)
    expect_is(char_seq, "character")
    expect_equal(length(char_seq), Ntip(tree))
    expect_equal(char_seq, c("T", "T", "T", "T", "T", "C", "C", "C", "C", "C", "C", "C", "T", "C", "C"))


    verbose = FALSE
    #errors
    expect_error(
        gen.seq.HKY.binary("a", c(runif, 2, 2), c(runif, 1, 1), verbose)
        )
    expect_error(
        gen.seq.HKY.binary(5, c(runif, 2, 2), c(runif, 1, 1), verbose)
        )
    expect_error(
        gen.seq.HKY.binary(rtree(5), runif, c(runif, 1, 1), verbose)
        )
    expect_error(
        gen.seq.HKY.binary(rtree(5), c(runif, 1, 1), runif, verbose)
        )

    #results is a vector of length 5 (characters)
    expect_equal(
        length(gen.seq.HKY.binary(rtree(5), c(runif, 2, 2), c(runif, 1, 1), verbose = verbose) ), 5
        )
    expect_is(
        gen.seq.HKY.binary(rtree(5), c(runif, 2, 2), c(runif, 1, 1), verbose = verbose), "character"
        )
    set.seed(1)
    expect_equal(
        unique(as.vector(gen.seq.HKY.binary(rtree(5), c(runif, 2, 2), c(runif, 1, 1), verbose = verbose))), c("1", "0")
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
        rTraitDisc.mk("a", c(runif,1,1), c(runif,2,2), c(0.5, 0.5), verbose)
        )
    expect_error(
        rTraitDisc.mk(rtree(5), c(runif,1,1), rates = "a", c(0.5, 0.5), verbose)
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

## Testing mixed model
test_that("MIXED.model works", {
    verbose = FALSE
    expect_error(
        MIXED.model("a", c(runif,1,1), c(runif,2,2), c(0.5, 0.5), verbose)
        )
    expect_error(
        MIXED.model(rtree(5), c(runif,1,1), rates = "a", c(0.5, 0.5), verbose)
        )

    set.seed(1)
    Mk <- MIXED.model(rtree(5), c(runif,1,1), c(runif,2,2), c(0, 1), verbose)
    Mk_or_HKY <- MIXED.model(rtree(5), c(runif,1,1), c(runif,2,2), c(1, 0), verbose)

    expect_is(Mk, "character")
    expect_is(Mk_or_HKY, "character")
    expect_equal(Mk, c("1", "2", "0", "0", "2"))
    expect_equal(Mk_or_HKY, c("0", "0", "0", "0", "0"))

})

## Testing the overall function
test_that("sim.morpho works", {
    set.seed(4)
    tree <- rcoal(15)
    my_rates = c(rgamma, rate = 10, shape = 5)
    my_substitutions = c(runif, 2, 2)

    ## Sanitising works
    expect_error(
        sim.morpho("tree", characters = 50, model = "HKY", rates = my_rates, substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = "50", model = "HKY", rates = my_rates, substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = 50, model = 3, rates = my_rates, substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = 50, model = sum, rates = my_rates, substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = 50, model = "HKY", rates = "my_rates", substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = 50, model = "HKY", rates = my_rates, substitution = "my_substitutions")
        )
    expect_error(
         sim.morpho(tree, states = c(0.6, 0.6), characters = 50, model = "HKY", rates = my_rates, substitution = my_substitutions)
         )
    expect_warning(
        sim.morpho(tree, states = c(0.6, 0.4), characters = 50, model = "HKY", rates = my_rates, substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = 50, model = "HKY", rates = c(rgamma, mean) , substitution = my_substitutions)
        )
    expect_error(
        sim.morpho(tree, characters = 50, model = "HKY", rates = my_rates , substitution = c(rgamma, mean))
        )

    ## Some examples
    matrixHKY <- sim.morpho(tree, characters = 50, model = "HKY", rates = my_rates, substitution = my_substitutions)
    matrixMk1 <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates) 
    matrixMk2 <- sim.morpho(tree, characters = 50, model = "ER", rates = my_rates, invariant = FALSE)
    matrixMixed <- sim.morpho(tree, characters = 50, model = "MIXED", rates = my_rates, substitution = my_substitutions,  invariant = FALSE, verbose = FALSE)

    expect_is(matrixHKY, "matrix")
    expect_is(matrixMk1, "matrix")
    expect_is(matrixMk2, "matrix")
    expect_is(matrixMixed, "matrix")
    expect_equal(dim(matrixHKY), c(15,50))
    expect_equal(dim(matrixMk1), c(15,50))
    expect_equal(dim(matrixMk2), c(15,50))
    expect_equal(dim(matrixMixed), c(15,50))
})
