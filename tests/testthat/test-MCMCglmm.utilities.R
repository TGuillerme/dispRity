## Test
load("covar_model_list.rda")
model_list <- covar_model_list

test_that("MCMCglmm.traits works", {
    ## Test data example
    for(i in 1:7) {
        expect_equal(MCMCglmm.traits(model_list[[i]]), c("PC1", "PC2", "PC3"))
    }
})

test_that("MCMCglmm.levels works", {

    ## Test data example
    expect_equal(MCMCglmm.levels(model_list[[1]]), c("residual" = "units"))
    expect_equal(MCMCglmm.levels(model_list[[2]]), c("residual" = "units:clade_1", "residual" = "units:clade_2", "residual" = "units:clade_3"))
    expect_equal(MCMCglmm.levels(model_list[[3]]), c("random" = "animal", "residual" = "units"))
    expect_equal(MCMCglmm.levels(model_list[[3]], convert = FALSE), c("random" = "us(trait):animal", "residual" = "us(trait):units"))
    expect_equal(MCMCglmm.levels(model_list[[4]]), c("random" = "animal", "residual" = "units:clade_1", "residual" = "units:clade_2", "residual" = "units:clade_3"))
    expect_equal(MCMCglmm.levels(model_list[[5]]), c("random" = "animal:clade_1", "random" = "animal:clade_2", "random" = "animal:clade_3", "residual" = "units:clade_1", "residual" = "units:clade_2", "residual" = "units:clade_3"))
    expect_equal(MCMCglmm.levels(model_list[[6]]), c("random" = "animal:clade_1", "random" = "animal:clade_2", "random" = "animal:clade_3", "random" = "animal", "residual" = "units:clade_1", "residual" = "units:clade_2", "residual" = "units:clade_3"))
    expect_equal(MCMCglmm.levels(model_list[[7]]), c("random" = "animal:clade_1", "random" = "animal:clade_2", "random" = "animal:clade_3", "random" = "animal", "residual" = "units"))
    expect_equal(MCMCglmm.levels(model_list[[7]], convert = FALSE), c("random" = "us(at.level(clade, 1):trait):animal", "random" = "us(at.level(clade, 2):trait):animal", "random" = "us(at.level(clade, 3):trait):animal", "random" = "us(trait):animal", "residual" = "us(trait):units"))
})

test_that("MCMCglmm.sample works", {
    test1 <- MCMCglmm.sample(covar_model_list[[1]])
    expect_equal(test1, seq(1:1000))
    set.seed(1)
    test2 <- MCMCglmm.sample(covar_model_list[[1]], n = 10)
    expect_equal(test2, c(836, 679, 129, 930, 509, 471, 299, 270, 978, 187))
    warn <- capture_warnings(test3 <- MCMCglmm.sample(covar_model_list[[1]], n = 1001))
    expect_equal(length(test3), 1001)
    expect_equal(warn[[1]], "The required number of samples 1001 is larger than the available number of samples 1000. Some samples will be used more than once.")
})

test_that("MCMCglmm.covars works", {
    ## Checking the correct structure in the nested lists
    correct.structure <- function(X) {expect_equal(length(X), 2); expect_is(X$VCV, "matrix"); expect_is(X$loc, "numeric")}

    ## Everything works!
    for(i in 1:7) {
        test <- get.sample.covar(421, model_list[[i]],
              MCMCglmm.levels(model_list[[i]]),
              MCMCglmm.traits(model_list[[i]]))
        expect_is(test, "list")
        expect_equal(names(test), unname(MCMCglmm.levels(model_list[[i]])))
        silent <- lapply(test, correct.structure)
    }

    ## default (all samples) works
    test <- MCMCglmm.covars(model_list[[1]])
    expect_equal(length(test), length(MCMCglmm.levels(model_list[[1]])))
    expect_equal(length(test[[1]]), 1000)
    ## n argument works
    test <- MCMCglmm.covars(model_list[[1]], n = 7)
    expect_equal(length(test), length(MCMCglmm.levels(model_list[[1]])))
    expect_equal(length(test[[1]]), 7)
    ## sample argument works
    test <- MCMCglmm.covars(model_list[[1]], sample = c(42, 5))
    expect_equal(length(test), length(MCMCglmm.levels(model_list[[1]])))
    expect_equal(length(test[[1]]), 2)
    ## Only takes the n argument
    warn <- capture_warnings(test <- MCMCglmm.covars(model_list[[7]], sample = 42, n = 7))
    expect_equal(warn[[1]], "sample argument is ignored since n = 7 random samples are asked for.")
    expect_equal(length(test), length(MCMCglmm.levels(model_list[[7]])))
    expect_equal(length(test[[1]]), 7)
})