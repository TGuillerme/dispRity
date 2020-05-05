context("multi.ace")

## Test
test_that("multi.ace works", {


    ## Loading the data
    source("read.nexus.data.R") ## While waiting for ape 5.4
    morpho_matrix <- read.nexus.data("../Data/Morphology/227t_682c_morphology.nex")
    short_matrix <- lapply(morpho_matrix, function(x, characters) return(x[characters]),
                           characters =  c(1, 2, 3, 4, 7, 288, 289, 290, 291))

    load("../Data/Processed/tree_list.Rda")
    tree <- tree_list
    tree <- tree[[1]]

    



    ## Sanitizing
    expect_error(multi.ace())

    ## Right output
    expect_is(
        multi.ace()
        , "class")
    expect_equal(
        dim(multi.ace())
        , dim)
})
