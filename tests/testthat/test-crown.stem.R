## TEST tree.age

context("crown.stem")

## Data
data(BeckLee_tree)
tree1 <- BeckLee_tree
set.seed(1)
tree2 <- rtree(10)



## Test
test_that("crown.stem works", {
    ## Sanitizing
    expect_error(
        crown.stem("tree1", output.names = TRUE)
        )
    expect_error(
        crown.stem(tree1, output.names = "TRUE")
        )
    expect_error(
        crown.stem(tree2)
        )

    ## Testing
    output1 <- crown.stem(tree1)
    output2 <- crown.stem(tree1, output.names = FALSE)
    
    ## Outputs are the right formats
    expect_is(output1, "list")
    expect_is(output2, "multiPhylo")
    expect_equal(names(output1), c("crown", "stem"))
    expect_equal(names(output2), c("crown", "stem"))

    ## Trees are the right dimensions
    expect_equal(unlist(tips <- lapply(output2, Ntip)), c("crown" = 30, "stem" = 20))
    expect_equal(unlist(nodes <- lapply(output2, Nnode)), c("crown" = 29, "stem" = 19))

    ## Right number of taxa nodes output
    expect_equal(unlist(lapply(output1, length)), apply(cbind(tips, nodes), 1, function(x) sum(as.numeric(x))))
})
