## Test
test_that("match.tip.edge works", {
    set.seed(3) 
    tree <- rtree(20)
    tip_values <- sample(c("blue", "red"), 20, replace = TRUE)
    edge_colors <- match.tip.edge(tip_values, tree)
    expect_equal(edge_colors, c(NA, "red", "red", "red", NA, NA, "red", "blue", "blue", NA, NA, NA, "red", "red", "red", NA, "red", "blue", "red", "red", "red", "red", "red", NA, NA, "blue", NA, "blue", "red", NA, NA, NA, "blue", "red", NA, "red", "blue", "blue"))
    edge_colors <- match.tip.edge(tip_values, tree, replace.na = "grey")
    expect_equal(edge_colors, c("grey", "red", "red", "red", "grey", "grey", "red", "blue", "blue", "grey", "grey", "grey", "red", "red", "red", "grey", "red", "blue", "red", "red", "red", "red", "red", "grey", "grey", "blue", "grey", "blue", "red", "grey", "grey", "grey", "blue", "red", "grey", "red", "blue", "blue"))
})