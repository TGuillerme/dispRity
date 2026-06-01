set.seed(123)
tree <- rtree(n = 10)
tree <- makeNodeLabel(tree)
tree <- set.root.time(tree)
mat <- matrix(rnorm(95), 19, 5)
rownames(mat) <- c(tree$tip.label, tree$node.label)
data <- make.dispRity(data = mat, tree = tree)
data <- chrono.subsets(data, method = "c", model = "equal.split", time = 4)


