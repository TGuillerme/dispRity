## Developer function for making the disparity data
make.demo.data_BeckLeeXXX <- function() {

    file.remove("../data/BeckLee_tree.rda")
    file.remove("../data/BeckLee_ages.rda")
    file.remove("../data/BeckLee_mat50.rda")
    file.remove("../data/BeckLee_mat99.rda")

    library(dispRity)
    library(paleotree)
    library(geiger)
    source("../tests/testthat/make.data/multi.ace.R")
    source("../tests/testthat/make.data/convert.tokens.R")
    source("../tests/testthat/make.data/read.nexus.data.R")
    ## matrix
    matrix <- do.call(rbind, read.nexus.data("../tests/testthat/make.data/2014-Beck-ProcB-matrix-morpho.nex"))

    ## tree
    tree <- read.nexus("2014-Beck-ProcB-TEM.tre")
    tree$node.labels <- seq(1:Nnode(tree)) + Ntip(tree)
    tree_tmp <- extract.clade(tree, 133)
    tree_tmp <- drop.tip(tree_tmp, extract.clade(tree_tmp, 127)$tip.label)
    tree <- drop.tip(tree_tmp, c("Erinaceus", "Ptilocercus", "Orycteropus", "Microgale"))
    tree$root.age <- max(tree.age(tree)$age)

    ## Clean the data
    cleaned_data <- clean.data(matrix, tree)
    matrix <- cleaned_data$data
    tree <- cleaned_data$tree

    ## Get the FADLADs
    FADLAD <- read.csv("Beck2014_FADLAD.csv")
    FADLAD <- FADLAD[-which(is.na(match(rownames(FADLAD), tree$tip.label))),]


    ## Add the ancestral states estimates
    ancestral_states <- multi.ace(matrix, tree, models = "ER", verbose = TRUE)[[1]]
    rownames(ancestral_states) <- tree$node.labels

    ## Combine both
    matrix_tips <- matrix
    matrix_tips_nodes <- rbind(matrix, ancestral_states)

    ## Measuring the distance
    distance_matrix_tips_nodes <- char.diff(matrix_tips_nodes, by.col = FALSE)
    distance_matrix_tips <- char.diff(matrix_tips, by.col = FALSE)

    ## Ordination (just because)
    pco_tips <- cmdscale(distance_matrix_tips, k = nrow(distance_matrix_tips) - 2, add = TRUE)$points
    pco_tips_nodes <- cmdscale(distance_matrix_tips_nodes, k = nrow(distance_matrix_tips_nodes) - 2, add = TRUE)$points

    BeckLee_tree <- tree
    BeckLee_ages <- FADLAD
    BeckLee_mat50 <- pco_tips
    BeckLee_mat99 <- pco_tips_nodes

    ## save the data
    save(BeckLee_ages, file = "../data/BeckLee_ages.rda")
    save(BeckLee_tree, file = "../data/BeckLee_tree.rda")
    save(BeckLee_mat50, file = "../data/BeckLee_mat50.rda")
    save(BeckLee_mat99, file = "../data/BeckLee_mat99.rda")
}




# ## Developer function for making the disparity data
# make.demo.data_disparity <- function() {
#     file.remove("../data/disparity.rda")
#     set.seed(1)
#     ## Getting the data ready
#     data(BeckLee_tree) ; data(BeckLee_mat99) ; data(BeckLee_ages)

#     ## Creating the time subsets
#     time_subsets <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(90, 80, 70, 60, 50, 40, 30), FADLAD = BeckLee_ages)

#     ## Bootstrapping the subsets
#     bs_subsets <- boot.matrix(chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "acctran", time = c(90, 80, 70, 60, 50, 40, 30), FADLAD = BeckLee_ages), bootstraps = 100, rarefaction = c(20,15,10,5))

#     ## Calculating disparity
#     disparity <- dispRity(bs_subsets, metric = c(median, centroids))

#     ## save the data
#     save(disparity, file = "../data/disparity.rda")
# }

# ## Developer function for making the disparity data
# make.demo.data_BeckLee_disparity <- function() {
#     file.remove("../data/BeckLee_disparity.rda")
#     set.seed(1)
#     ## Getting the data ready
#     data(BeckLee_tree) ; data(BeckLee_mat99) ; data(BeckLee_ages)

#     ## Creating the time subsets
#     time_subsets <- chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "continuous", model = "proximity", time = seq(from = 0, to = 120, by = 1), FADLAD = BeckLee_ages)

#     ## Bootstrapping the subsets
#     bs_subsets <- boot.matrix(time_subsets, bootstraps = 100)

#     ## Calculating disparity
#     BeckLee_disparity <- dispRity(bs_subsets, metric = c(sum, variances))

#     ## save the data
#     save(BeckLee_disparity, file = "../data/BeckLee_disparity.rda")
# }