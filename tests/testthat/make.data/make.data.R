library(dispRity)
library(paleotree)
library(geiger)
source("multi.ace.R")
source("convert.tokens.R")
source("read.nexus.data.R")
## matrix
matrix <- do.call(rbind, read.nexus.data("2014-Beck-ProcB-matrix-morpho.nex"))

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

## Save these data for tests
test_data <- list("tree_data" = tree, "pco_tips" = pco_tips, "pco_tips_nodes" = pco_tips_nodes, "FADLAD_data" = FADLAD)

save(test_data, file = "../test_data.Rda")


#####################################
## Model test data
#####################################

## Test data
set.seed(42)
data(BeckLee_mat99) ; data(BeckLee_ages) ; data(BeckLee_tree)
data_bootstrapped <- boot.matrix(chrono.subsets(BeckLee_mat99, BeckLee_tree, method = "continuous", rev(seq(from = 0, to = 120, by = 5)), model = "proximity"))
model_test_data <- dispRity(data_bootstrapped, c(sum, variances))

save(model_test_data, file="../model_test_data.Rda")



#####################################
## Paleotree data
#####################################
library(paleotree)
set.seed(42)
record <- paleotree::simFossilRecord(p = 0.1, q = 0.1, nruns = 1, nTotalTaxa = c(10,15), nExtant = c(10,15))
taxa <- paleotree::fossilRecord2fossilTaxa(record)
rangesCont <- paleotree::sampleRanges(taxa, r = 0.5)
cladogram <- paleotree::taxa2cladogram(taxa, plot = FALSE)
likFun <- paleotree::make_durationFreqCont(rangesCont)
srRes <- optim(paleotree::parInit(likFun), likFun, lower = paleotree::parLower(likFun), upper = paleotree::parUpper(likFun), method = "L-BFGS-B", control = list(maxit = 1000000))
sRate <- srRes[[1]][2]
divRate <- srRes[[1]][1]
tree <- paleotree::cal3TimePaleoPhy(cladogram, rangesCont, brRate = divRate, extRate = divRate, sampRate = sRate, ntrees = 2, plot = FALSE)
tree[[1]]$node.label <- tree[[2]]$node.label <- paste0("n", 1:Nnode(tree[[1]]))
## Scale the trees to have the same most recent root age
tree[[1]]$root.time <- tree[[2]]$root.time <- tree[[2]]$root.time
## Make the dummy data
set.seed(1)
data <- matrix(rnorm((Ntip(tree[[1]])+Nnode(tree[[1]]))*6), nrow = Ntip(tree[[1]])+Nnode(tree[[1]]), ncol = 6, dimnames = list(c(tree[[1]]$tip.label, tree[[1]]$node.label)))

paleotree_data <- list(tree = tree, data = data)

save(paleotree_data, file="../paleotree_test_data.Rda")




####################
## geiger data
####################

geiger_test_data <- get(data(geospiza))

save(geiger_test_data, file="../geiger_test_data.Rda")
