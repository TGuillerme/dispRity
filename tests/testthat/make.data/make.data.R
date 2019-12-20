library(Claddis)
library(mulTree)
library(paleotree)
source("MorphDistMatrix.verbose.R")
source("anc.state.R")
source("anc.state_fun.R")
source("anc.unc.R")
## matrix
Nexus_data <- ReadMorphNexus("2014-Beck-ProcB-matrix-morpho.nex")
Nexus_matrix <- Nexus_data$matrix
## tree
tree <- read.nexus("2014-Beck-ProcB-TEM.tre")

#######################################
## Cleaning the matrices and the trees
#######################################

## Removing some species
tree_tmp <- extract.clade(tree, 133)
tree_tmp <- drop.tip(tree_tmp, extract.clade(tree_tmp, 127)$tip.label)
tree_data <- drop.tip(tree_tmp, c("Erinaceus", "Ptilocercus", "Orycteropus", "Microgale"))

## Cleaning the tree and the table
tree_data <- clean.tree(tree_data, Nexus_matrix)
table <- clean.table(Nexus_matrix, tree_data)
Nexus_data$matrix <- table
tree_data <- lapply.root(tree_data)

## Removing some characters
Nexus_data$matrix <- table
to_remove <- which(apply(Nexus_data$matrix, 2, function(X) length(levels(as.factor(X)))) < 2)
to_remove <- c(to_remove, which(apply(Nexus_data$matrix, 2, function(X) length(which(is.na(X)))) > 25))
matrix_tmp <- Nexus_data$matrix[,-c(to_remove)]
matrix_tmp <- matrix_tmp[,-c(28,59,75,98,135,172,182,186,189,191,204,224,235)]
matrix_tmp <- matrix_tmp[,-c(57,239)]
Nexus_matrix <- make.nexus(matrix_tmp)
Nexus_matrix$min.vals <- as.numeric(Nexus_matrix$min.vals)
Nexus_matrix$max.vals <- as.numeric(Nexus_matrix$max.vals)

#######################################
## Estimating ancestral states
#######################################

anc_states <- anc.state(tree_data, Nexus_matrix, method='ML-claddis', verbose=TRUE)

#####################################
## Distance matrix
#####################################

## Distance matrix using tips only
matrix_tips <- Nexus_matrix
dist_tips <- MorphDistMatrix.verbose(matrix_tips, verbose=TRUE)

## Distance matrix using also nodes
matrix_nodes <- Nexus_matrix
matrix_nodes$matrix <- anc.unc(anc_states, 0.95, missing=NA)$state
dist_nodes <- MorphDistMatrix.verbose(matrix_nodes, verbose=TRUE)

#####################################
## Creating the two pco's (with and without nodes)
#####################################

ord_data_tips <- cmdscale(dist_tips$gower.dist.matrix, k=nrow(dist_tips$gower.dist.matrix) - 2, add=T)$points
ord_data_tipsNnodes <- cmdscale(dist_nodes$gower.dist.matrix, k=nrow(dist_nodes$gower.dist.matrix) - 2, add=T)$points

#####################################
## Loading the FADLAD data
#####################################

FADLAD_data <- read.csv("Beck2014_FADLAD.csv", row.names=1)

## removing taxa not present in the tree
FADLAD_data <- FADLAD_data[-which(is.na(match(rownames(FADLAD_data), tree_data$tip.label))),]

test_data <- list("tree_data"=tree_data, "ord_data_tips"=ord_data_tips, "ord_data_tips_nodes"=ord_data_tipsNnodes, "FADLAD_data"=FADLAD_data)

save(test_data, file="../test_data.Rda")


#####################################
## Ecology data
#####################################

## Loading the data
McClean_data <- read.csv("2015-McClean.csv")

## Generating the matrix
McClean_matrix <- with(McClean_data,tapply(McClean_data$Abundance,list(McClean_data$Tdepth,McClean_data$Genus),mean))
McClean_matrix <- as.matrix(cbind(McClean_matrix[,1:93]))

## Calculating the distance matrix
McClean_distance <- as.matrix(dist(McClean_matrix, method="euclidean"))

## Ordinating the distance matrix
McClean_ordination <- cmdscale(McClean_distance, eig = TRUE, k = nrow(McClean_distance)-1)$points

## Factors
treatment <- c("a","a","b","b","a","a","a","b","b","a","a","b","b","b","b","b","b","b","a","b","b","a","b","b","b","b","a","a","a","b","b","a","a","a","a","a","a","a","a","a")
depth <- c(1,2,1,2,1,1,2,1,2,1,2,1,2,1,2,1,2,2,2,1,2,1,1,2,1,2,1,1,2,1,2,1,2,1,2,1,1,1,1,1)

## Output list
McClean_data <- list("ordination"=McClean_ordination, "treatment"=treatment, "depth"=depth)



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
