#' Measuring Disparity in R
#' 
#' A modular package for measuring disparity (multidimensional space occupancy). Disparity can be calculated from any matrix defining a multidimensional space. The package provides a set of implemented metrics to measure properties of the space and allows users to provide and test their own metrics (Guillerme (2018) <doi:10.1111/2041-210X.13022>). The package also provides functions for looking at disparity in a serial way (e.g. disparity through time - Guillerme and Cooper (2018) <doi:10.1111/pala.12364>) or per groups as well as visualising the results. Finally, this package provides several statistical tests for disparity analysis.
#' 
#' @name dispRity-package
#'
#' @docType package
#'
#' @author Thomas Guillerme <guillert@@tcd.ie>
#'
#' @concept disparity ordination phylogeny cladistic morphometric ecology
#'
# @import ape
# @import stats
# @importFrom geometry convhulln 
# @importFrom ade4 randtest as.randtest 
# @importFrom grDevices colorRampPalette grey 
# @importFrom caper comparative.data 
# @importFrom graphics axis boxplot hist image lines mtext par plot points polygon text legend
# @importFrom utils combn data capture.output tail
# @importFrom phyclust gen.seq.HKY 
# @importFrom phangorn dist.hamming NJ RF.dist CI RI optim.parsimony parsimony
# @importFrom vegan adonis vegdist
# @importFrom geiger dtt ratematrix sim.char
# @importFrom parallel parLapply detectCores makeCluster clusterExport stopCluster
# @importFrom Claddis calculate_morphological_distances


NULL

#' Beck and Lee 2014 datasets
#'
#' Example datasets from Beck and Lee 2014.
#'
#' \itemize{
#'   \item \code{BeckLee_tree} A phylogenetic tree with 50 living and fossil taxa
#'   \item \code{BeckLee_mat50} The ordinated matrix based on the 50 taxa cladistic distances
#'   \item \code{BeckLee_mat99} The ordinated matrix based on the 50 taxa + 49 nodes cladistic distances
#'   \item \code{BeckLee_ages} A list of first and last occurrence data for fossil taxa
#'   \item \code{BeckLee_disparity} a \code{dispRity} object with estimated sum of variances in 120 time bins, boostrapped 100 times from the Beck and Lee data
#' }
#'
#' @format three matrices and one phylogenetic tree.
#' @source \url{https://www.royalsocietypublishing.org/doi/abs/10.1098/rspb.2014.1278}
#' @references Beck RMD & Lee MSY. 2014. Ancient dates or accelerated rates?
#' Morphological clocks and the antiquity of placental mammals.
#' Proc. R. Soc. B 2014 281 20141278; DOI: 10.1098/rspb.2014.1278
#' @name BeckLee
#' @aliases BeckLee_tree BeckLee_mat50 BeckLee_mat99 BeckLee_ages
#' @seealso BeckLee_disparity disparity
NULL

#' disparity
#'
#' An example of a \code{dispRity} object.
#'
#' This matrix is based on the \code{\link{BeckLee}} dataset and split into seven continuous subsets (\code{\link{chrono.subsets}}).
#' It was bootstrapped 100 times (\code{\link{boot.matrix}}) with four rarefaction levels.
#' Disparity was calculated as the \code{\link[stats]{median}} of the \code{\link{centroids}} (\code{\link{dispRity}}).
#'
#' @format one \code{dispRity} object.
#' @name disparity
#' @seealso BeckLee_disparity BeckLee
#' @examples
# set.seed(42)
#' \dontrun{
#' ## Loading the data
#' data(BeckLee_mat99)
#' data(BeckLee_tree)
#' data(BeckLee_ages)
#' 
#' ## Creating the 7 subsets
#' subsets <- chrono.subsets(BeckLee_mat99, BeckLee_tree,
#'                           time = seq(from = 30, to = 90, by = 10),
#'                           method = "continuous", model = "ACCTRAN",
#'                           FADLAD = BeckLee_ages)
#' 
#' ## Bootstrapping and rarefying
#' bootstraps <- boot.matrix(subsets, bootstraps = 100,
#'                           rarefaction = c(20, 15, 10, 5))
#' 
#' ## Calculating disparity
#' disparity <- dispRity(bootstraps, metric = c(median, centroids))
#' }
# save(disparity, file = "../Data/disparity.rda")
NULL


#' BeckLee_disparity
#'
#' An example of a \code{dispRity} object.
#'
#' This matrix is based on the \code{\link{BeckLee}} dataset and split into 120 continuous subsets (\code{\link{chrono.subsets}}).
#' It was bootstrapped 100 times (\code{\link{boot.matrix}}) with four rarefaction levels.
#' Disparity was calculated as the \code{\link[base]{sum}} of the \code{\link{variances}} (\code{\link{dispRity}}).
#'
#' @format one \code{dispRity} object.
#' @name BeckLee_disparity
#' @seealso BeckLee disparity
#' @examples
# set.seed(42)
#' \dontrun{
#' ## Loading the data
#' data(BeckLee_mat99)
#' data(BeckLee_tree)
#' data(BeckLee_ages)
#' 
#' ## Creating the 7 subsets
#' subsets <- chrono.subsets(BeckLee_mat99, BeckLee_tree,
#'                           time = seq(from = 0, to = 120, by = 1),
#'                           method = "continuous", model = "proximity",
#'                           FADLAD = BeckLee_ages)
#' 
#' ## Bootstrapping and rarefying
#' bootstraps <- boot.matrix(subsets, bootstraps = 100)
#' 
#' ## Calculating disparity
#' BeckLee_disparity <- dispRity(bootstraps, metric = c(sum, variances))
#' }
# save(BeckLee_disparity, file = "../Data/BeckLee_disparity.rda")
NULL


#' @title Demo datasets
#'
#' @description A set six trait spaces with different groups and different dimensions.
#'
#' @details
#' 
#' The content of these datasets and the pipeline to build them is described in details in Guillerme et al 2020.
#' 
#' \itemize{
#'   \item \code{beck} A palaeobiology study of mammals. The data is a 105 dimensions ordination (PCO) of the distances between 106 mammals based on discrete morphological characters.
#'   \item \code{wright} A palaeobiology study of crinoids. The data is a 41 dimensions ordination (PCO) of the distances between 42 crinoids based on discrete morphological characters.
#'   \item \code{marcy} A geometric morphometric study of gophers (rodents). The data is a 134 dimensions ordination (PCA) the Procrustes superimposition of landmarks from 454 gopher skulls.
#'   \item \code{hopkins} A geometric morphometric study of trilobites. The data is a 134 dimensions ordination (PCA) the Procrustes superimposition of landmarks from 46 trilobites cephala.
#'   \item \code{jones} An ecological landscape study. The data is a 47 dimensions ordination (PCO) of the Jaccard distances between 48 field sites based on species composition.
#'   \item \code{healy} A life history analysis of the pace of life in animals. The data is a 6 dimensions ordination (PCA) of 6 life history traits from 285 animal species.
#' }
#' 
#' @source \url{https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.6452}
#' @references Guillerme T, Puttick MN, Marcy AE, Weisbecker V. \bold{2020} Shifting spaces: Which disparity or dissimilarity measurement best summarize occupancy in multidimensional spaces?. Ecol Evol. 2020;00:1-16. (doi:10.1002/ece3.6452)
#' @references Beck, R. M., & Lee, M. S. (2014). Ancient dates or accelerated rates? Morphological clocks and the antiquity of placental mammals. Proceedings of the Royal Society B: Biological Sciences, 281(1793), 20141278.
#' @references Wright, D. F. (2017). Bayesian estimation of fossil phylogenies and the evolution of early to middle Paleozoic crinoids (Echinodermata). Journal of Paleontology, 91(4), 799-814.
#' @references Marcy, A. E., Hadly, E. A., Sherratt, E., Garland, K., & Weisbecker, V. (2016). Getting a head in hard soils: convergent skull evolution and divergent allometric patterns explain shape variation in a highly diverse genus of pocket gophers (Thomomys). BMC evolutionary biology, 16(1), 207.
#' @references Hopkins, M.J. and Pearson, J.K., 2016. Non-linear ontogenetic shape change in Cryptolithus tesselatus (Trilobita) using three-dimensional geometric morphometrics. Palaeontologia Electronica, 19(3), pp.1-54.
#' @references Jones, N. T., Germain, R. M., Grainger, T. N., Hall, A. M., Baldwin, L., & Gilbert, B. (2015). Dispersal mode mediates the effect of patch size and patch connectivity on metacommunity diversity. Journal of Ecology, 103(4), 935-944.
#' @references Healy, K., Ezard, T.H., Jones, O.R., Salguero-Gomez, R. and Buckley, Y.M., 2019. Animal life history is shaped by the pace of life and the distribution of age-specific mortality and reproduction. Nature ecology & evolution, p.1.
#' 
#' @name demo_data
#' 
#' @examples
#' data(demo_data)
#' 
#' ## Loading the Beck and Lee 2014 demo data
#' demo_data$beck
#' 
#' ## Loading the Wright 2017 demo data
#' demo_data$wright
#'
#' ## Loading the Marcy et al. 2015 demo data
#' demo_data$marcy
#' 
#' ## Loading the Hopkins and Pearson 2016 demo data
#' demo_data$hopkins
#' 
#' ## Loading the Jones et al. 2015 demo data
#' demo_data$jones
#' 
#' ## Loading the Healy et al. 2019 demo data
#' demo_data$healy
 
NULL



#' @title Charadriiformes
#' @name charadriiformes
#'
#' @description An example of a \code{\link[MCMCglmm]{MCMCglmm}} model.
#'
#' @details This dataset is based on a random subset of 359 Charadriiformes (gulls, plovers and sandpipers) from Cooney et al 2017 and trees from Jetz et al 2012.
#' It contains:
#' \itemize{
#'   \item \code{data} A \code{"data.frame"} .
#'   \item \code{tree} A consensus tree of 359 charadriiformes species (\code{"phylo"}).
#'   \item \code{posteriors} The posteriors from a \code{"MCMCglmm"} model (see example below).
#'   \item \code{tree_distribution} A random distribution of 10 trees of the 359 charadriiformes species (\code{"multiPhylo"}).
#' }
#'
#' @format one \code{data.frame}, one \code{phylo} and one \code{MCMCglmm}.
#'
#' @references Cooney CR, Bright JA, Capp EJ, Chira AM,Hughes EC, Moody CJ, Nouri LO, Varley ZK, Thomas GH. Mega-evolutionary dynamics of the adaptive radiation of birds. Nature. 2017 Feb;542(7641):344-7.
#' @references Jetz W, Thomas GH, Joy JB, Hartmann K, Mooers AO. The global diversity of birds in space and time. Nature. 2012 Nov;491(7424):444-8.
#' 
#' @examples
# set.seed(42)
#' \dontrun{
#' ## Reproducing the MCMCglmm model
#' require(MCMCglmm)
#' data(charadriiformes)
#' 
#' ## Setting up the model parameters:
#' ## 1 - The formula (the first three PC axes)
#' model_formula <- cbind(PC1, PC2, PC3) ~ trait:clade-1
#' ## 2 - The residual term
#' model_residuals <- ~us(trait):units
#' ## 3 - The random terms
#' ## (one per clade and one for the whole phylogeny)
#' model_randoms <- ~ us(at.level(clade,1):trait):animal
#'                  + us(at.level(clade,2):trait):animal
#'                  + us(at.level(clade,3):trait):animal
#'                  + us(trait):animal
#' 
#' ## Flat priors for the residuals and random terms
#' flat_priors <- list(
#'      ## The residuals priors
#'      R = list(
#'          R1 = list(V = diag(3), nu = 0.002)), 
#'      ## The random priors (the phylogenetic terms)
#'      G = list(
#'          G1 = list(V = diag(3), nu = 0.002),
#'          G2 = list(V = diag(3), nu = 0.002),
#'          G3 = list(V = diag(3), nu = 0.002),
#'          G4 = list(V = diag(3), nu = 0.002)))
#' 
#' ## Run the model for 110000 iterations
#' ## sampled every 100 with a burnin (discard)
#' ## of the first 10000 iterations)
#' model <- MCMCglmm(formula  = model_formula,
#'                   rcov     = model_residual,
#'                   random   = model_randoms,
#'                   family   = rep("gaussian", 3),
#'                   prior    = flat_priors,
#'                   nitt     = 110000,
#'                   burnin   = 10000,
#'                   thin     = 100,
#'                   pedigree = charadriiformes$tree,
#'                   data     = charadriiformes$data)
#' }
# charadriiformes <- list(data = charadriiformes$data, tree = charadriiformes$tree, model = model)
# save(charadriiformes, file = "../Data/charadriiformes.rda")
NULL