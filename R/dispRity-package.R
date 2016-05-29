#' Measuring Disparity in R
#' 
#' A modular package for measuring disparity from ordinated matrices. Disparity can be calculated from any ordinated matrix. The package provides a set of implemented metrics to measure the ordinated space and allows users to provide and test their own metrics. The package also provides functions for looking at disparity in a serial way (e.g. time series) as well as visualising the results. Finally, this package provides several basic statistical test for disparity analysis.
#' 
#' @name dispRity-package
#'
#' @docType package
#'
#' @author Thomas Guillerme <guillert@@tcd.ie>
#'
#' @keywords disparity, ordination, phylogeny, cladistic, morphometric, ecology
#'
#' @import ape
#' @importFrom paleotree timeSliceTree
#' @importFrom snow makeCluster stopCluster parLapply
#' @importFrom geometry convhulln
#' @importFrom hypervolume hypervolume estimate_bandwidth get_volume
#' @importFrom ade4 randtest as.randtest
#' @importFrom grDevices colorRampPalette

NULL

#' Beck and Lee 2014 datasets
#'
#' Example datasets from Beck and Lee 2014.
#'
#' \itemize{
#'   \item \code{BeckLee_tree} A phylogenetic tree with 50 living and fossil taxa
#'   \item \code{BeckLee_mat50} The ordinated matrix based on the 50 taxa cladistic distances
#'   \item \code{BeckLee_mat99} The ordinated matrix based on the 50 taxa + 49 nodes cladistic distances
#'   \item \code{BeckLee_ages} A lit of first al last occurrence data for fossil taxa
#' }
#'
#' @format 3 matrices and one phylogenetic tree.
#' @source \url{http://rspb.royalsocietypublishing.org/content/281/1793/20141278.short}
#' @references Beck RMD & Lee MSY. 2014. Ancient dates or accelerated rates?
#' Morphological clocks and the antiquity of placental mammals.
#' Proc. R. Soc. B 2014 281 20141278; DOI: 10.1098/rspb.2014.1278
#' @name BeckLee
#' @aliases BeckLee_tree BeckLee_mat50 BeckLee_mat99 BeckLee_ages
NULL