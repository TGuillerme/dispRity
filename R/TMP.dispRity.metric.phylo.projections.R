# ## Projection of elements on an axis
# phylo.projections <- function(matrix, phy, type = c("root","anc"), ...) {

# #' \code{type} designates which vector to draw and can be any pair of the following options (the first element being the origin of the vector and the second where the vector points to):
# #' \itemize{
# #'     \item "root": the root of the tree (the first element in phy$node.label);
# #'     \item "anc": the element's most recent ancestor;
# #'     \item "tips": the centroid of the tips;
# #'     \item "nodes": the centroid of all the nodes;
# #'     \item "living": the centroid of the tips the furthest from the root;
# #'     \item "fossil": the centroid of all the tips that are not the furthest from the root;
# #'     \item or any numeric values or function that can be interpreted as \code{point1} and \code{point2} in \code{\link{projections}}.
# #' }
# #' 
#     ## Select the start of the vector
#     if(is(type[1]) == "character") {
#         from <- switch(type[1],
#                     "root"   = ,
#                     "anc"    = ,
#                     "tips"   = ,
#                     "nodes"  = ,
#                     "living" = ,
#                     "fossil" =)
#     } else {
#         from <- type[[1]]
#     }
#     ## Select the end of the vector
#     if(is(to[1]) == "character") {
#         to <- switch(type[1],
#                     "root" = )
#     } else {
#         to <- type[[1]]
#     }

# ## Applyt the function on the matrix (per row)
# return(apply(matrix, projections, point1 = from(matrix, phy),
#                                   to     = to(matrix, phy),
#                                   ...))


# select.root <- function(row, matrix, phy) {
#     return(matrix[phy$node.label[1], ])
# }
# select.anc <- function(row, matrix, phy) {
#     ## Get the row name
#     row_name <- rownames(matrix)[row]
#     if(row_name == phy$node.label[1]) {
#         ## If the row is the root, return itself
#         return(matrix[phy$node.label[1], ])
#     } else {
#         ## Return the ancestor
#         return(matrix[phy$node.label[phy$edge[1, which(c(phy$tip.label, phy$node.label) %in% row_name)] - Ntip(phy)], ])
#     }
# }


# stop("DEBUG")
# matrix <- matrix(rnorm(90), 9, 10)
# phy <- rtree(5)
# phy <- makeNodeLabel(phy, prefix = "n")
# rownames(matrix) <- c(phy$tip.label, phy$node.label)

# plot(phy)
# nodelabels(phy$node.label[1])



# }
