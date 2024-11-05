        

#         ## Disparity is calculated for all nodes as the disparity of the descendants
#         ## But also as the average disparity from all descendants if there is more data on other branches
#         ## e.g. for the plot(tree), node 17 is average of node 18 and 23, 18 is the average of 19 and 23, etc.
#         ## This is some kind of acctran chrono.subsets with no node values? - probably not
# library(geiger); library(dispRity)
# geiger_data <- get(data(geospiza))
# data = geiger_data$dat
# average.sq <- function(X) mean(pairwise.dist(X)^2)
# metric = average.sq
# tree = geiger_data$phy
# cleaned_data <- clean.data(data, tree)
# data <- cleaned_data$data
# tree <- cleaned_data$tree
# tree$node.label <- paste0(1:Nnode(tree)+Ntip(tree))



#         ## Basic disparity
#         disparity_per_node <- unlist(get.disparity(dispRity(custom.subsets(data, group = tree), metric = average.sq)))

#         plot(tree, show.tip.label = FALSE)
#         nodelabels()
#         axisPhylo()
#         # dev.new()


#         # plot(NULL, xlim = c(0.6,0), ylim = c(0,1))

#         node_ages <- tree.age(tree)[-c(1:Ntip(tree)), 1]
#         names(node_ages) <- tree.age(tree)[-c(1:Ntip(tree)), 2]
#         # node_ages <- (1-rev(nodes_ages))

#         abline(v = max(node_ages) - node_ages, lwd = 0.5, col = "grey")
#         ordered_nodes <- sort(node_ages, decreasing = TRUE)
#         plot_nodes <- (max(node_ages) - node_ages)[names(ordered_nodes)]
#         axis(2)

#         ## Basic sub-clade diversity
#         disparity <- unlist(get.disparity(dispRity(custom.subsets(data, tree), metric = metric)))
#         relative_disparity <- disparity/max(disparity)
#         relative_disparity <- relative_disparity * 12
#         points(y = relative_disparity[names(ordered_nodes)], x = plot_nodes, col = "blue")



#     phy <- tree
#     phy$node.label <- NULL
#     td <- list("phy" = phy, "data" = data) #TG: data is already cleaned prior to the function
#     phy2 <- td$phy
#     ## Converting to old phylo format
#     phy <- new2old.phylo(td$phy)

#     ## Getting the node depth from ape if no attributes
#     if(is.null(phy2$root.time)){
#         node.depth <- branching.times(phy2)
#     } else {
#         node.depth <- tree.age(phy2)$ages[-c(1:Ntip(phy2))]
#         names(node.depth) <- 1:length(node.depth) + Ntip(phy2)
#     }

#     stem.depth <- numeric()
#     stem.depth[1] <- node.depth[1]
#     for(i in 2:phy2$Nnode) {
#         anc <- which(as.numeric(phy$edge[,2]) == -i)
#         stem.depth[i] <- node.depth[names(node.depth) == phy2$edge[anc,1]]
#     }

#     ## Lineages through time
#     ltt <- sort(node.depth, decreasing = TRUE)

#     ## Scaling by lineage through time
#     # node.depth <- node.depth/max(ltt)
#     # stem.depth <- stem.depth/max(ltt)
#     # ltt <- ltt/max(ltt)

#     result <- numeric()

#     ## By matrix
#     if(length(dim(td$data)) == 2) {

#         ## Calculate disparity per clade
#         disparity <- as.vector(summary(dispRity(custom.subsets(td$data, phy2), metric = metric), digits = 10)$obs)
#         names(disparity) <- 1:length(node.depth) + Ntip(phy2)
        
#         ## Disparity at the root
#         result[1] <- disparity[1]

#         ## Disparity for the other nodes (average per time slice)
#         for(i in 2:length(ltt)) {
#             x <- disparity[stem.depth >= ltt[i-1] & node.depth < ltt[i-1]]
#             ## Checks: which stem.depth greater= than previous lineage through time
#             ##         and   node.depth smaller  than previous lineage through time

#             #result[i] <- ifelse(length(x) == 0, 0, mean(x))                             #TODO: check 0 for non-fossil tree
#             if(length(x) == 0) {
#                 result[i] <- 0
#             } else {
#                 result[i] <- mean(x)
#             }
#         }

#         result[length(ltt)+1] <- 0                                                      #TODO: check 0 for non-fossil tree
#         names(result) <- names(disparity)

#         # ## Scaling the results
#         # if(result[1] > 0){                                                              #TODO: check 0 for non-fossil tree
#         #     result <- result/result[1]
#         # }

#     } 

#     result_plot <- result/max(disparity)*12


#     lines(y = result_plot[-length(result)][names(ordered_nodes)], x = plot_nodes, col = "orange")


#     mean(c(disparity["14"])) == result["14"]
#     mean(c(disparity["15"])) == result["15"]
#     mean(c(disparity["16"])) == result["16"]
#     mean(c(disparity["17"])) == result["17"]
#     mean(c(disparity["18"], disparity["23"])) == result["18"]
#     mean(c(disparity["19"], disparity["23"])) == result["19"]
#     mean(c(disparity["20"], disparity["23"])) == result["20"]
#     mean(c(disparity["21"], disparity["23"])) == result["21"]
#     mean(c(disparity["22"], disparity["23"])) == result["22"]
#     mean(c(disparity["22"], disparity["24"])) == result["23"]
#     mean(c(disparity["24"])) == result["24"]
#     mean(c(disparity["25"])) == result["25"]


#     ## TODO: do an option like: @param use.average logical, whether to use the average clade disparity (\code{TRUE}; as used in \code{geiger::dtt}) or just the clade disparity (\code{FALSE}). See details.

#     #@details When using \code{use.average = FALSE}, disparity is calculated as the disparity of each clade (i.e. the disparity for each node and their unique descendants). When using \code{use.average = TRUE}, disparity is calculated 





#         ## Combining the tree and the data
#         phy <- tree

#         ## Get the node depth
#         node.depth <- tree.age(phy2)$ages[-c(1:Ntip(phy2))]
#         names(node.depth) <- 1:length(node.depth) + Ntip(phy2)
    
    
#         stem.depth <- numeric()
#         for(one_node in 1:Nnode(phy)) {
#             ## Get the ancestor
#             anc <- which(phy$edge[,2] == one_node+Ntip(phy)+1)
#             stem.depth[one_node] <- node.depth[names(node.depth) == phy2$edge[anc,1]]
#         }

#         ## Lineages through time
#         ltt <- sort(node.depth, decreasing = TRUE)

#         ## Scaling by lineage through time
#         node.depth <- node.depth/max(ltt)
#         stem.depth <- stem.depth/max(ltt)
#         ltt <- ltt/max(ltt)

#         result <- numeric()

#         ## Calculate disparity per clade





#         ## Disparity at the root
#         result[1] <- disparity[1]
#         backup_check <- list()
#         backup_check[[1]] <- disparity[1]

#         ## Disparity for the other nodes (average per time slice)
#         for(i in 2:length(ltt)) {
#             x <- disparity[stem.depth >= ltt[i-1] & node.depth < ltt[i-1]]
#             #result[i] <- ifelse(length(x) == 0, 0, mean(x))                             #TODO: check 0 for non-fossil tree
#             backup_check[[i]] <- x
#             if(length(x) == 0) {
#                 result[i] <- 0
#             } else {
#                 result[i] <- mean(x)
#             }
#         }




# #        14        15        16        17        18        19        20        21 
# # 0.7198663 0.6992722 0.4539000 0.4677565 0.5668206 0.5991425 0.7182857 0.9987066 
# #        22        23        24        25 
# # 0.4368961 0.1490411 0.1522438 0.2928359 

# # [[1]]
# #        14 
# # 0.7198663 

# # [[2]]
# # named numeric(0)

# # [[3]]
# # named numeric(0)

# # [[4]]
# # named numeric(0)

# # [[5]]
# #        15 
# # 0.6992722 

# # [[6]]
# #        17 
# # 0.4677565 

# # [[7]]
# #        18        23 
# # 0.5668206 0.1490411 # this is node 18 + 23

# # [[8]]
# #        18        23 
# # 0.5668206 0.1490411 

# # [[9]]
# #        20        23 
# # 0.7182857 0.1490411 

# # [[10]]
# #        21        23 
# # 0.9987066 0.1490411 

# # [[11]]
# #        22        23 
# # 0.4368961 0.1490411 

# # [[12]]
# #        22        24 
# # 0.4368961 0.1522438 


























#         if(!node.clade.disparity) {
#             ## Basic disparity
#             return(disparity_per_node)
#         } else {
#             ## Detect each node that has a pair of nodes as descendants
#             which(tree$edge[, ]

#             tree$edge[which(!(tree$edge[,2] %in% 1:Ntip(tree))), ]

#             ## Get the disparity for all nodes
#             disparity_per_node 
#         }
#     }


# geiger_data <- get(data(geospiza))
# data = geiger_data$dat
# average.sq <- function(X) mean(pairwise.dist(X)^2)
# metric = average.sq
# tree = geiger_data$phy
# tree$root.time <- 5


#     # Step 2 measure disparity for that group through time
#     clade_disparity <- dispRity(clade_groups, metric = average.sq)
#     disp_clade <- unlist(get.disparity(clade_disparity))
#         # If simulation step
#         ## measure disparity on the simulations

# # To investigate patterns of disparity through time, we moved up the phylogeny from the root. At each divergence event (i.e., each node), we calculated the mean relative disparity for that point in time as the average of the relative disparities of all subclades whose ancestral lineages were present at that time. Values near 0 imply that subclades contain relatively little of the variation present within the taxon as a whole and that, consequently, most variation is partitioned as among-subclade differences; conversely, values near 1 imply that subclades contain a substantial proportion of the total variation and thus are likely to overlap extensively, indicating that subclades have independently evolved to occupy similar regions of morphological space. In this way, we plotted an average disparity through time analogous to the plots of lineage accumulation through time

# # To calculate how much mean disparity differed from that expected under a null hypothesis of character evolution by unconstrained Brownian motion, we conducted 1000 simulations of morphological diversification on each taxon's phylogeny (25). [(We also conducted simulations with a speciational model of character evolution; results were qualitatively unchanged (fig. S5).] Variances of each morphological axis among species in simulations were set equal to the actual variances of the principal components for each taxon. The morphological disparity index (MDI), the overall difference in relative disparity of a clade compared with that expected under the null hypothesis, was calculated as the area contained between the line connecting observed relative disparity points versus the line connecting median relative disparity points of the simulations; areas in which observed values were above expected were given positive values, whereas those below expected were given negative values. Results from this analysis were corroborated with the δ statistic, which tests for acceleration or deceleration of morphological change through time against a null model of constant rates of character evolution (26).

#     # If simulation step
#         ## measure the alternative




#     ## Create the subsets for each clade and calculate disparity
# clade_disparity <- dispRity(clade_groups, metric = average.sq)
# disp_clade <- unlist(get.disparity(clade_disparity))
# disp_clade <- disp_clade/max(disp_clade) 
# lines(x = dtt_disparity$times[-c(1)], y = disp_clade, col = "blue", lwd = 2)







#     ## mdi.range
#     if(is.null(dots$mdi.range)) {
#         dots$mdi.range <- c(0,1)
#     }

#     ## Get the scaled disparity through time
#     disparity_through_time1 <- geiger.dtt.dispRity(tree, data, metric)
#     disparity_through_time2 <- c(unname(unlist(get.disparity(scale.dispRity(dispRity(custom.subsets(data, tree), metric = metric))))), 0)


#     ## Get the lineages through time
#     lineage_through_time <- sort(branching.times(tree), decreasing = TRUE)
#     if(scale.time) {
#         lineage_through_time <- c(0, (max(lineage_through_time)-lineage_through_time)/max(lineage_through_time))
#     }

#     ## Simulating the null disparity through time
#     if(is.numeric(nsim) && nsim > 0){

#         ## Calculating the rate matrix
#         rate_matrix <- geiger.ratematrix(tree, data)

#         ## Simulate the data
#         #TODO: Eventually replace this with dads!
#         simulated_data <- geiger.sim.char(tree, rate_matrix, nsim, model = model)

#         disparity_through_time_sim <- geiger.dtt.dispRity(tree, simulated_data, metric)

#         # ## Convert into a list
#         # simulated_data <- lapply(seq(dim(simulated_data)[3]), function(x) simulated_data[ , , x])

#         # ## Calculating the disparity
#         # disparity_through_time_sim <- lapply(simulated_data, function(simulated_data, tree, metric) .dtt.dispRity(tree, simulated_data, metric), tree, metric)
#         # disparity_through_time_sim <- matrix(unlist(disparity_through_time_sim), ncol = nsim, byrow = FALSE)

#         colnames(disparity_through_time_sim) <- NULL

#         ## MDI
#         MDI <- unname(geiger.area.between.curves(lineage_through_time, apply(disparity_through_time_sim, 1, median, na.rm = TRUE), disparity_through_time, sort(dots$mdi.range)))

#         ## Get the simulated MDIs
#         sim_MDI <- apply(disparity_through_time_sim, 2, function(X) geiger.area.between.curves(x = lineage_through_time, f1 = X, f2 = disparity_through_time))

#         ## Calculate the p_value
#         p_value <- get.p.value(sim_MDI, MDI)

#         ## Sort the output
#         output <- list(dtt = disparity_through_time, times = lineage_through_time, sim = disparity_through_time_sim, MDI = MDI, sim_MDI = sim_MDI, p_value = p_value, call = match_call)

#         ## Add the model (if it was used as default)
#         if(is.null(output$call$model)) {
#             output$call$model <- model
#         }
#         if(is.null(output$call$alternative)) {
#             output$call$alternative <- alternative
#         }

#     } else {

#         output <- list(dtt = disparity_through_time, times = lineage_through_time)
#     }

#     class(output) <- c("dispRity", "dtt")

#     return(output)
# }
