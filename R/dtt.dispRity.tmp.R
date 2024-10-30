#' @title dtt dispRity (from \code{geiger::dtt})
#'
#' @description A wrapper for the \code{geiger::dtt} function working with any disparity metric.
#'
#' @param data A \code{dispRity} object or a \code{matrix}
#' @param metric The disparity metric to be passed to \code{\link{dispRity}}.
#' @param tree A \code{phylo} object matching the data and with a \code{root.time} element. Can be missing if \code{data} has a \code{tree} component.
#' @param nsim The number of simulations to calculate null disparity-through-time.
#' @param model A evolutionary model for the simulations (see \code{geiger::sim.char} - default is \code{"BM"}).
#' @param alternative The H1 alternative (for calculating the p-value). Can be \code{"two-sided"} (default), \code{"greater"} or \code{"lesser"}; see details.
#' @param scale.time Optional, whether to scale the time (between 0 and 1; \code{TRUE}, default) or not (\code{FALSE}).
#' @param ... Any other arguments to be passed to \code{geiger::dtt}.
#' 
#' @details
#' See \code{geiger::dtt} for details.
#' 
#' @examples
#'
#' ## Loading morphological data and a tree
#' data(BeckLee_mat50)
#' data(BeckLee_tree)
#' 
#' ## The average squared pairwise distance metric (used in geiger::dtt)
#' average.sq <- function(X) mean(pairwise.dist(X)^2)
#' 
#' ## Calculate the disparity of the dataset using dtt.dispRity
#' dispRity_dtt <- dtt.dispRity(data = BeckLee_mat50, metric = average.sq,
#'                              tree = BeckLee_tree, nsim = 20)
#' 
#' ## Plotting the results
#' plot(dispRity_dtt)
#' 
#' 
#' @seealso
#' \code{\link{test.dispRity}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{plot.dispRity}}.
#' 
#' @author Thomas Guillerme
# @export

# source("sanitizing.R")
# source("dispRity_fun.R")
# source("dtt.dispRity_fun.R")
# geiger_data <- get(data(geospiza))
# data = geiger_data$dat
# average.sq <- function(X) mean(pairwise.dist(X)^2)
# metric = average.sq
# tree = geiger_data$phy
# tree$root.time <- 5
# nsim = 100
# model = "BM"
# #relative.disp = TRUE
# alternative = "two-sided"
# dots <- list()

# set.seed(1)
# dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq, tree = geiger_data$phy, nsim = 100, alternative = "two-sided", ...)
# dispRity_dtt$p_value

# average.sq <- function(X) mean(pairwise.dist(X)^2)
# metric = average.sq
# tree <- rcoal(5)
# data <- space.maker(5, 4, rnorm)
# rownames(data) <- tree$tip.label


# Modified version of the geiger::dtt function (https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R)
dtt.dispRity.tmp <- function(data, metric, tree, nsim = 0, model = "BM", alternative = "two-sided", scale.time = TRUE, ...) {


    match_call <- match.call()
    dots <- list(...)

    ## SANITIZING
    ## data
    data_class <- check.class(data, c("dispRity", "matrix"))
    if(data_class == "dispRity") {
        data <- data$matrix
    } else {
        data <- list(data)
    }

    ## metric
    metrics_list <- get.dispRity.metric.handle(metric, match_call)

    ## Tree
    check.class(tree, "phylo")
    #TODO: in the future allow for a list of trees

    ## Match the tree to the data
    cleaned_data <- clean.data(data, tree)
    data <- cleaned_data$data
    tree <- cleaned_data$tree

    if(!is.na(cleaned_data$dropped_tips)) {
        warning("The following tip(s) was not present in the data: ", paste(cleaned_data$dropped_tips, collapse = ", "), ".")
    }
    if(!is.na(cleaned_data$dropped_rows)) {
        warning("The following element(s) was not present in the tree: ", paste(cleaned_data$dropped_rows, collapse = ", "), ".")
    }

    ## alternative
    check.method(alternative, c("two-sided", "greater", "lesser"), msg = "alternative")
    ## Set up the p-value function
    get.p.value <- switch(alternative,
        "two-sided" = {
            function(sim_disparity, obs_disparity, replicates) {
                ## Centring the randoms and observed
                center_random <- abs(sim_disparity - mean(sim_disparity, na.rm = TRUE))
                center_observed <- abs(obs_disparity - mean(sim_disparity, na.rm = TRUE))
                ## Getting the p
                return((sum(center_random >= center_observed))/(length(sim_disparity)))
            }
        },
        greater = {
           function(sim_disparity, obs_disparity, replicates) {
                # Getting the p
                return((sum(sim_disparity >= obs_disparity))/(length(sim_disparity)))
            }
        },
        lesser = {
            function(sim_disparity, obs_disparity, replicates) {
                # Getting the p
                return((sum(sim_disparity <= obs_disparity))/(length(sim_disparity)))
            }
        }
    )

    ## nsim
    check.class(nsim, c("numeric", "integer"))
    if(nsim > 0) {
        ## Check the model
        model_default <- c("BM", "OU")
    }






    ## New version

    # Step 1 get clade groups through time
    clade_groups <- custom.subsets(data, group = tree)
        # If simulation step
        ## map the trait on the tree n_times

    get.nodes.disparity <- function(data, tree, metric) {
        
        ## Basic disparity
        disparity_per_node <- unlist(get.disparity(dispRity(custom.subsets(data, group = tree), metric = average.sq)))

        ## Disparity is calculated for all nodes as the disparity of the descendants
        ## But also as the average disparity from all descendants if there is more data on other branches
        ## e.g. for the plot(tree), node 17 is average of node 18 and 23, 18 is the average of 19 and 23, etc.
        ## This is some kind of acctran chrono.subsets with no node values? - probably not




        if(!node.clade.disparity) {
            ## Basic disparity
            return(disparity_per_node)
        } else {
            ## Detect each node that has a pair of nodes as descendants
            which(tree$edge[, ]

            tree$edge[which(!(tree$edge[,2] %in% 1:Ntip(tree))), ]

            ## Get the disparity for all nodes
            disparity_per_node 
        }
    }


    # Step 2 measure disparity for that group through time
    clade_disparity <- dispRity(clade_groups, metric = average.sq)
    disp_clade <- unlist(get.disparity(clade_disparity))
        # If simulation step
        ## measure disparity on the simulations

# To investigate patterns of disparity through time, we moved up the phylogeny from the root. At each divergence event (i.e., each node), we calculated the mean relative disparity for that point in time as the average of the relative disparities of all subclades whose ancestral lineages were present at that time. Values near 0 imply that subclades contain relatively little of the variation present within the taxon as a whole and that, consequently, most variation is partitioned as among-subclade differences; conversely, values near 1 imply that subclades contain a substantial proportion of the total variation and thus are likely to overlap extensively, indicating that subclades have independently evolved to occupy similar regions of morphological space. In this way, we plotted an average disparity through time analogous to the plots of lineage accumulation through time

# To calculate how much mean disparity differed from that expected under a null hypothesis of character evolution by unconstrained Brownian motion, we conducted 1000 simulations of morphological diversification on each taxon's phylogeny (25). [(We also conducted simulations with a speciational model of character evolution; results were qualitatively unchanged (fig. S5).] Variances of each morphological axis among species in simulations were set equal to the actual variances of the principal components for each taxon. The morphological disparity index (MDI), the overall difference in relative disparity of a clade compared with that expected under the null hypothesis, was calculated as the area contained between the line connecting observed relative disparity points versus the line connecting median relative disparity points of the simulations; areas in which observed values were above expected were given positive values, whereas those below expected were given negative values. Results from this analysis were corroborated with the δ statistic, which tests for acceleration or deceleration of morphological change through time against a null model of constant rates of character evolution (26).

    # If simulation step
        ## measure the alternative




    ## Create the subsets for each clade and calculate disparity
clade_disparity <- dispRity(clade_groups, metric = average.sq)
disp_clade <- unlist(get.disparity(clade_disparity))
disp_clade <- disp_clade/max(disp_clade) 
lines(x = dtt_disparity$times[-c(1)], y = disp_clade, col = "blue", lwd = 2)







    ## mdi.range
    if(is.null(dots$mdi.range)) {
        dots$mdi.range <- c(0,1)
    }

    ## Get the scaled disparity through time
    disparity_through_time1 <- geiger.dtt.dispRity(tree, data, metric)
    disparity_through_time2 <- c(unname(unlist(get.disparity(scale.dispRity(dispRity(custom.subsets(data, tree), metric = metric))))), 0)


    ## Get the lineages through time
    lineage_through_time <- sort(branching.times(tree), decreasing = TRUE)
    if(scale.time) {
        lineage_through_time <- c(0, (max(lineage_through_time)-lineage_through_time)/max(lineage_through_time))
    }

    ## Simulating the null disparity through time
    if(is.numeric(nsim) && nsim > 0){

        ## Calculating the rate matrix
        rate_matrix <- geiger.ratematrix(tree, data)

        ## Simulate the data
        #TODO: Eventually replace this with dads!
        simulated_data <- geiger.sim.char(tree, rate_matrix, nsim, model = model)

        disparity_through_time_sim <- geiger.dtt.dispRity(tree, simulated_data, metric)

        # ## Convert into a list
        # simulated_data <- lapply(seq(dim(simulated_data)[3]), function(x) simulated_data[ , , x])

        # ## Calculating the disparity
        # disparity_through_time_sim <- lapply(simulated_data, function(simulated_data, tree, metric) .dtt.dispRity(tree, simulated_data, metric), tree, metric)
        # disparity_through_time_sim <- matrix(unlist(disparity_through_time_sim), ncol = nsim, byrow = FALSE)

        colnames(disparity_through_time_sim) <- NULL

        ## MDI
        MDI <- unname(geiger.area.between.curves(lineage_through_time, apply(disparity_through_time_sim, 1, median, na.rm = TRUE), disparity_through_time, sort(dots$mdi.range)))

        ## Get the simulated MDIs
        sim_MDI <- apply(disparity_through_time_sim, 2, function(X) geiger.area.between.curves(x = lineage_through_time, f1 = X, f2 = disparity_through_time))

        ## Calculate the p_value
        p_value <- get.p.value(sim_MDI, MDI)

        ## Sort the output
        output <- list(dtt = disparity_through_time, times = lineage_through_time, sim = disparity_through_time_sim, MDI = MDI, sim_MDI = sim_MDI, p_value = p_value, call = match_call)

        ## Add the model (if it was used as default)
        if(is.null(output$call$model)) {
            output$call$model <- model
        }
        if(is.null(output$call$alternative)) {
            output$call$alternative <- alternative
        }

    } else {

        output <- list(dtt = disparity_through_time, times = lineage_through_time)
    }

    class(output) <- c("dispRity", "dtt")

    return(output)
}
