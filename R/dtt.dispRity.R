#' @title dtt dispRity (from \code{geiger::dtt})
#'
#' @description A wrapper for the \code{\link[geiger]{dtt}} function working with any disparity metric.
#'
#' @param data A \code{dispRity} object or a \code{matrix}
#' @param metric The disparity metric to be passed to \code{\link{dispRity}}.
#' @param tree A \code{phylo} object matching the data and with a \code{root.time} element.
#' @param nsim The number of simulations to calculate null disparity-through-time.
#' @param model A evolutionary model for the simulations (see \code{\link[geiger]{sim.char}} - default is \code{"BM"}).
#' @param alternative The H1 alternative (for calculating the p-value). Can be \code{"two-sided"} (default), \code{"greater"} or \code{"lesser"}; see details.
#' @param ... Any other arguments to be passed to \code{\link[geiger]{dtt}}.
#' 
#' @details
#' See \code{\link[geiger]{dtt}} for details. Note that for calculating the default metrics implemented in \code{\link[geiger]{dtt}} (i.e \code{c("avg.sq", "avg.manhattan", "num.states")}) this implementation in \code{dispRity} is much slower!
#' 
#' 
#' @examples
#' ## Loading geiger's example data set
#' require(geiger)
#' geiger_data <- get(data(geospiza))
#' 
#' ## Calculate the disparity of the dataset using dtt::geiger
#' geiger_dtt <- dtt(phy = geiger_data$phy, data = geiger_data$dat, nsim = 100)
#'
#' ## The average squared pairwise distance metric (used in geiger::dtt)
#' average.sq <- function(X) mean(pairwise.dist(X)^2)
#' 
#' ## Calculate the disparity of the dataset using dtt.dispRity
#' dispRity_dtt <- dtt.dispRity(data = geiger_data$dat, metric = average.sq,
#'                              tree = geiger_data$phy, nsim = 100)
#' 
#' ## Plotting the results
#' plot(dispRity_dtt)
#' 
#' ## Disparity values are identical up to the 9th digit!
#' round(geiger_dtt$dtt, 9) == round(dispRity_dtt$dtt, 9)
#'  
#' ## Calculate disparity with a different metric using dtt.dispRity
#' dispRity_dtt2 <- dtt.dispRity(data = geiger_data$dat, tree = geiger_data$phy,
#'                              metric = c(median, centroids), nsim = 50)
#' plot(dispRity_dtt2)
#' 
#' @seealso
#' \code{\link[geiger]{dtt}}, \code{\link{test.dispRity}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{plot.dispRity}}.
#' 
#' @author Thomas Guillerme
#' @export

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




# Modified version of the geiger::dtt function (https://github.com/mwpennell/geiger-v2/blob/master/R/disparity.R)
dtt.dispRity <- function(data, metric, tree, nsim = 0, model = "BM", alternative = "two-sided", ...) {

    match_call <- match.call()
    dots <- list(...)

    ## SANITIZING
    ## data
    data_class <- check.class(data, c("dispRity", "matrix"))
    if(data_class == "dispRity") {
        data <- data$matrix
    }

    ## metric
    metrics_list <- get.dispRity.metric.handle(metric, match_call)

    ## Stop if data already contains disparity and metric is not level1
    if(!is.null(metrics_list$level3.fun) && length(data$call$disparity$metric) != 0) {
        stop.call("", "Impossible to apply a dimension-level 3 metric on disparity data.")
    }

    ## tree
    check.class(tree, "phylo")

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
    
    ## Choose the p method
    if(alternative == "two-sided") {
        get.p.value <- function(sim_MDI, MDI, replicates) {
            ## Centring the randoms and observed
            center_random <- abs(sim_MDI - mean(sim_MDI))
            center_observed <- abs(MDI - mean(sim_MDI))
            ## Getting the p
            return((sum(center_random >= center_observed))/(length(sim_MDI)))
        }
    }
    if(alternative == "greater") {
        get.p.value <- function(sim_MDI, MDI, replicates) {
            # Getting the p
            return((sum(sim_MDI >= MDI))/(length(sim_MDI)))
        }
    }
    if(alternative == "lesser") {
        get.p.value <- function(sim_MDI, MDI, replicates) {
            # Getting the p
            return((sum(sim_MDI <= MDI))/(length(sim_MDI)))
        }
    }

    ## mdi.range
    if(is.null(dots$mdi.range)) {
        dots$mdi.range <- c(0,1)
    }

    ## Get the scaled disparity through time
    disparity_through_time <- .dtt.dispRity(tree, data, metric)
    
    ## Get the lineages through time
    lineage_through_time <- sort(branching.times(tree), decreasing = TRUE)
    lineage_through_time <- c(0, (max(lineage_through_time)-lineage_through_time)/max(lineage_through_time))

    ## Simulating the null disparity through time
    if(is.numeric(nsim) && nsim > 0){

        ## Calculating the rate matrix
        rate_matrix <- geiger::ratematrix(tree, data)

        ## Simulate the data
        simulated_data <- geiger::sim.char(tree, rate_matrix, nsim, model = model)

        disparity_through_time_sim <- .dtt.dispRity(tree, simulated_data, metric)

        # ## Convert into a list
        # simulated_data <- lapply(seq(dim(simulated_data)[3]), function(x) simulated_data[ , , x])

        # ## Calculating the disparity
        # disparity_through_time_sim <- lapply(simulated_data, function(simulated_data, tree, metric) .dtt.dispRity(tree, simulated_data, metric), tree, metric)
        # disparity_through_time_sim <- matrix(unlist(disparity_through_time_sim), ncol = nsim, byrow = FALSE)

        colnames(disparity_through_time_sim) <- NULL

        ## MDI
        MDI <- unname(.area.between.curves(lineage_through_time, apply(disparity_through_time_sim, 1, median), disparity_through_time, sort(dots$mdi.range)))

        ## Get the simulated MDIs
        sim_MDI <- apply(disparity_through_time_sim, 2, function(X) .area.between.curves(x = lineage_through_time, f1 = X, f2 = disparity_through_time))

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
        
        ## Calculate the p_value
        p_value <- get.p.value(sim_MDI, MDI)

    } else {

        output <- list(dtt = disparity_through_time, times = lineage_through_time)
    }

    class(output) <- c("dispRity", "dtt")

    return(output)
}
