#' @title adonis dispRity (from \code{vegan::adonis})
#'
#' @description Passing \code{dispRity} objects to the \code{\link[vegan]{adonis}} function from the \code{vegan} package.
#'
#' @param data A \code{dispRity} object with subsets
#' @param formula The model formula (default is \code{matrix ~ group}, see details)
#' @param method The distance method to be passed to \code{\link[vegan]{adonis}} and eventually to \code{\link[vegan]{vegdist}} (see details - default \code{method ="euclidean"})
#' @param ... Any optional arguments to be passed to \code{\link[vegan]{adonis}}
#' @param warn \code{logical}, whether to print internal warnings (\code{TRUE}; default - advised) or not (\code{FALSE}).
#' 
#' @details
#' The first element of the formula (the response) must be called \code{matrix} and the predictors must be existing in the subsets of the \code{dispRity} object.
#'
#' If \code{data$matrix} is not a distance matrix, distance is calculated using the \code{\link[stats]{dist}} function. The type of distance can be passed via the standard \code{method} argument that will be recycled by \code{\link[vegan]{adonis}}.
#' 
#' If the \code{dispRity} data has custom subsets with a single group, the formula is set to \code{matrix ~ group}.
#' 
#' If the \code{dispRity} data has custom subsets with multiple group categories (separated by a dot, e.g. \code{c("group1.cat1", "group1.cat2", "group2.catA", "group2.catB")} being two groups with two categories each), the default formula is \code{matrix ~ first_group} but can be set to any combination (e.g. \code{matrix ~ first_group + second_group}).
#' 
#' If the \code{dispRity} data has time subsets, the predictor is automatically set to \code{time}.
#' 
#' @seealso
#' \code{\link[vegan]{adonis}}, \code{\link{test.dispRity}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}.
#' 
#' @examples
#' ## Adonis with one groups 
#' 
#' ## Generating a random character matrix
#' character_matrix <- sim.morpho(rtree(20), 50, rates = c(rnorm, 1, 0))
#' ## Calculating the distance matrix
#' distance_matrix <- as.matrix(dist(character_matrix))
#' ## Creating two groups
#' random_groups <- list("group1" = 1:10, "group2" = 11:20)
#' 
#' ## Generating a dispRity object
#' random_disparity <- custom.subsets(distance_matrix, random_groups)
#' ## Running a default NPMANOVA
#' adonis.dispRity(random_disparity)
#' 
#' 
#' ## Adonis with multiple groups
#' 
#' ## Creating a random matrix
#' random_matrix <- matrix(data = rnorm(90), nrow = 10, 
#'                      dimnames = list(letters[1:10]))
#' ## Creating two groups with two states each
#' groups <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5), rep(c(1,2), 5)),
#'          nrow = 10, ncol = 2, dimnames = list(letters[1:10], c("g1", "g2"))))
#' 
#' ## Creating the dispRity object
#' multi_groups <- custom.subsets(random_matrix, groups)
#' 
#' ## Running the NPMANOVA
#' adonis.dispRity(multi_groups, matrix ~ g1 + g2)
#' 
#' ## Adonis with time
#' 
#' ## Creating time series
#' data(BeckLee_mat50)
#' data(BeckLee_tree)
#' data(BeckLee_ages)
#' time_subsets <- chrono.subsets(BeckLee_mat50, BeckLee_tree, 
#'      method = "discrete", inc.nodes = FALSE, time = c(100, 85, 65, 0),
#'      FADLAD = BeckLee_ages)
#' 
#' ## Running the NPMANOVA with time as a predictor
#' adonis.dispRity(time_subsets, matrix ~ time)
#' 
#' ## Running the NPMANOVA with each time bin as a predictor
#' adonis.dispRity(time_subsets, matrix ~ chrono.subsets)
#' 
#' @seealso \code{\link{test.dispRity}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

# source("sanitizing.R")
# source("adonis.dispRity_fun.R")
# formula = matrix ~ group
# method = "euclidean"
# warn = TRUE

adonis.dispRity <- function(data, formula = matrix ~ group, method = "euclidean", ..., warn = TRUE) {

    match_call <- match.call()

    ## data must be dispRity
    check.class(data, "dispRity")

    ## data must have subsets
    if(is.null(data$subsets) || length(data$subsets) == 0) {
        stop("The data must have subsets. Use custom.subsets() or chrono.subsets() to create some.", call. = FALSE)
    } else {
        if(is.na(match(data$call$subsets[1], "customised"))) {
            ## Subsets are time
            time_subsets <- TRUE
        } else {
            time_subsets <- FALSE
        }
    }

    ## formula must be formula
    check.class(formula, "formula")

    ## formula must have the right response/predictors
    formula_error_format <- "Formula must be of type: matrix ~ predictor(s) (where matrix is the response)."
    check.length(formula, 3, msg = formula_error_format)
    # if(!formula[[1]] == "~") stop(formula_error_format) #TG: Tested from formula
    if(!formula[[2]] == "matrix") stop(formula_error_format)
    ## Non-default predictors
    if(!formula[[3]] == "group") {

        ## Predictor is time
        if(formula[[3]] == "time" || formula[[3]] == "chrono.subsets") {

            if(!time_subsets) {
                stop(paste0(as.expression(match_call$data), " has no time subsets.\nImpossible to use the following formula: ", as.expression(match_call$formula)), call. = FALSE)
            }
            
            ## Set up the model details
            n_predictors <- ifelse(formula[[3]] == "time", 1, length(data$subsets))
            group_names <- ifelse(formula[[3]] == "time", "time", NA)
            group_variables <- NA
            pool_time <- ifelse(formula[[3]] == "time", TRUE, FALSE)

            ## Update the formula
            if(formula[[3]] == "chrono.subsets") {
                group_names_tmp <- paste0("t", gsub(" - ", "to", names(data$subsets)))
                series <- paste(group_names_tmp, collapse = " + ")
                formula <- formula(paste0("matrix ~ ", series))
            } else {
                time_f <- ~time
                formula[[3]] <- time_f[[2]]
            }
        } else {

            n_predictors <- length(formula[[3]])-1
            group_variables <- names(data$subsets)
            group_names <- unique(unlist(lapply(strsplit(group_variables, split = "\\."), `[[`, 1)))
            split.variables <- function(one_group_name, group_variables) {
                return(group_variables[grep(one_group_name, group_variables)])

            }
            group_variables <- lapply(as.list(group_names), split.variables, group_variables)
            
            ## Check the predictors
            for(predictor in 1:n_predictors) {
                if(is.na(match(as.character(formula[[3]][[predictor + 1]]), group_names))) {
                    stop(paste0("Predictor ", as.character(formula[[3]][[predictor + 1]]), " not found in ", as.expression(match_call$data), " subsets.\n"))
                }
            
            }
        }
    } else {

        n_predictors <- 1

        if(time_subsets) {
            ## Modifying group to be time
            time_fun <- ~ time
            formula[[3]] <- time_fun[[2]]
            group_names <- "time"
            group_variables <- NA
            pool_time <- TRUE

        } else {
            group_names <- as.character(formula[[3]])
            group_variables <- names(data$subsets)
        }
    }

    ## methods
    methods_avail <- c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis")
    check.method(method, methods_avail, "method")

    ## warnings
    check.class(warn, "logical")
    
    ## Checking if the data is a distance matrix or not
    matrix <- check.dist.matrix(data$matrix, method = method)
    was_dist <- matrix[[2]]
    matrix <- matrix[[1]]
    if(warn && !was_dist) {
        warning("The input data for adonis.dispRity was not a distance matrix.\nThe results are thus based on the distance matrix for the input data (i.e. dist(data$matrix)).\nMake sure that this is the desired methodological approach!")
    }

    ## Making the predictors table
    predictors <- make.factors(data, group_names, group_variables, time_subsets, pool_time)

    if(any(is.na(predictors))) {
        ## Select the NA predictor
        na_predictor <- as.vector(apply(predictors, 1, is.na))
        warning(paste0("The following element(s) has no associated predictor value and was removed from the analysis: ", paste(attr(matrix, "Labels")[na_predictor], collapse = ", "), "."))

        ## Removing the data from the predictors
        predictors_tmp <- as.matrix(predictors[!na_predictor,])
        colnames(predictors_tmp) <- colnames(predictors)
        for(col in 1:ncol(predictors)) {
            predictors_tmp[,col] <- factor(predictors_tmp[,col])
        }
        predictors <- data.frame(predictors_tmp)

        ## Removing the data from the matrix
        matrix_tmp <- as.matrix(matrix)
        matrix_tmp <- matrix_tmp[,-which(na_predictor)]
        matrix_tmp <- matrix_tmp[-which(na_predictor),] 
        matrix <- as.dist(matrix_tmp)
    }

    ## Run adonis
    adonis_out <- vegan::adonis(formula, predictors, method = method, ...)
    #adonis_out <- vegan::adonis(formula, predictors, method = method) ; warning("DEBUG adonis.dispRity")

    ## Update the formula
    if(time_subsets) {
        if(pool_time) {
            time_f <- ~time
        } else {
            time_f <- ~chrono.subsets
        }
        formula[[3]] <- time_f[[2]]
    }
    if(!was_dist) {
        matrix_f <- ~dist(matrix)
        formula[[2]] <- matrix_f[[2]]
    }

    ## Update the call
    adonis_out$call[[which(names(adonis_out$call) == "formula")]] <- as.expression(formula)[[1]]
    adonis_out$call[[which(names(adonis_out$call) == "data")]] <- match_call$data
    if(method == "euclidean") {
        adonis_out$call[[which(names(adonis_out$call) == "method")]] <- "euclidean"
    } else {
        adonis_out$call[[which(names(adonis_out$call) == "method")]] <- match_call$method
    }

    return(adonis_out)
}