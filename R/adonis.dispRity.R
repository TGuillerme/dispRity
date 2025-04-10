#' @title adonis dispRity (from \code{vegan::adonis2})
#'
#' @description Passing \code{dispRity} objects to the \code{\link[vegan]{adonis2}} function from the \code{vegan} package.
#'
#' @param data A \code{dispRity} object with subsets
#' @param formula The model formula (default is \code{matrix ~ group}, see details)
#' @param method The distance method to be passed to \code{\link[vegan]{adonis2}} and eventually to \code{\link[vegan]{vegdist}} (see details - default \code{method ="euclidean"})
#' @param ... Any optional arguments to be passed to \code{\link[vegan]{adonis2}}
#' @param warn \code{logical}, whether to print internal warnings (\code{TRUE}; default - advised) or not (\code{FALSE}).
#' @param matrix \code{numeric}, which matrix to use (default is \code{1}).
#' 
#' @details
#' The first element of the formula (the response) must be called \code{matrix} and the predictors must be existing in the subsets of the \code{dispRity} object.
#'
#' If \code{data$matrix[[1]]} is not a distance matrix, distance is calculated using the \code{\link[stats]{dist}} function. The type of distance can be passed via the standard \code{method} argument that will be recycled by \code{\link[vegan]{adonis2}}.
#' 
#' If the \code{dispRity} data has custom subsets with a single group, the formula is set to \code{matrix ~ group}.
#' 
#' If the \code{dispRity} data has custom subsets with multiple group categories (separated by a dot, e.g. \code{c("group1.cat1", "group1.cat2", "group2.catA", "group2.catB")} being two groups with two categories each), the default formula is \code{matrix ~ first_group} but can be set to any combination (e.g. \code{matrix ~ first_group + second_group}).
#' 
#' If the \code{dispRity} data has time subsets, the predictor is automatically set to \code{time}.
#' 
#' @seealso
#' \code{\link[vegan]{adonis2}}, \code{\link{test.dispRity}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}.
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
#' 
#' @author Thomas Guillerme
#' @references Oksanen J, Simpson G, Blanchet F, Kindt R, Legendre P, Minchin P, O'Hara R, Solymos P, Stevens M, Szoecs E, Wagner H, Barbour M, Bedward M, Bolker B, Borcard D, Carvalho G, Chirico M, De Caceres M, Durand S, Evangelista H, FitzJohn R, Friendly M, Furneaux B, Hannigan G, Hill M, Lahti L, McGlinn D, Ouellette M, Ribeiro Cunha E, Smith T, Stier A, Ter Braak C, Weedon J (2024). vegan: Community Ecology Package_. R package version 2.6-8,

# @export

# source("sanitizing.R")
# source("adonis.dispRity_fun.R")
# formula = matrix ~ group
# method = "euclidean"
# warn = TRUE

adonis.dispRity <- function(data, formula = matrix ~ group, method = "euclidean", ..., warn = TRUE, matrix = 1) {

    match_call <- match.call()

    ## data must be dispRity
    check.class(data, "dispRity")

    ## data must have subsets
    if(is.null(data$subsets) || length(data$subsets) == 0) {
        stop.call(match_call$data, " must have subsets. Use custom.subsets() or chrono.subsets() to create some.")
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
    if(!formula[[2]] == "matrix") {
        stop.call("", formula_error_format)
    }
    ## Non-default predictors
    if(!formula[[3]] == "group") {

        ## Predictor is time
        if(formula[[3]] == "time" || formula[[3]] == "chrono.subsets") {

            if(!time_subsets) {
                stop.call(match_call$data, paste0(" has no time subsets.\nImpossible to use the following formula: ", as.expression(match_call$formula)))
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
                nas <- is.na(match(as.character(formula[[3]][[predictor + 1]]), group_names))
                if(any(nas)) {
                    arithmetic_signs <- c("+", "-", "/", "*", ":", "|")
                    if(!(as.character(formula[[3]][[predictor + 1]])[nas] %in% arithmetic_signs)) {
                        stop.call(msg.pre = paste0("Predictor ", as.character(formula[[3]][[predictor + 1]])[nas], " not found in "), call = as.expression(match_call$data), msg = " subsets.")
                    }
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

    ## matrix
    check.class(matrix, c("integer", "numeric"))
    matrix_n <- matrix
    
    ## Checking if the data is a distance matrix or not
    matrix <- check.dist.matrix(data$matrix[[matrix_n]], method = method)
    was_dist <- matrix[[2]]
    matrix <- matrix[[1]]
    if(warn && !was_dist) {
        warning(paste0("The input data for adonis.dispRity was not a distance matrix.\nThe results are thus based on the distance matrix for the input data (i.e. dist(data$matrix[[", matrix_n, "]])).\nMake sure that this is the desired methodological approach!"))
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
    ## Modifying adonis2 to only check the parent environment (not the global one: matrix input here should be present in the environment
    adonis2.modif <- adonis2
    formals(adonis2.modif) <-c(formals(adonis2), "matrix_input" = NA)
    body(adonis2.modif)[[5]] <- substitute(lhs <- matrix_input)
    adonis_out <- adonis2.modif(formula, predictors, method = method, matrix_input = matrix, ...)
    # adonis_out <- adonis2.modif(formula, predictors, method = method, matrix_input = matrix) ; warning("DEBUG adonis.dispRity")

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

    ## Update the call (cleaning it)
    call_update <- attr(adonis_out, "heading")[2]
    call_update <- gsub("adonis2.modif", "vegan::adonis2", gsub(", matrix_input = matrix", "", gsub("method = method", paste0("method = \"", method, "\""), gsub(", data = predictors", "", gsub("formula = formula", paste0("formula = ", as.expression(formula)), call_update)))))
    attr(adonis_out, "heading")[2] <- call_update

    return(adonis_out)
}