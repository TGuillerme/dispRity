# ' @name sequential.test
# '
# ' @title Sequential linear regressions
# '
# ' @description Function still in development.
# ' @description Performs a sequential \code{\link[stats]{glm}} on the subsets by correcting for time autocorrelation. 
# '
# ' @param subsets time subsets of which to estimate the slopes sequentially.
# ' @param family the family of the \code{\link[stats]{glm}}.
# ' @param correction optional, which p-value correction to apply (see \code{\link[stats]{p.adjust}}). If missing, no correction is applied.
# ' @param call optional, a call from a \code{dispRity} object.
# ' @param ... optional arguments to be passed to the \code{\link[stats]{glm}}.
# '
# ' @details
# ' This test allows to correct for time autocorrelation by estimating the intercept of the \code{\link[stats]{glm}} using a predicted intercept using the preceding \code{\link[stats]{glm}}.
# '
# ' @examples
# ' ## Load the Beck & Lee 2014 data
# ' data(BeckLee_mat50)
# ' ## Calculating the disparity from a customised subsets
# ' ## Generating the subsets
# ' groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 12),
# '      rep(4, 13)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# ' customised_subsets <- custom.subsets(BeckLee_mat50, groups)
# ' ## Bootstrapping the data
# ' bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 100)
# ' ## Calculating variances of each dimension
# ' dim_variances <- dispRity(bootstrapped_data, metric = variances)
# ' ## Extracting the disparity values of each subset
# ' subsets <- extract.dispRity(dim_variances, observed = FALSE,
# '      keep.structure = TRUE, concatenate = TRUE)
# '
# ' ## Running a gaussian sequential test on the subsets
# ' results <- sequential.test(subsets, family = gaussian)
# ' ## Summarising the results
# ' summary(results, digits = 5)
# ' ## Simple plotting the results
# ' plot(results)
# ' 
# ' ## Running a Gaussian sequential test on multiple subsets
# ' ## (i.e. non- concatenated)
# ' subsets <- extract.dispRity(dim_variances, observed = FALSE,
# '      keep.structure = TRUE, concatenate = FALSE)
# ' results <- sequential.test(subsets, family = gaussian)
# ' ## Summarising
# ' summary(results, digits = 5, quantiles = c(50, 75), cent.tend = mean)
# ' ## Plotting the disparity first (as the me)
# ' plot(dim_variances, type = "c", cent.tend = median)
# ' ## Adding the sequential model (using the first quantile (12.55%) for our 
# ' ## the significance level to consider).
# ' plot(results, add = TRUE, significance = 1,
# '      token.args = list(float = 0.3, col = "blue", cex = 2),
# '      lines.args = list(col = "red", lty = 3))
# ' 
#' @seealso \code{\link{test.dispRity}}, \code{\link{bhatt.coeff}}, \code{\link{null.test}}.
#'
#' @author Thomas Guillerme
# @export

#Testing
# source("sanitizing.R")
# source("sequential.test_fun.R")
# source("test.dispRity_fun.R")
# data(BeckLee_mat50)
# groups <- as.data.frame(matrix(data = c(rep(1, 12), rep(2, 13), rep(3, 25)), dimnames = list(rownames(BeckLee_mat50))), ncol = 1)
# customised_subsets <- custom.subsets(BeckLee_mat50, groups)
# bootstrapped_data <- boot.matrix(customised_subsets, bootstraps = 3)
# data_single <- dispRity(bootstrapped_data, metric = c(sum, variances))
# data_multi <- dispRity(bootstrapped_data, metric = variances)
# subsets_single <- extract.dispRity(data_single, observed = FALSE)
# subsets_multi <- extract.dispRity(data_multi, observed = FALSE, concatenate = FALSE)
# results = "coefficients"
# family = gaussian
# data <- sequential.test(subsets_multi, family = gaussian)

# sequential.test <- function(){ #function(subsets, family, correction, call = NULL, ...)

    #Warning
    # stop("The sequential.test function is still under development!")

    # #SANITIZING
    # match_call <- match.call()
    # #warning("DEBUG") ; return(match_call)
    
    # #Family
    # if(missing(family)) {
    #     stop("glm family type argument is necessary!")
    # }

    # #correction
    # if(!missing(correction)) {
    #     check.class(correction, 'character')
    #     p.adjust_list<- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
    #     if(all(is.na(match(correction, p.adjust_list)))) {
    #         stop("correction type must be one of the p.adjust function options.")
    #     }
    # }

    # #Testing whether the results are distributions (BSed) or not.
    # is.distribution <- ifelse(unique(unlist(lapply(subsets, class))) == "numeric", FALSE, TRUE)

    # #If is not a distribution, reformat the list to be a list of MethodsListSelect
    # if(is.distribution != TRUE) {
    #     subsets <- lapply(subsets, list)
    # } 

    # #APPLYING THE SEQUENTIAL TEST

    # #Setting the sequence
    # seq_subsets <- set.comparisons.list("sequential", subsets, 1)

    # #Applying the first test to get the intercept origin
    # first_model <- lapply(set.pair.subsets(subsets[seq_subsets[[1]]]), create.model, intercept = NULL, family, ...)
    # #first_model <- lapply(set.pair.subsets(subsets[seq_subsets[[1]]]), create.model, intercept = NULL, family) ; warning("DEBUG")
    
    # #Calculate the intercepts for each first models
    # intercept_predict <- list()
    # intercept_predict[[1]] <- lapply(first_model, set.intercept0)

    # #Storing the first model
    # models <- list()
    # models[[1]] <- first_model

    # #Loop through the other models
    # for(model in 2:(length(seq_subsets))) {
    #     #Calculate the new intercept from the previous model
    #     intercept_predict[[model]] <- mapply(set.intercept.next, models[[model-1]], intercept_predict[[model-1]], SIMPLIFY = FALSE)

    #     #Create the new model 
    #     models[[model]] <- lapply(set.pair.subsets(subsets[seq_subsets[[model]]], intercept = intercept_predict[[model-1]]), create.model, intercept = "in.data", family, ...)
    #     #models[[model]] <- lapply(set.pair.subsets(subsets[seq_subsets[[model]]], intercept = intercept_predict[[model-1]]), create.model, intercept = "in.data", family) ; warning("DEBUG")
    # }

    # #OUTPUT
    # #Naming the models
    # names(models) <- save.comparison.list(seq_subsets, subsets)
    # #Creating the new call
    # if(!missing(correction)) {
    #     new_call <- paste("Sequential test (", as.character(match_call$family), ") across ", length(models)+1, " subsets with ", as.character(correction), " correction.\n@", sep="")
    # } else {
    #     new_call <- paste("Sequential test (", as.character(match_call$family), ") across ", length(models)+1, " subsets.\n@", sep="")
    # }

    # #Adding previous call (if exists)
    # if(!is.null(call)) {
    #     new_call <- paste(new_call, call, sep = "")
    # }

    # #output
    # if(!missing(correction)) {
    #     output_raw <- list("models" = models, "intercepts" = intercept_predict, "call" = new_call, "correction" = correction)
    # } else {
    #     output_raw <- list("models" = models, "intercepts" = intercept_predict, "call" = new_call)
    # }

    # class(output_raw) <- c("dispRity", "seq.test")
    # return(output_raw)
# }