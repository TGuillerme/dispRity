#' @name MCMCglmm.utilities
#' @aliases MCMCglmm.traits MCMCglmm.levels MCMCglmm.sample MCMCglmm.covars MCMCglmm.variance
#' @title MCMCglmm object utility functions
#'
#' @description Different utility functions to extract aspects of a \code{MCMCglmm} object.
#'
#' @usage MCMCglmm.traits(MCMCglmm)
#' @usage MCMCglmm.levels(MCMCglmm, convert)
#' @usage MCMCglmm.sample(MCMCglmm, n)
#' @usage MCMCglmm.covars(MCMCglmm, n, sample)
#' @usage MCMCglmm.variance(MCMCglmm, n, sample, levels, scale)
#'  
#' @param MCMCglmm A \code{MCMCglmm} object.
#' @param n        Optional, a number of random samples to extract.
#' @param sample   Optional, the specific samples to extract (is ignored if \code{n} is present).
#' @param convert  Logical, whether to return the raw term names names as expressed in the model column names (\code{FALSE}) or to convert it to something more reader friendly (\code{TRUE}; default).
#' @param levels   Optional, a vector \code{"character"} values (matching \code{MCMCglmm.levels(..., convert = TRUE)}) or of \code{"numeric"} values designating which levels to be used to calculate the variance (if left empty, all the levels are used).
#' @param scale    Logical, whether to scale the variance relative to all the levels (\code{TRUE}; default) or not (\code{FALSE})/
#'
#' @details
#' \itemize{
#'      \item \code{MCMCglmm.levels} returns the different random and residual terms levels of a \code{MCMCglmm} object. This function uses the default option \code{convert = TRUE} to convert the names into something more readable. Toggle to \code{convert = FALSE} for the raw names.
#'      \item \code{MCMCglmm.traits} returns the column names of the different traits of a \code{MCMCglmm} formula object.
#'      \item \code{MCMCglmm.sample} returns a vector of sample IDs present in the \code{MCMCglmm} object. If \code{n} is missing, all the samples IDs are returned. Else, a random series of sample IDs are returned (with replacement if n greater than the number of available samples).
#'      \item \code{MCMCglmm.covars} returns a list of covariance matrices and intercepts from a \code{MCMCglmm} object (respectively from \code{MCMCglmm$VCV} and \code{MCMCglmm$Sol}). By default, all the covariance matrices and intercepts are returned but you can use either of the arguments \code{sample} to return specific samples (e.g. \code{MCMCglmm.covars(data, sample = c(1, 42))} for returning the first and 42nd samples) or \code{n} to return a specific number of random samples (e.g. \code{MCMCglmm.covars(data, n = 42)} for returning 42 random samples).
#'      \item \code{MCMCglmm.variance} returns a list of covariance matrices and intercepts from a \code{MCMCglmm} object (respectively from \code{MCMCglmm$VCV} and \code{MCMCglmm$Sol}). By default, all the covariance matrices and intercepts are returned but you can use either of the arguments \code{sample} to return specific samples (e.g. \code{MCMCglmm.covars(data, sample = c(1, 42))} for returning the first and 42nd samples) or \code{n} to return a specific number of random samples (e.g. \code{MCMCglmm.covars(data, n = 42)} for returning 42 random samples). 
#' }
#' 
#' @examples
#' ## Loading the charadriiformes model
#' data(charadriiformes)
#' model <- charadriiformes$posteriors
#' class(model) # is MCMCglmm
#' 
#' ## Get the list of levels from the model
#' MCMCglmm.levels(model)
#' ## The raw levels names (as they appear in the MCMCglmm object)
#' MCMCglmm.levels(model, convert = FALSE)
#' 
#' ## Get the traits names from the model
#' MCMCglmm.traits(model)
#' 
#' ## Get all the available samples in the model
#' length(MCMCglmm.sample(model))
#' ## Get 5 random sample IDs from the model
#' MCMCglmm.sample(model, n = 5)
#' 
#' ## Get one specific samples from the model
#' MCMCglmm.covars(model, sample = 42)
#' ## Get two random samples from the model
#' MCMCglmm.covars(model, n = 2)
    
## Get the variance for each terms in the model
# terms_variance <- MCMCglmm.variance(model)
# boxplot(terms_variance, horizontal = TRUE)

#' @seealso \code{\link{MCMCglmm.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

## Get the possible levels from a MCMCglmm
MCMCglmm.levels <- function(MCMCglmm, convert = TRUE) {
    convert.term.name <- function(one_term) {
        ## Return the term is simple, keep it like that
        if(length(grep(":", one_term)) == 0) {
            return(one_term)
        } else {
            ## Split the term 
            elements <- strsplit(one_term, ":")[[1]]
            ## Remove the trait component
            if(any(grep("trait", elements))) {
                elements <- elements[-grep("trait", elements)]
            }

            if(length(elements) == 1) {
                ## That's the element name
                return(gsub(" ", "", elements))
            } else {
                ## Remove spaces and get the most nested element
                return(gsub("_:", ":", paste(rev(unname(sapply(elements, function(X) gsub(" ", "_", gsub("[^[:alnum:] ]", "", rev(strsplit(X, "\\(")[[1]])[1]))))), collapse = ":")))
            }

        }
    }

    ## Sanitizing
    check.class(MCMCglmm, "MCMCglmm")
    check.class(convert, "logical")

    ## Get the random terms
    random_formula <- as.character(MCMCglmm$Random$formula[2])
    if(length(random_formula) == 0) {
        random_terms <- NULL
    } else {
        random_terms <- strsplit(random_formula, "\\+")[[1]]
    }
    
    ## Get the residuals terms
    residuals_formula <- as.character(MCMCglmm$Residual$formula[2])
    if(length(residuals_formula) == 0) {
        residual_terms <- NULL
    } else {
        residual_terms <- strsplit(residuals_formula, "\\+")[[1]]
    }

    ## No terms!
    if(is.null(random_terms) && is.null(residual_terms)) {
        return(c("residual" = "units"))
    }

    ## Clean the terms (remove spaces and \n)
    random_terms <- gsub(" $", "", gsub("^ ", "", gsub("  ", " ", gsub("    ", " ", gsub("\n", "", random_terms)))))
    residual_terms <- gsub(" $", "", gsub("^ ", "", gsub("  ", " ", gsub("    ", " ", gsub("\n", "", residual_terms)))))
    
    ## Convert the names to make them look fancy
    if(convert) {
        if(length(random_terms) != 0) {
            random_terms <- unname(sapply(random_terms, convert.term.name))
        }
        if(length(residual_terms) != 0) {
            residual_terms <- unname(sapply(residual_terms, convert.term.name))
        }
    }

    ## Get the terms names
    all_terms <- c(random_terms, residual_terms)
    names(all_terms) <- c(rep("random", length(random_terms)), rep("residual", length(residual_terms)))

    return(all_terms)
}

## Get the number of traits from a MCMCglmm
MCMCglmm.traits <- function(MCMCglmm) {

    ## Sanitizing
    check.class(MCMCglmm, "MCMCglmm")

    ## Get the variables
    variables <- as.character(MCMCglmm$Fixed$formula[2])

    ## Are the variables bunched in an expression (e.g. "c()")?
    if(any(grep("\\(", variables))) {
        ## Remove the brackets of the expression
        variables <- strsplit(strsplit(variables, "\\(")[[1]][2], "\\)")[[1]]
    }

    ## Are there multiple variables in that expression (e.g. ",")?
    if(any(grep(",", variables))) {
        ## Remove the commas and spaces
        variables <- gsub(" ", "", strsplit(variables, ",")[[1]])
    }

    return(variables)
}

## Get the samples from a MCMCglmm object
MCMCglmm.sample <- function(MCMCglmm, n) {

    ## Sanitizing
    check.class(MCMCglmm, "MCMCglmm")

    n_samples <- nrow(MCMCglmm$Sol)
    if(missing(n)) {
        return(1:n_samples)
    } else {
        check.class(n, c("numeric", "integer"))
        replace = n > n_samples
        if(replace) {
            warning(paste0("The required number of samples ", n, " is larger than the available number of samples ", n_samples, ". Some samples will be used more than once."))
        }
        return(sample(1:n_samples, n, replace = replace))
    }
}

## Get some covar matrices
MCMCglmm.covars <- function(MCMCglmm, n, sample){   

    ## Sanitizing
    check.class(MCMCglmm, "MCMCglmm")

    ## The number of traits
    traits <- MCMCglmm.traits(MCMCglmm)
    n_traits <- length(traits)
    ## The number of levels
    levels <- MCMCglmm.levels(MCMCglmm)
    n_levels <- length(levels)

    ## Check the samples
    if(missing(n)) {
        if(missing(sample)) {
            sample <- MCMCglmm.sample(MCMCglmm)
        } else {
            check.class(sample, c("numeric", "integer"))

            ## Check for incorrect samples
            if(length(incorect_sample <- which(sample > length(MCMCglmm.sample(MCMCglmm)))) > 0) {
                stop("Some samples are not available in the MCMCglmm object.", call. = FALSE)
            }
        }
    } else {
        check.class(n, c("numeric", "integer"))
        if(!missing(sample)) {
            #dispRity_export in: MAKE dispRity WARNING STYLE
            warning("sample argument is ignored since n = ", n, " random samples are asked for.")
        }
        sample <- MCMCglmm.sample(MCMCglmm, n)
    }

    ## Select the covar matrices
    covar_matrices <- unlist(lapply(as.list(sample), get.sample.covar, MCMCglmm, levels, traits), recursive = FALSE)

    ## Rearrange the list per levels
    results_out <- list()
    for(one_level in 1:n_levels) {
        results_out[[one_level]] <- unname(covar_matrices[which(names(covar_matrices) == levels[one_level])])
        names(results_out)[one_level] <- levels[one_level]
    } 
    return(results_out)
}

## Get the variance per VCV for each level
MCMCglmm.variance <- function(MCMCglmm, n, sample, levels, scale = TRUE) {
    
    match_call <- match.call()

    ## Sanitizing
    check.class(MCMCglmm, "MCMCglmm")
    check.class(scale, "logical")

    ## Extract sum of each VCV matrices
    VCV_sums <- lapply(MCMCglmm.covars(MCMCglmm, n, sample), lapply, function(x, what) sum(diag(x[[what]])), what = "VCV")

    ## Make that into a matrix
    model_variances <- matrix(unlist(VCV_sums), ncol = length(VCV_sums), byrow = FALSE)

    ## Extract only the relevant levels
    avail_levels <- MCMCglmm.levels(MCMCglmm, convert = TRUE)

    if(!missing(levels)) {
        ## Remove some levels
        level_class <- check.class(levels, c("numeric", "integer", "character"))
        if(level_class == "character") {
            if(!all(is_match <- levels %in% avail_levels)) {
                stop.call(msg.pre = paste0("The following level(s): ", paste0(levels[!is_match], collapse = ", "), " are not found in "), match_call$MCMCglmm, msg = ".")
            } else {
                levels <- match(levels, avail_levels)
            }
        }
        if(any(levels > length(avail_levels))) {
            stop.call(msg.pre = paste0("Only ", length(avail_levels), " levels (terms) are present in "), match_call$MCMCglmm, msg = ".")
        }
        avail_levels <- avail_levels[levels]
        model_variances <- model_variances[, levels]
    }

    ## Rename the levels columns
    colnames(model_variances) <- avail_levels

    if(scale){
        ## Scale the results
        return(model_variances/rowSums(model_variances))
    } else {
        return(model_variances)
    }
}