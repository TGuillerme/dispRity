#' @title MCMCglmm.subsets
#'
#' @description Creating a dispRity object from a MCMCglmm posterior output
#'
#' @param data The \code{data.frame} or \code{matrix} used for the \code{\link[MCMCglmm]{MCMCglmm}} model.
#' @param posteriors A \code{\link[MCMCglmm]{MCMCglmm}} object, the posteriors of the model.
#' @param group Optional, a named vector of which group to include from the posteriors (if left empty the random and residual terms are used). See details.
#' @param tree Optional, the tree(s) used in the MCMCglmm analyses.
#' @param rename.groups optional, a vector of group names for renaming them. See details.
#' @param ... Optional arguments to be passed to \code{\link{MCMCglmm.covars}}.
#' 
#' @details
#' \itemize{
#'      \item For the \code{group} option, the group names must be ones found in the \code{posteriors} formula in the format \emph{<Type = Term:FactorLevel>} as returned by \code{MCMCglmm.levels(posteriors)}. For example, for returning two random effect, the phylogenetic one (\code{"animal"}) and one for a specific clade (say the 2nd clade) as well as two residual terms for a specific factor (say level 1 and 4) you can use \code{group = c(random = "animal", random = "animal:clade2", residual = "units:myfactor1", residual = "units:myfactor4")}.
#'      \item For the \code{rename.groups} option, the vector must be of class \code{"character"} and must of the same length as the number of random and residual terms in \code{posteriors} or of \code{group} argument (if used). If the \code{group} argument is left empty, the groups are extracted from the \code{posteriors} in the following order: the random terms first then the residual terms as specified in the \code{posteriors} object formulas (respectively \code{posteriors$Random$formula} and \code{posteriors$Residual$formula}).
#' }
#' 
#' \emph{NOTE} that the output \code{dispRity} inherits the dimensions used in the \code{posteriors} argument. You can always check the selected dimensions using:
#' \code{data$call$dimensions}
#' 
#' @examples
#' data(charadriiformes)
#' 
#' ## Creating a dispRity object from the charadriiformes model
#' MCMCglmm.subsets(data       = charadriiformes$data,
#'                  posteriors = charadriiformes$posteriors)
#' 
#' ## Same but selecting only the three first random terms
#' MCMCglmm.subsets(data       = charadriiformes$data,
#'                  posteriors = charadriiformes$posteriors,
#'                  tree       = charadriiformes$tree,
#'                  group      = MCMCglmm.levels(
#'                                  charadriiformes$posteriors)[1:3],
#'                  rename.groups = c("gulls", "plovers", "sandpipers"))
#' 
#'
#' @seealso \code{\link{dispRity}}  \code{\link{covar.plot}}
#' 
#' @author Thomas Guillerme
#' @export

MCMCglmm.subsets <- function(data, posteriors, group, tree, rename.groups, ...) {

    match_call <- match.call()

    ## Cleaning then checking the data (i.e. only removing the values)
    data_class <- check.class(data, c("data.frame", "matrix"))
    ## Sorting the data
    if(data_class == "data.frame") {
        ## Remove potential non-numeric columns
        col_classes <- sapply(1:ncol(data), function(col, dat) class(data[,col]), dat = data)
        numerics <- (col_classes %in% c("numeric", "integer"))
        ## Check for non-numerics
        if(!any(numerics)) {
            stop.call(msg = " does not contain any column with numeric or integer values.", call = match_call)
        }
        ## Clean the data
        cleaned_data <- as.matrix(data[,numerics])
        ## Is there any classification column?
        classifier <- col_classes[!numerics] %in% "factor"
        if(any(classifier)) {
            group_classifier <- data[,which(!numerics)[which(classifier)], drop = FALSE]
        }
    }
    
    ## Checking the posteriors
    check.class(posteriors, "MCMCglmm")

    ## Check which dimensions where used
    dimensions <- match(MCMCglmm.traits(posteriors), colnames(cleaned_data))

    ## Extracting the residuals and randoms
    posterior_levels <- MCMCglmm.levels(posteriors)
    posterior_terms <- lapply(posterior_levels, split.term.name)
    
    ## Extracting the group from the posteriors
    extracted_group <- lapply(posterior_terms, get.one.group, group_classifier, elements = rownames(cleaned_data))
    names(extracted_group) <- posterior_levels

    ## Setting the groups
    if(missing(group)) {
        subsets <- extracted_group
        selected_groups <- seq(1:length(extracted_group))
    } else {
        if(is.null(names(group))) {
            stop.call(msg = "The group argument must be a named vector.", call = "")
        } else {
            if(!any(is.na(selected_groups <- match(group, posterior_levels)))) {
                ## Select only the specific groups
                subsets <- extracted_group[selected_groups]
            } else {
                ## Empty groups
                nas <- which(is.na(selected_groups))
                stop.call(msg = paste0("The following group", ifelse(length(nas) == 1, "", "s"), " cannot be found in the posteriors: ", paste0(group[nas], collapse = ", "), ".\nCheck MCMCglmm(posteriors) for the available group names."), call = "")
            }
        }
    }

    ## Getting the covar matrices per group
    covar_matrices <- MCMCglmm.covars(posteriors, ...)[selected_groups]

    ## Renaming the groups
    if(!missing(rename.groups)) {
        if(length(rename.groups) != length(subsets)) {
            stop.call(msg = paste0("The rename.groups argument must the same length as group argument (", length(selected_groups), ")."), call = "")
        }
        names(subsets) <- names(covar_matrices) <- rename.groups
    }

    ## Create a dispRity style object
    output <- dispRity::make.dispRity(data = cleaned_data, call = list("subsets" = "covar", "dimensions" = dimensions), subsets = subsets)
    ## Add the covar element
    output$covar <- covar_matrices
    ## Update the call (bootstrap part)
    output$call$bootstrap <- list(length(MCMCglmm.sample(posteriors)), "covar", NULL)

    ## Adding the tree(s)
    if(!missing(tree)) {
        return(add.tree(output, tree))
    } else {
        return(output)
    }

}
