## create the group list
get.one.group <- function(one_term, group_classifier, elements) {
    ## Animal term (phylogeny)
    if(is.null(one_term$factor) && is.null(one_term$level)) {
        ## The group is the full phylogeny (dispRity format)
        #return(1:length(elements))
        return(list(elements = matrix(1:length(elements), ncol = 1)))
    } else {

        ## Find if the factor matches with the MCMCglmm output (some characters - like " " or "_" get removed by MCMCglmm without warning. Boo!)
        group_classifier_names <- colnames(group_classifier)
        if(!(one_term$factor %in% group_classifier_names)) {
            group_classifier_names <- gsub(" ", "", gsub("_", "", group_classifier_names))
            ## Rename the columns for the classifier potentially as output from MCMCglmm
            # if(one_term$factor %in% group_classifier_names) {
                colnames(group_classifier) <- group_classifier_names
            # } else {
                # stop("DEBUG")
            # }
        }

        ## Get the factor in group_classifier (dispRity format)
        #return(which(group_classifier[,one_term$factor] == levels(group_classifier[,one_term$factor])[one_term$level]))
        return(list(elements = matrix(which(group_classifier[, one_term$factor] == levels(group_classifier[,one_term$factor])[one_term$level]), ncol = 1)))
    }
}

## Splitting a term name
split.term.name <- function(one_term) {
    ## Initialise the factor and level
    factor <- level <- NULL
    ## Split the term
    split_term <- strsplit(one_term, ":")[[1]]
    ## Get the second part of the term
    if(length(split_term) > 1) {
        splited <- strsplit(split_term[[2]], "_")[[1]]
        level   <- as.numeric(splited[2])
        factor  <- splited[1]
        # level  <- as.numeric(gsub(".*?([[:digit:]]+)", "\\1", split_term[[2]]))
        # factor <- gsub("_", "", as.character(gsub(level, "", split_term[[2]])))
    }
    return(list(term   = split_term[[1]],
                factor = factor,
                level  = level))
}