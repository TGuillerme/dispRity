## Checking number of elements in each group in the data.frame
check.elements.data.frame <- function(group) {
    any(table(as.factor(group)) < 3)
}

## Splitting element function
split.elements.data.frame <- function(one_group, data) {

    ## lapply fun for selecting the elements
    select.elements <- function(subsamples, one_group) {
        return(list("elements" = as.matrix(which(as.character(one_group) == as.character(levels(as.factor(one_group))[[subsamples]])))))
    }

    ## Select the elements per subsamples
    selected_elements <- lapply(as.list(1:length(levels(as.factor(one_group)))), select.elements, one_group)

    ## Adding the names to the subsamples
    names(selected_elements) <- levels(as.factor(one_group))

    ## Output
    return(selected_elements)
}

## Convert row names in row numbers
convert.name.to.numbers <- function(one_group, data) {
    return(match(one_group, rownames(data)))
}