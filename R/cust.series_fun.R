## Splitting element function
split.elements <- function(one_factor, data) {

    ## lapply fun for selecting the elements
    select.elements <- function(series, one_factor) {
        return(list("elements" = which(as.character(one_factor) == as.character(levels(as.factor(one_factor))[[series]]))))
    }

    ## Select the elements per series
    selected_elements <- lapply(as.list(1:length(levels(as.factor(one_factor)))), select.elements, one_factor)

    ## Adding the names to the series
    names(selected_elements) <- levels(as.factor(one_factor))

    ## Output
    return(selected_elements)
}