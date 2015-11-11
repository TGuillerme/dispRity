#flip tests for referential lapply (x becomes fix variable and y becomes list)
flip.ref.lapply <- function(series, referential, test, ...) {
    output <- test(referential, series, ...)
    return(output)
}

#transforming test into a mapply loop
test.pair.lapply <- function(list_of_comp, data, test, ...) {
    output <- test(data[[list_of_comp[[1]]]], data[[list_of_comp[[2]]]], ...)
    return(output)
}