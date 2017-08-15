## Transform list of factors into a factor list for custom.subsamples
make.groups.factors <- function(one_factor_list) {

    ## Separating the list for each level
    data_list <- data.frame(seq_along(one_factor_list), one_factor_list)
    data_list <- lapply(split(data_list, data_list[[2]]), function(X) return(X[,1]))

    return(data_list)
}