chrono.test <- function(data, method, changepoint, time.window, ...) {
    match_call <- match.call()
    ##############
    # SANITISING #
    ##############

    ## start with data
    check.class(data, "dispRity") ## do all the classic dispRity checking, is_multi etc, make sure it has a tree and a matrix.
    mat <- get.matrix(data)
    tree <- get.tree(data)



    check.time(changepoint, c("numeric", "integer", ))
    check.length(changepoint, 1, " must be a single numeric value.")
    # check.class(replicates, c("numeric"))
    # check.length(replicates, 1, " must be a single numeric value.")
    methods <- c("itsa", "citsa", "area", "h.test")
    check.method(slice.model, slice_models, "slice.model argument")



    #######################################################################################################

    

    delta_df <- make.deltatronic(data, changepoint)

    if(!is.null(time.window)) {
        delta_df <- set.time.window(delta_df, time.window, changepoint)
    }


    






}

## sanitizing function for changepoint + time.window
