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

}

## sanitizing function for changepoint + time.window
check.time <- function(object, class, msg, type, tree) {
    # match_call <- match.call()

    # class_object <- class(object)[1]
    # length_class <- length(class)
    if (type == "changepoint"){
        check.class(object, c("numeric", "integer", "character"))
        check.length(object, length = 1, paste(" argument must be a single numeric/integer or character string: `detect`.\n"))
        if (class(object) == "numeric" || class(object) == "integer") {
            tree_span <- c(max(node.depth.edgelength(tree)), min(node.depth.edgelength(tree)))
            if(!object < tree_span[1] && object > tree_span[2]) {
                stop()
            }
        }
    }


}