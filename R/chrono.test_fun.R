## note that changepoint is TIME BEFORE PRESENT (i.e Ma geological time)


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


make.deltatronic <- function(data, changepoint, concatenate = TRUE) {
    disp_vals <- t(as.data.frame(get.disparity(data, concatenate = as.logical(concatenate)), check.names = FALSE))
    colnames(disp_vals) <- "disparity"
    numeric_time <- as.numeric(rownames(disp_vals))

    delta_df <- data.frame(
        time_elapsed =  max(numeric_time) - numeric_time,
        disparity = as.numeric(disp_vals[,"disparity"]),
        impact = as.numeric(numeric_time <= changepoint)
    )

    delta_df$time_post_cp <- ifelse(delta_df$impact == 0, 0, delta_df$time_elapsed - changepoint)

    return(delta_df)
}

set.time.window <- function(tree, time.window) {
    if(class(time.window) ==  "numeric" && length(time.window) == 1 && time.window >= 1) {

    }
}

