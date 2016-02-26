#Get specific elements from call
get.from.call <- function(data, what) {
    if(what == "metric") {
        call_out <- strsplit(strsplit(data$call, split = "Disparity calculated as: ")[[1]][[2]], split = " for ")[[1]][[1]]
    }
    if(what == "dimensions") {
        call_out <- strsplit(strsplit(data$call, split = " for ")[[1]][[2]], split = " dimensions")[[1]][[1]]
    }
    return(eval(parse(text = call_out)))
}
