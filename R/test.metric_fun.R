# ## Run the reduction for one type
# reduce.space.one.type <- function(type, data, steps, shift.options, verbose) {

#     ## Setting up the reduce space call (verbose)
#     reduce.space.call <- reduce.space
#     if(verbose) {
#         body(reduce.space.call)[[2]] <- substitute(message(".", appendLF = FALSE))
#     }

#     ## Reduce the matrix
#     make.reduce.space.args <- function(data, type, shift.options) {

#         ## Default arguments
#         args_list <- list("space" = data,
#                           "type" = type,
#                           "verbose" = FALSE,
#                           "return.optim" = FALSE)

#         ## Optional arguments
#         if(!is.null(shift.options$parameters)) {
#             args_list <- c(args_list, "parameters" = shift.options$parameters)
#         }
#         if(!is.null(shift.options$tuning)) {
#             args_list <- c(args_list, "tuning" = shift.options$tuning)
#         }

#         return(args_list)
#     }
#     ## Add the shifts
#     add.steps.to.args <- function(type_args, steps) {
#         return(lapply(steps, function(x, args) return(c(args, "remove" = x)), type_args))
#     }

#     # print("ping")

#     ## Run the reductions
#     reductions <- lapply(add.steps.to.args(make.reduce.space.args(data[[1]], type, shift.options), steps), function(args, fun) do.call(fun, args), fun = reduce.space.call)
#     reductions <- lapply(reductions, function(x, matrix) return(rownames(matrix)[!x]), data[[1]])
#     names(reductions) <- as.character(steps)

#     ## Make the dispRity objects
#     return(custom.subsets(data, group = reductions))
# }

# ## Getting the disparity
# get.reduced.dispRity <- function(reduction, metric, dimensions, verbose, ...) {
    
#     ## Duplicate function for verbose
#     dispRity.verbose <- dispRity
#     if(verbose) {
#         ## Remake the function verbose
#         body(dispRity.verbose)[[16]] <- substitute(silent <- "silent")
#         body(dispRity.verbose)[[18]] <- substitute(silent <- "silent")
#     }

#     ## Run the disparity
#     return(dispRity.verbose(data = reduction, metric = metric, ..., dimensions = dimensions, verbose = verbose)$disparity)
#     # return(dispRity.verbose(data = reduction, metric = metric, dimensions = dimensions, verbose = verbose)$disparity)
# }

# ## Transforming the tables
# make.reduction.tables <- function(one_type_results) {
#     disparity_values <- lapply(one_type_results, function(x) unname(unlist(x)))
#     return(data.frame("disparity" = unname(unlist(disparity_values)),
#                       "reduction" = rep(names(disparity_values), unlist(lapply(disparity_values, length)))))
# }
