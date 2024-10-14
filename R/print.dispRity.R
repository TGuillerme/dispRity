#' @title Prints a \code{dispRity} object.
#'
#' @description Summarises the content of a \code{dispRity} object.
#'
#' @param x A \code{dispRity} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise its contents (\code{FALSE} - default).
#' @param ... further arguments to be passed to \code{print} or to \code{print.dispRity}.
#' 
#' @examples
#' ## Load the disparity data based on Beck & Lee 2014
#' data(disparity)
#' 
#' ## Displaying the summary of the object content
#' disparity
#' print(disparity) # the same
#' print.dispRity(disparity) # the same
#'
#' ## Displaying the full object
#' print.dispRity(disparity, all = TRUE)
#'
#' @seealso \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#'
#' @author Thomas Guillerme

## DEBUG
# warning("DEBUG dispRity.R")
# library(dispRity)
# source("sanitizing.R")
# source("dispRity.R")
# source("dispRity_fun.R")
# source("dispRity.metric.R")
# source("make.dispRity.R")
# source("fetch.dispRity.R")
# source("boot.matrix.R") ; source("boot.matrix_fun.R")
# source("chrono.subsets.R") ; source("chrono.subsets_fun.R")
# source("custom.subsets.R") ; source("custom.subsets_fun.R")
# data(BeckLee_mat50)
# data(BeckLee_tree)
# data_simple <- BeckLee_mat50
# data_boot <- boot.matrix(BeckLee_mat50, bootstraps = 11, rarefaction = c(5,6))
# data_subsets_simple <- chrono.subsets(BeckLee_mat50, tree = BeckLee_tree,  method = "discrete", time = c(120,80,40,20))
# data_subsets_boot <- boot.matrix(data_subsets_simple, bootstraps = 11, rarefaction = c(5,6))
# data <- dispRity(data_subsets_boot, metric = c(variances))

 
print.dispRity <- function(x, all = FALSE, ...) {

    match_call <- match.call()
    x_name <- match_call$x

    if(all) {
        ## Print everything
        tmp <- x
        class(tmp) <- "list"
        print(tmp)

    } else {

        ## ~~~~~~~
        ## Composite dispRity objects (subclasses)
        ## ~~~~~~~
        x_classes <- class(x)

        if(length(x_classes) > 1) {
            ## randtest
            switch(x_classes[[2]],
                randtest = {
                    ## Remove the call (messy)
                    remove.call <- function(element) {
                        if(length(grep("dispRity.randtest", element$call)) == 0) {
                            element$call <- "dispRity::null.test"
                        }
                        return(element)
                    }

                    x <- lapply(x, remove.call)

                    if(length(x) == 1) {
                        print(x[[1]])
                    } else {
                        tmp <- x
                        class(tmp) <- "list"
                        print(tmp) 
                    }
                    return(invisible())
                },
                model.test = {
                    cat("Disparity evolution model fitting:\n")

                    ## Check the model call (to avoid garbage collection with do.call)
                    if(class(x$call[[1]])[[1]] == "name") {
                        call_text <- as.expression(x$call)
                    } else {
                        call_text <- "model.test(...) # Unknown call trace"
                    }
                    cat(paste0("Call: ", call_text, " \n\n"))
                    
                    print(x$aic.models)

                    cat(paste0("\nUse x$full.details for displaying the models details\n"))
                    cat(paste0("or summary(x) for summarising them.\n"))

                    return(invisible())
                },
                model.sim = {
                    cat("Disparity evolution model simulation:\n")
                    cat(paste0("Call: ", as.expression(x$call), " \n\n"))
                    cat(paste0("Model simulated (", x$nsim, " times):\n"))

                    print(x$model)

                    cat("\n")

                    if(!is.null(x$p.value)) {
                        cat("Rank envelope test:\n")
                        cat(" p-value of the global test: ", attr(x$p.value, "p", exact = TRUE), sep="")
                        if(!is.null(attr(x$p.value, "ties"))) {
                            cat(" (ties method: ", attr(x$p.value, "ties"), ")\n", sep="")
                        } else {
                            cat("\n")
                        }
                        if(!is.null(attr(x$p.value, "p_interval"))) {
                            cat(" p-interval                : (", attr(x$p.value, "p_interval")[1], ", ", attr(x$p.value, "p_interval")[2],")\n", sep="")
                        }
                    }

                    return(invisible())
                },
                dtt = {
                    if(length(x) != 2){
                        ## Tested dtt
                        cat("Disparity-through-time test (modified from geiger::dtt)\n")
                        cat(paste0("Call: ", as.expression(x$call), " \n\n"))

                        cat(paste0("Observation: ", x$MDI , "\n\n"))

                        cat(paste0("Model: ", x$call$model , "\n"))
                        cat(paste0("Based on ", length(x$sim_MDI) , " replicates\n"))
                        cat(paste0("Simulated p-value: ", x$p_value , "\n"))
                        cat(paste0("Alternative hypothesis: ", x$call$alternative , "\n\n"))

                        print(c("Mean.dtt" = mean(x$dtt, na.rm = TRUE), "Mean.sim_MDI" = mean(x$sim_MDI, na.rm = TRUE), "var.sim_MDI" = var(x$sim_MDI, na.rm = TRUE)))

                        cat(paste0("\nUse plot.dispRity() to visualise."))
                        return(invisible())
                    } else {
                        ## raw dtt
                        ## Fake an object with no attributes
                        x_tmp <- x
                        class(x_tmp) <- "list"
                        print(x_tmp)
                        cat(paste0("- attr(*, \"class\") = \"dispRity\" \"dtt\"\n"))
                        cat(paste0("Use plot.dispRity to visualise."))
                        return(invisible())
                    }
                },
                test.metric = {
                    cat("Metric testing:\n")
                    cat(paste0("The following metric was tested: ", as.expression(x$call$metric), ".\n"))
                    cat(paste0("The test was run on "))
                    if(length(x$call$shifts) > 1) {
                        cat(paste0("the ", paste0(x$call$shifts, collapse = ", "), " shifts"))
                    } else {
                        cat(paste0("the ", x$call$shifts, " shift"))
                    }
                    cat(paste0(" for ", x$call$replicates, " replicate", ifelse(x$call$replicates > 1, "s", "")))
                    if(!is.null(x$models)) {
                        cat(paste0(" using the following model:\n"))
                        cat(as.character(as.expression(body(x$call$model))))
                    } else {
                        cat(paste0("."))
                    }
                    cat(paste0("\nUse summary(x) or plot(x) for more details."))
                    return(invisible())
                },
                axes = {
                    ## Get the groups names
                    groups <- names(x$dim.list)
                    ## Get the in_groups variable
                    in_groups <- if(length(groups) == 1 && groups[1] == "whole_space") {
                        "in the whole trait space"
                    } else {
                        if(length(groups) < 5) {
                            paste0("in the following groups: ", paste0(groups, collapse = ", "))
                        } else {
                            paste0("in the ", length(groups), " following groups: ", paste0(groups[1:5], collapse = ", "), ", ..")
                        }
                    }
                    cat(paste0(
                        ## N axes
                        "The first ", length(x$dimensions), " dimensions are needed to express ",
                        ## At least
                        ifelse(x$call$inc.threshold, "at least ", "up to "),
                        ## Percentage var
                        paste0(x$call$threshold*100, "% of the variance "),
                        ## In groups
                        in_groups, ".\n"))
                    cat(paste0("You can use x$dimensions to select them or use plot(x) and summary(x) to summarise them.\n"))
                    return(invisible())
                },
                projection = {
                    ## Print as a list
                    class(x) <- "list"
                    print(x)
                    return(invisible())
                },
                pgls.dispRity = {

                    ## Modified from phylolm::print.phylolm
                    ## Print the general info
                    cat(paste0("phylolm test (pgls) applied to ", length(x), " disparity estimates\n"))
                    cat(paste0("using the formula: ", Reduce(paste, deparse(x[[1]]$formula))," and the model: ", x[[1]]$model,"\n\n"))

                    ## Print fit
                    print(rbind(pool.pgls.param(x, "aic"), pool.pgls.param(x, "logLik")), ...)

                    ## Print param
                    cat("\nParameter estimate(s) using ML:\n")
                    if(!is.null(x[[1]]$optpar)) {
                        opt_param <- pool.pgls.param(x, "optpar")
                        if (x[[1]]$model %in% c("OUrandomRoot","OUfixedRoot")) {
                            rownames(opt_param) <- "alpha"
                            print(opt_param, ...)
                        }
                        if (x[[1]]$model %in% c("lambda","kappa","delta")) {
                            cat(x[[1]]$model,":")
                            print(opt_param, ...)
                        }
                        if (x[[1]]$model=="EB") {
                            rownames(opt_param) <- "rate"
                            print(opt_param, ...)
                        }
                    cat("\n")
                    }

                    ## Variance
                    print(pool.pgls.param(x, "sigma2"), ...)
                    if(x[[1]]$sigma2_error > 0) {
                        print(pool.pgls.param(x, "sigma2_error"), ...)
                    }
                    
                    ## Print the coefficients
                    cat("\nCoefficients:\n")
                    pool_coef <- pool.pgls.param(x, "coefficients")
                    # rownames(pool_coef) <- names(x[[1]]$coefficients)
                    print(pool_coef, ...)

                    cat(paste0("\nYou can access individual models by using their index (e.g. x[[1]])\nor summarise and plot all models using summary(x) or plot(x)."))
                    return(invisible())
                },
                multi = {
                    print.dispRity(dispRity.multi.merge.data(x), ...)
                    return(invisible())
                }
            )
        }

        
        ## ~~~~~~~
        ## Simple dispRity objects
        ## ~~~~~~~


        if(length(x$call) == 0) {
            if(!is.null(x$matrix) && is(x$matrix[[1]], "matrix")) {
                cat(" ---- dispRity object ---- \n")
                dims <- dim(x$matrix[[1]])
                n_matrices <- length(x$matrix)
                cat(paste0("Contains ", ifelse(n_matrices > 1, paste0(n_matrices, " matrices "), "a matrix "), dims[1], "x", dims[2], "."))
            } else {
                cat("Empty dispRity object.\n")
            }
            return()
        }

        cat(" ---- dispRity object ---- \n")

        ## Print the matrix information
        if(any(names(x$call) == "subsets") && length(x$subsets) != 1) {
            ## Get the number of subsets (minus origin)
            subsets <- names(x$subsets)

            ## Check if there is more than one subset
            if(length(subsets) != 1) {

                ## Get the method
                method <- x$call$subsets

                switch(method[1],
                    "discrete"   = cat(paste(length(subsets), method[1], "time subsets for", nrow(x$matrix[[1]]), "elements")),
                    "continuous" = cat(paste(length(subsets), paste(method[1], " (", method[2],")", sep = ""), "time subsets for", nrow(x$matrix[[1]]), "elements")),
                    "customised" = cat(paste(length(subsets), method[1], "subsets for", nrow(x$matrix[[1]]), "elements")),
                    "covar"      = cat(paste(length(subsets), method[1], "subsets for", nrow(x$matrix[[1]]), "elements"))
                    )

                ## Print the number of matrices
                if(length(x$matrix) > 1) {
                    cat(paste0(" in ", length(x$matrix), ifelse((!is.null(x$call$dispRity.multi) && x$call$dispRity.multi), " separated", ""), " matrices"), sep = "")
                } else {
                    cat(paste0(" in one matrix"), sep = "")
                }
                if(length(x$call$dimensions) != 0) cat(paste(" with", length(x$call$dimensions), "dimensions"), sep = "")
                
                ## Print the number of trees
                if(!is.null(x$tree[[1]])) {
                    cat(" with ") ; print(x$tree)
                } else {
                    cat(":\n")
                }

                if(length(subsets) > 5) {
                    cat("    ",paste(subsets[1:5], collapse=", "),"...\n")
                } else {
                    cat("    ",paste(subsets, collapse=", "), ".\n", sep="")
                }
            }
        } else {

            ## Covar matrices
            if(!is.null(x$call$subsets) && ("covar" %in% x$call$subsets)) {
                cat(paste0("One covar matrix (", names(x$subsets), ") with "))
            }
            cat(paste(nrow(x$matrix[[1]]), "elements"))
            if(length(x$matrix) > 1) {
                cat(paste0(" in ", length(x$matrix), ifelse((!is.null(x$call$dispRity.multi) && x$call$dispRity.multi), " separated", ""), " matrices"), sep = "")
            } else {
                cat(paste0(" in one matrix"), sep = "")
            }
            if(length(x$call$dimensions) != 0) cat(paste(" with", length(x$call$dimensions), "dimensions"), sep = "")
            if(!is.null(x$tree[[1]])) {cat(" with ") ; print(x$tree)} else {cat(".\n")}
        }
    
        ## Print the bootstrap information
        if(any(names(x$call) == "bootstrap")) {
            if(x$call$bootstrap[[1]] != 0) {
                if(x$call$bootstrap[[2]] == "covar") {
                    cat(paste0("Data is based on ", length(x$covar[[1]]), " posterior sample", ifelse(length(x$covar[[1]]) > 1, "s","")))
                } else {
                    cat(paste(ifelse(x$call$bootstrap[[4]] == "both", "Rows and columns",paste(toupper(substr(x$call$bootstrap[[4]], 1, 1)), substr(x$call$bootstrap[[4]], 2, nchar(x$call$bootstrap[[4]])), sep="")), " were bootstrapped ", x$call$bootstrap[[1]], " times (method:\"", x$call$bootstrap[[2]], "\")", sep = ""))
                }
            }
            if(!is.null(x$call$bootstrap[[3]])) {
                if(x$call$bootstrap[[3]][[1]] == "full") {
                    cat(" and fully rarefied")
                } else {
                    cat(paste(" and rarefied to ", paste(x$call$bootstrap[[3]], collapse = ", "), " elements", sep = ""))
                }
            }
            cat(".\n")
        }

        ## Print the disparity information
        if(any(names(x$call) == "disparity")) {
            cat(paste("Disparity was calculated as:", paste(as.character(x$call$disparity$metrics$name), collapse = ", ")))
            if(x$call$disparity$metrics$between.groups) {
                cat(" between groups")
            }

            ## Print info from dispRitreats
            if(!is.null(x$call$dispRitreats)) {
                ## PLACEHOLDER FOR dispRitreats INFO
                if(x$call$dispRitreats) {
                    cat(".\nDisparity was calculated from treats simulated data")
                }
            }

            cat(".\n")
        }
    }
}