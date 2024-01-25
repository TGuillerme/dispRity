#' @title Separating data in chronological subsets.
#' @aliases time.series time.subsets
#'
#' @description Splits the data into a chronological (time) subsets list.
#'
#' @param data A \code{matrix} or a \code{list} of matrices.
#' @param tree \code{NULL} (default) or an optional \code{phylo} or \code{multiPhylo} object matching the data and with a \code{root.time} element. This argument can be left missing if \code{method = "discrete"} and all elements are present in the optional \code{FADLAD} argument.
#' @param method The time subsampling method: either \code{"discrete"} (or \code{"d"}) or \code{"continuous"} (or \code{"c"}).
#' @param time Either a single \code{integer} for the number of discrete or continuous samples; a \code{vector} containing the age of each sample; or, if \code{method = "discrete"} a character in the format \code{"n.elements"} where n is the minimum number of elements per bin (see details). 
#' @param model One of the following models: \code{"acctran"}, \code{"deltran"}, \code{"random"}, \code{"proximity"}, \code{"equal.split"} or \code{"gradual.split"}. Is ignored if \code{method = "discrete"}.
#' @param inc.nodes A \code{logical} value indicating whether nodes should be included in the time subsets. Is ignored if \code{method = "continuous"}.
#' @param FADLAD \code{NULL} (default) or an optional \code{data.frame} or \code{list} of \code{data.frame}s containing the first and last occurrence data.
#' @param verbose A \code{logical} value indicating whether to be verbose or not. Is ignored if \code{method = "discrete"}.
#' @param t0 If \code{time} is a number of samples, whether to start the sampling from the \code{tree$root.time} (\code{TRUE}), or from the first sample containing at least three elements (\code{FALSE} - default) or from a fixed time point (if \code{t0} is a single \code{numeric} value).
#' @param bind.data If \code{data} contains multiple matrices and \code{tree} contains the same number of trees, whether to bind the pairs of matrices and the trees (\code{TRUE}) or not (\code{FALSE} - default).
#' 
#' 
#' 
#'  
#' @details
#' The data is considered as the multidimensional space with rows as elements and columns as dimensions and is not transformed (e.g. if ordinated with negative eigen values, no correction is applied to the matrix).
#' 
#' If \code{method = "continuous"} and when the sampling is done along an edge of the tree, the data selected for the time subsets can be one of the following:
#' \itemize{
#'      \item Punctuated models:
#'      \itemize{
#'         \item \code{"acctran"}: always selecting the value from the ancestral node.
#'         \item \code{"deltran"}: always selecting the value from the descendant node or tip.
#'         \item \code{"random"}: randomly selecting between the ancestral node or the descendant node/tip.
#'         \item \code{"proximity"}: selecting the ancestral node or the descendant node/tip with a probability relative to branch length.
#'      }
#'      \item Gradual models:
#'      \itemize{
#'          \item \code{"equal.split"}: randomly selecting from the ancestral node or the descendant node or tip with a 50\% probability each.
#'          \item \code{"gradual.split"}: selecting the ancestral node or the descendant with a probability relative to branch length.
#'      }
#' }
#' N.B. \code{"equal.split"} and \code{"gradual.split"} differ from the punctuated models by outputting a node/tip probability table rather than simply the node and the tip selected. In other words, when bootstrapping using \code{\link{boot.matrix}}, the two former models will properly integrate the probability to the bootstrap procedure (i.e. different tips/nodes can be drawn) and the two latter models will only use the one node/tip determined by the model before the bootstrapping.
#'
#' If \code{method = "discrete"}, the \code{time} argument can be set to be a minimum number of elements per bin. For example if you need time bins with at least 15 elements in them, you can use \code{time = "15.elements"}. The algorithm will then start from the root and try to split the data in order to contain that number of elements. This results in non-equal time bins and the last time bin contains usually more than the number of required elements (e.g. if the tree is ultrametric, the last time bin contains all the living tips).
#'
#' @references
#' Guillerme T. & Cooper N. \bold{2018}. Time for a rethink: time sub-sampling methods in disparity-through-time analyses. Palaeontology. DOI: 10.1111/pala.12364.
#' 
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_tree) ; data(BeckLee_mat50)
#' data(BeckLee_mat99) ; data(BeckLee_ages)
#'
#' ## Time binning (discrete method)
#' ## Generate two discrete time bins from 120 to 40 Ma every 40 Ma
#' chrono.subsets(data = BeckLee_mat50, tree = BeckLee_tree, method = "discrete",
#'      time = c(120, 80, 40), inc.nodes = FALSE, FADLAD = BeckLee_ages)
#' ## Generate the same time bins but including nodes
#' chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree, method = "discrete",
#'      time = c(120, 80, 40), inc.nodes = TRUE, FADLAD = BeckLee_ages)
#'
#' ## Time slicing (continuous method)
#' ## Generate five equidistant time slices in the dataset assuming a proximity
#' ## evolutionary model
#' chrono.subsets(data = BeckLee_mat99, tree = BeckLee_tree,
#'      method = "continuous", model = "acctran", time = 5,
#'      FADLAD = BeckLee_ages)
#'
#' @seealso \code{\link{tree.age}}, \code{\link{slice.tree}}, \code{\link{cust.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' @author Thomas Guillerme

##Testing
# warning("DEBUG chrono.subsets")
# source("sanitizing.R")
# source("chrono.subsets_fun.R")
# source("slice.tree_fun.R")
# data(BeckLee_tree) ; data(BeckLee_mat50)
# data(BeckLee_mat99) ; data(BeckLee_ages)
# data = BeckLee_mat50
# tree = BeckLee_tree
# method = "continuous"
# model = "acctran"
# time = 5
# inc.nodes = TRUE
# FADLAD = BeckLee_ages
# verbose <- TRUE
# t0 <- FALSE


# plot(BeckLee_tree, cex = 0.5)
# nodelabels(BeckLee_tree$node.label, cex = 0.5)
# axisPhylo()
# abline(v = 40)

# # DEBUG LOAD FROM TEST
# method = "continuous"
# time = 3
# model = "gradual.split"
# inc.nodes = TRUE
# verbose = FALSE
# t0 = 5
# bind.data = TRUE

chrono.subsets <- function(data, tree = NULL, method, time, model, inc.nodes = FALSE, FADLAD = NULL, verbose = FALSE, t0 = FALSE, bind.data = FALSE) {
    match_call <- match.call()

    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    # data <- check.dispRity.data(data, returns = "matrix")

    if(!is.null(tree)) {
        data <- check.dispRity.data(data, tree, returns = c("matrix", "tree", "multi"))
    } else {
        data <- check.dispRity.data(data, returns = c("matrix", "multi"))
    }

    ## VERBOSE
    check.class(verbose, "logical")

    # If is multi lapply the stuff
    if(((!is.null(data$call$dispRity.multi) && data$call$dispRity.multi) || data$multi)) {
        ## Split the data
        split_data <- dispRity.multi.split(data)
        
        ## Get only the matrices and/or the trees
        matrices <- unlist(lapply(split_data, `[[`, "matrix"), recursive = FALSE)
        
        ## Get the trees
        if(!is.null(split_data[[1]]$tree)) {
            tree <- unlist(lapply(split_data, `[[`, "tree"), recursive = FALSE)
        } else {
            tree <- NULL
        }

        ## Toggle bind data (each is now a pair of matrix + tree)
        bind.data <- FALSE

        ## Toggle verbose (if required)
        chrono.subsets.call <- chrono.subsets
        if(verbose) {
            ## Changing the chrono.subsets function name (verbose line edited out)
            ## Find the verbose lines
            start_verbose <- which(as.character(body(chrono.subsets.call)) == "if (method == \"discrete\") {\n    chrono.subsets.fun <- chrono.subsets.discrete\n    if (verbose) \n        message(\"Creating \", length(time) - 1, \" time bins through time:\", appendLF = FALSE)\n} else {\n    chrono.subsets.fun <- chrono.subsets.continuous\n    if (verbose) \n        message(\"Creating \", length(time), \" time samples through \", ifelse(length(tree) > 1, paste0(length(tree), \" trees:\"), \"one tree:\"), appendLF = FALSE)\n}")
            end_verbose <- which(as.character(body(chrono.subsets.call)) == "if (verbose) message(\"Done.\\n\", appendLF = FALSE)")

            ## Blank out the lines
            body(chrono.subsets.call)[[start_verbose]][[3]][[3]] <- body(chrono.subsets.call)[[start_verbose]][[4]][[3]] <- body(chrono.subsets.call)[[end_verbose]] <- substitute(empty_line <- NULL)
        }

        ## Apply the custom.subsets
        if(method == "discrete") {
            if(verbose) message("Creating ", length(time)-1, " time bins through time:", appendLF = FALSE)
        } else {
            if(verbose) message("Creating ", length(time), " time samples through ", length(matrices), " trees and matrices:", appendLF = FALSE)
        }

        output <- dispRity.multi.apply(matrices, fun = chrono.subsets.call, tree = tree, method = method, time = time, model = model, inc.nodes = inc.nodes, FADLAD = FADLAD, verbose = verbose, t0 = t0, bind.data = bind.data)

        if(verbose) message("Done.\n", appendLF = FALSE)
        return(output)

    } else {
        if(!is.null(tree)) {
            tree <- data$tree
        }
        data <- data$matrix
    }
    
    ## Check whether it is a distance matrix
    if(check.dist.matrix(data[[1]], just.check = TRUE)) {
        warning("chrono.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!", call. = FALSE)
    }

    ## nrow_data variable declaration
    nrow_data <- nrow(data[[1]])

    ## TREE (1)
    ## tree must be a phylo object
    if(!is.null(tree)) {
        tree_class <- check.class(tree, c("phylo", "multiPhylo"))
        is_multiPhylo <- ifelse((tree_class == "multiPhylo" && length(tree) > 1), TRUE, FALSE)
        ## Make the tree into a single multiPhylo object

        ## Check if all the trees are the same
        tips <- lapply(tree, function(x) x$tip.label)
        if(!all(unique(unlist(tips)) %in% tips[[1]])) {
            stop.call(match_call$tree, msg.pre = "The trees in ", msg = " must have the same tip labels.")
        }
        if(inc.nodes) {
            nodes <- lapply(tree, function(x) x$node.label)
            unique_nodes <- unique(unlist(nodes))
            if(is.null(unique_nodes) || !all(unique_nodes %in% nodes[[1]])) {
                stop.call(match_call$tree, msg.pre = "The trees in ", msg = " must have the same node labels.")
            }
        }

        ## tree must be dated
        if(is_multiPhylo) {
            ## Check the root times
            root_time <- check.list(tree, function(x) return(ifelse(is.null(x$root.time), NA, x$root.time)))
            
            ## Check for missing root times
            if(any(is.na(root_time))) {
                stop.call(match_call$tree, msg.pre = paste0("The following tree(s) in "), paste0(" ", paste0(seq(1:length(root_time))[is.na(root_time)], collapse = ", "), msg = " needs a $root.time element."))
            } else {
                ## Check for different root time
                if(length(unique(root_time)) != 1) {
                    ## Make all the root times identical
                    stretch.tree <- function(tree, root) {
                        ## Find the root edges
                        root_edges <- which(tree$edge[,1] == Ntip(tree)+1)
                        ## Stretch the edges
                        tree$edge.length[root_edges] <- tree$edge.length[root_edges] + (root - tree$root.time)
                        ## Update the root time
                        tree$root.time <- root
                        ## Done
                        return(tree)
                    }

                    tree <- lapply(tree, stretch.tree, root = max(root_time))
                    class(tree) <- "multiPhylo"
                    warning(paste0("Differing root times in ", as.expression(match_call$tree), ". The $root.time for all tree has been set to the maximum (oldest) root time: ", max(root_time), " by stretching the root edge."))
                }
            }
        } else {
            ## Check the single root time
            if(is.null(tree[[1]]$root.time)) {
                stop.call(match_call$tree, paste0(" must be a dated tree with a $root.time element. Try using:\n    ", as.expression(match_call$tree), "$root.time <- the_age_of_the_root"))
            }
        }

        ## tree.age_tree variable declaration
        tree.age_tree <- lapply(tree, tree.age)
    } else {
        ## Default tree list
        is_multiPhylo <- FALSE
    }

    ## METHOD
    all_methods <- c("discrete", "d", "continuous", "c")
    ## method must be a character string
    check.class(method, "character")
    ## method must have only one element
    check.length(method, 1, paste(" argument must be one of the following: ", paste(all_methods, collapse = ", "), ".", sep = ""))
    ## method must be either "discrete", "d", "continuous", or "c"
    check.method(method, all_methods, "method argument")

    ## if method is "d" or "c", change it to "discrete" or "continuous" (lazy people...)
    if(method == "d") method <- "discrete"
    if(method == "c") method <- "continuous"

    ## If the tree is missing, the method can intake a star tree (i.e. no phylogeny)
    if(is.null(tree)) {
        if(is.null(FADLAD)) {
            stop.call("", "If no phylogeny is provided, all elements must be present in the FADLAD argument.")
        }
        if(method == "continuous") {
            stop.call("", "If no phylogeny is provided, method must be \"discrete\".")
        }
        tree_was_missing <- TRUE

        ## Checking FADLAD disponibilities
        names_data <- rownames(data[[1]])
        ## All names must be present in the data
        if(!all(names_data %in% rownames(FADLAD))) {
            stop.call("", "If no phylogeny is provided, all elements must be present in the FADLAD argument.")
        }
        ## Generating the star tree
        tree <- stree(nrow_data, tip.label = names_data)
        tree$root.time <- max(FADLAD)
        tree$edge.length <- FADLAD$FAD
        tree <- list(tree)
        class(tree) <- "multiPhylo"
    } else {
        tree_was_missing <- FALSE
    }

    ## Ntip_tree variable declaration and check
    Ntip_tree <- Ntip(tree)
    if(any(wrong_ntip <- !(Ntip_tree %in% Ntip_tree[[1]]))) {
        stop.call(match_call$tree, paste0(": wrong number of tips in the following tree(s): ", paste0(c(1:length(wrong_ntip))[wrong_ntip], collapse = ", "), "."))
    } else {
        Ntip_tree <- Ntip_tree[1]
    }

    ## MODEL
    ## if method is discrete ignore model
    if(method == "discrete") {
        model <- NULL
    } else {
        ## else model must be one of the following
        model <- tolower(model)
        all_models <- c("acctran", "deltran", "random", "proximity", "equal.split", "gradual.split")
        check.class(model, "character")
        check.length(model, 1, paste(" argument must be one of the following: ", paste(all_models, collapse = ", "), ".", sep = ""))
        check.method(model, all_models, "model argument")
    }

    ## INC.NODES
    if(method != "continuous") {
        ## else inc.nodes must be logical
        check.class(inc.nodes, "logical")
        if(tree_was_missing && inc.nodes) {
            stop.call("", "If no phylogeny is provided, inc.nodes must be FALSE.")
        }
    } else {
        ## Include nodes is mandatory
        inc.nodes <- TRUE
    }

    ## t0
    if(!is(t0, "logical")) {
        silent <- check.class(t0, c("numeric", "integer"), msg = " must be logical or a single numeric value.")
        check.length(t0, 1, errorif = FALSE, msg = " must be logical or a single numeric value.")
    } 

    ## TIME
    ## time must be numeric or integer
    time_class <- check.class(time, c("numeric", "integer", "character"))
    if(time_class == "character" && method != "discrete" && (!is.null(data$tree) || !is.missing(tree))) {
        stop("time can only be in the format n.elements if method is set to discrete and a tree is provided.", call. = FALSE)
    } else {
        if(grep(".element", time) < 1) {
            stop("time argument for discrete bins with a minimum number of elements should be in the format \"n.elements\" where n is the number of elements (e.g. \"15.elements\" for 15 elements per bin)")
        }
        ## Get the time size
        size <- as.numeric(strsplit(time, ".element")[[1]][1])
        ## Get the time bins
        time <- get.bin.elements.size(tree, size, continuous.boundaries = TRUE, return.boundaries = TRUE)
    }


    ## If time is a single value create the time vector by sampling evenly from just after the tree root time (1%) to the present
    if(length(time) == 1) {
        ## time must be at least three if discrete
        if(time < 2) {
            stop.call("", "time must be greater or equal than 2.")
        }

        if(tree_was_missing) {
            ## Set tmax
            tmax <- min(FADLAD$LAD)
            ## Set t0
            t0 <- max(FADLAD$FAD)
        } else {
            ## Set tmax
            tmax <- min(unlist(lapply(tree.age_tree, function(x) min(x$ages))))
            ## Set up t0
            if(is(t0, "logical")) {
                if(!t0) {
                    ## Get the percentages
                    percents <- lapply(tree, get.percent.age)

                    ## Set t0 to root time +/- some age
                    t0 <- tree[[1]]$root.time - max(unlist(lapply(tree, get.percent.age))) * tree[[1]]$root.time
                } else {
                    ## Set t0 to root time
                    t0 <- tree[[1]]$root.time
                }
            } else {
                combine_ages <- unlist(lapply(tree.age_tree, function(x) x$ages))
                if(t0 > max(combine_ages) || t0 < min(combine_ages)) {
                    stop.call(match_call$t0, paste0(") is out of the tree", ifelse(is_multiPhylo, "s", ""), " age range (", paste0(range(combine_ages), collapse = " - "), ")."), msg.pre = "t0 argument (")
                }
            }
        }

        ## Set up time
        time <- round(seq(from = tmax, to = t0, length.out = time + ifelse(method == "discrete", 1, 0)), 2)
    }

    ## time vector must go from past to present
    reverse.time <- function(time) {
        if(time[1] < time[2]) {
            return(time <- rev(time))
        } else {
            return(time)
        }
    }
    time <- reverse.time(time)

    ## TREE (3)
    ## If inc.nodes is not TRUE
    if(inc.nodes != TRUE) {
        ## Check if at least all the data in the table are present in the tree
        no_match_rows <- check.list(tree, function(tree, data) is.na(match(tree$tip.label, rownames(data))), data = data[[1]], condition = any)
        if(any(no_match_rows)) {
            stop.call("", "The labels in the matrix and in the tree do not match!\nTry using clean.data() to match both tree and data or make sure whether nodes should be included or not (inc.nodes = FALSE by default).")
        }
    } else {
        ## Check if the tree has node labels
        if(any(node_labels <- check.list(tree, function(x) length(x$node.label)) == 0)) {
            if(is_multiPhylo) {
                stop.call(match_call$tree, paste0(" contains trees with no node labels (", paste0(c(1:length(node_labels))[node_labels], collapse = ", "), ")."))
            } else {
                stop.call(match_call$tree, paste0(" has no node labels."))
            }

        } else {
            no_match_rows <- check.list(tree, function(tree, data) is.na(match(c(tree$tip.label, tree$node.label), rownames(data))), data = data[[1]], condition = any)
            if(any(no_match_rows)) {
                stop.call("", "The labels in the matrix and in the tree do not match!\nTry using clean.data() to match both tree and data or make sure whether nodes should be included or not (inc.nodes = FALSE by default).")
            }
        }
    }

    ## FADLAD

    # cat("DEBUG chrono.subsets")
    ## If FADLAD is missing, set it to NULL (skipped in the chrono.subsets.fun)
    ## Remove adjust FADLAD and associated functions from the whole package

    if(is.null(FADLAD)) {
        if(method != "continuous") {
            ## If missing, create the FADLAD table
            make.fadlad <- function(tree.age_tree, Ntip_tree) {
                return(data.frame("FAD" = tree.age_tree[1:Ntip_tree,1], "LAD" = tree.age_tree[1:Ntip_tree,1], row.names = tree.age_tree[1:Ntip_tree,2]))
            }
            FADLAD <- lapply(tree.age_tree, make.fadlad, Ntip_tree)
        } else {
            FADLAD <- list(NULL)
        }
    } else {
        ## Check if FADLAD is a table
        check.class(FADLAD, "data.frame")

        if(!all(colnames(FADLAD) %in% c("FAD", "LAD"))) {
            stop.call(match_call$FADLAD, " must be a data.frame with two columns being called respectively:\n\"FAD\" (First Apparition Datum) and \"LAD\" (Last Apparition Datum).")
        } else {
            ## Check if FAD/LAD is in the right order (else reorder)
            if(colnames(FADLAD)[1] == "LAD") {
                FADLAD <- data.frame("FAD" = FADLAD[,2], "LAD" = FADLAD[,1], row.names = rownames(FADLAD))
            }
        }

        ## Check if the FADLAD contains all taxa
        if(any(tree[[1]]$tip.label %in% as.character(rownames(FADLAD)) == FALSE)) {
            ## If not generate the FADLAD for the missing taxa
            missing_FADLAD <- which(is.na(match(tree[[1]]$tip.label, as.character(rownames(FADLAD)))))
            add_FADLAD <- data.frame(tree.age_tree[[1]][missing_FADLAD, 1], tree.age_tree[[1]][missing_FADLAD, 1], row.names = tree.age_tree[[1]][missing_FADLAD, 2])
            colnames(add_FADLAD) <- colnames(FADLAD)
            FADLAD <- rbind(FADLAD, add_FADLAD)
        }
        
        ## Check if nodes are included in the FADLAD
        if(any(rownames(FADLAD) %in% tree[[1]]$node.label)) {
            ## Remove FADLAD nodes/tips not present in either
            if(nrow(FADLAD) != Ntip_tree+Nnode(tree[[1]])) {
                FADLAD <- FADLAD[-c(which(is.na(match(rownames(FADLAD), c(tree[[1]]$tip.label, tree[[1]]$node.label))))),]
            }
        } else {
            ## Remove FADLAD taxa not present in the tree
            if(nrow(FADLAD) != Ntip_tree) {
                FADLAD <- FADLAD[-c(which(is.na(match(rownames(FADLAD), tree[[1]]$tip.label)))),]
            }
        }
        FADLAD <- list(FADLAD)
    }

    ## Check bind data
    if(is_multiPhylo && length(data) == length(tree)) {
        check.class(bind.data, "logical")
    } else {
        if(bind.data) {
            stop(paste0("Impossible to bind the data to the trees since the number of matrices (", length(data), ") is not equal to the number of trees (", length(tree), ")."), call. = FALSE)
        }
    }

    ## -------------------------------
    ##  GENRATING THE TIME subsets
    ## -------------------------------

    ## Toggle the functions
    if(method == "discrete") {
        chrono.subsets.fun <- chrono.subsets.discrete
        if(verbose) message("Creating ", length(time)-1, " time bins through time:", appendLF = FALSE)
    } else {
        chrono.subsets.fun <- chrono.subsets.continuous
        if(verbose) message("Creating ", length(time), " time samples through ", ifelse(length(tree) > 1, paste0(length(tree), " trees:"), "one tree:"), appendLF = FALSE)
    }

    ## Toggle the multiPhylo option
    if(!is_multiPhylo) {
        time_subsets <- chrono.subsets.fun(data[[1]], tree[[1]], time, model, FADLAD[[1]], inc.nodes, verbose)
        # time_subsets <- chrono.subsets.fun(data[[1]], tree[[1]], time, model, FADLAD[[1]], inc.nodes, verbose) ; warning("DEBUG chrono.subsets")
        # data <- data[[1]] ; warning("DEBUG chrono.subsets")
        # tree <- tree[[1]] ; warning("DEBUG chrono.subsets")
        # FADLAD <- FADLAD[[1]] ; warning("DEBUG chrono.subsets")
    } else {

        ## Combining arguments into lists
        combine.args <- function(tree, FADLAD, ...) {
            fixed_args <- list(...)
            return(c(list(tree = tree, FADLAD = FADLAD), fixed_args))
        }

        ## Bundle the arguments into a list
        args_list <- mapply(combine.args, tree, FADLAD, MoreArgs = list(data = data[[1]], time = time,  model = model, inc.nodes = inc.nodes, verbose = verbose), SIMPLIFY = FALSE)

        ## Run all time subsets
        time_subsets <- lapply(args_list, function(arg, fun) do.call(fun, arg), fun = chrono.subsets.fun)

        ## Combine all the data recursively
        time_subsets <- recursive.combine.list(time_subsets)
    }

    if(verbose) message("Done.\n", appendLF = FALSE)


    ## Adding the original subsets
    #time_subsets <- c(make.origin.subsets(data), time_subsets)

    ## Output as a dispRity object
    if(!tree_was_missing) {
        return(make.dispRity(data = data, call = list("subsets" = c(method, model, "trees" = length(tree), "matrices" = length(data), "bind" = bind.data)), subsets = time_subsets, tree = tree))    
    } else {
        return(make.dispRity(data = data, call = list("subsets" = c(method, model, "trees" = length(tree), "matrices" = length(data), "bind" = bind.data)), subsets = time_subsets))        
    }
}
