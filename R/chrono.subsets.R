#' @title Separating data in chronological subsets.
#' @aliases time.series time.subsets
#'
#' @description Splits the data into a chronological (time) subsets list.
#' 
#' @usage chrono.subsets(data, tree, method, time, model, inc.nodes = FALSE,
#'                     FADLAD, verbose = FALSE, t0 = FALSE)
#'
#' @param data A \code{matrix} (see details).
#' @param tree A \code{phylo} object matching the data and with a \code{root.time} element. This argument can be left missing if \code{method = "discrete"} and all elements are present in the optional \code{FADLAD} argument.
#' @param method The time subsampling method: either \code{"discrete"} (or \code{"d"}) or \code{"continuous"} (or \code{"c"}).
#' @param time Either a single \code{integer} for the number of discrete or continuous samples or a \code{vector} containing the age of each sample.
#' @param model One of the following models: \code{"acctran"}, \code{"deltran"}, \code{"random"}, \code{"proximity"}, \code{"equal.split"} or \code{"gradual.split"}. Is ignored if \code{method = "discrete"}.
#' @param inc.nodes A \code{logical} value indicating whether nodes should be included in the time subsets. Is ignored if \code{method = "continuous"}.
#' @param FADLAD An optional \code{data.frame} containing the first and last occurrence data.
#' @param verbose A \code{logical} value indicating whether to be verbose or not. Is ignored if \code{method = "discrete"}.
#' @param t0 If \code{time} is a number of samples, whether to start the sampling from the \code{tree$root.time} (\code{TRUE}), or from the first sample containing at least three elements (\code{FALSE} - default) or from a fixed time point (if \code{t0} is a single \code{numeric} value).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{matrix}{the multidimensional space (a \code{matrix}).}
#' \item{call}{A \code{list} containing the called arguments.}
#' \item{subsets}{A \code{list} containing matrices pointing to the elements present in each subsets.}
#'
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#'  
#' @details
#' The data is considered as the multidimensional space with rows as elements and columns as dimensions and is not transformed (e.g. if ordinated with negative eigen values, no correction is applied to the matrix).
#' 
#' If \code{method = "continuous"} and when the sampling is done along an edge of the tree, the data selected for the time subsets is can be one of the following:
#' \itemize{
#'      \item Punctuated models:
#'      \itemize{
#'         \item \code{"acctran"}: always the value from the ancestral node.
#'         \item \code{"deltran"}: always the value from the descendant node or tip.
#'         \item \code{"random"}: randomly selected from the ancestral node or the descendant node or tip.
#'         \item \code{"proximity"}: selects the ancestral node or the descendant with a probability relative to branch length.
#'      }
#'      \item Gradual models:
#'      \itemize{
#'          \item \code{"equal.split"}: randomly selected from the ancestral node or the descendant node or tip with a 50\% probability each.
#'          \item \code{"gradual.split"}: selects the ancestral node or the descendant with a probability relative to branch length.
#'      }
#' }
#' N.B. \code{"equal.split"} and \code{"gradual.split"} differ from the punctuated models by outputting a node/tip probability table rather than simply the node and the tip selected. In other words, when bootstrapping using \code{\link{boot.matrix}}, the two former models will properly integrate the probability to the bootstrap procedure (i.e. different tips/nodes can be drawn) and the two latter models will only use the one node/tip determined by the model before the bootstrapping.
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
# method = "discrete"
# model = "acctran"
# time = 5

# inc.nodes = TRUE
# FADLAD = BeckLee_ages
# data = BeckLee_mat99
# tree = BeckLee_tree
# method = "continuous"
# model = "proximity"
# time <- c(120, 100, 80, 60, 40 , 20, 0)
# verbose <- TRUE
# t0 <- FALSE


# plot(BeckLee_tree, cex = 0.5)
# nodelabels(BeckLee_tree$node.label, cex = 0.5)
# axisPhylo()
# abline(v = 40)


chrono.subsets <- function(data, tree, method, time, model, inc.nodes = FALSE, FADLAD, verbose = FALSE, t0 = FALSE) {
    
    match_call <- match.call()

    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## data must be a matrix
    check.class(data, "matrix")

    ## Check whether it is a distance matrix
    if(check.dist.matrix(data, just.check = TRUE)) {
        warning("chrono.subsets is applied on what seems to be a distance matrix.\nThe resulting matrices won't be distance matrices anymore!", call. = FALSE)
    }

    ## nrow_data variable declaration
    nrow_data <- nrow(data)

    ## TREE (1)
    ## tree must be a phylo object
    if(!missing(tree)) {
        check.class(tree, "phylo")
        ## tree must be dated
        if(length(tree$root.time) != 1) {
            stop.call(match_call$tree, paste0(" must be a dated tree with a $root.time element. Use:\n    ", as.expression(match_call$tree), "$root.time <- the_age_of_the_root"))
        }
        ## tree.age_tree variable declaration
        tree.age_tree <- tree.age(tree)
    }


    ## METHOD
    all_methods <- c("discrete", "d", "continuous", "c")
    ## method must be a character string
    check.class(method, "character")
    method <- tolower(method)
    ## method must have only one element
    check.length(method, 1, paste(" argument must be one of the following: ", paste(all_methods, collapse = ", "), ".", sep = ""))
    ## method must be either "discrete", "d", "continuous", or "c"
    check.method(method, all_methods, "method argument")

    ## if method is "d" or "c", change it to "discrete" or "continuous" (lazy people...)
    if(method == "d") method <- "discrete"
    if(method == "c") method <- "continuous"

    ## If the tree is missing, the method can intake a star tree (i.e. no phylogeny)
    if(missing(tree)) {
        if(missing(FADLAD)) {
            stop.call("", "If no phylogeny is provided, all elements must be present in the FADLAD argument.")
        }
        if(method == "continuous") {
            stop.call("", "If no phylogeny is provided, method must be \"discrete\".")
        }
        tree_was_missing <- TRUE

        ## Checking FADLAD disponibilities
        names_data <- rownames(data)
        ## All names must be present in the data
        if(!all(names_data %in% rownames(FADLAD))) {
            stop.call("", "If no phylogeny is provided, all elements must be present in the FADLAD argument.")
        }
        ## Generating the star tree
        tree <- stree(nrow_data, tip.label = names_data)
        tree$root.time <- max(FADLAD)
        tree$edge.length <- FADLAD$FAD
    } else {
        tree_was_missing <- FALSE
    }

    ## Ntip_tree variable declaration
    Ntip_tree <- Ntip(tree)

    ##t0
    if(class(t0) != "logical") {
        silent <- check.class(t0, c("numeric", "integer"), msg = " must be logical or a single numeric value.")
        check.length(t0, 1, errorif = FALSE, msg = " must be logical or a single numeric value.")
    } 

    ## TIME
    ## time must be numeric or integer
    silent <- check.class(time, c("numeric", "integer"))
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
            tmax <- min(tree.age_tree[,1])
        
            ## Set up t0
            if(class(t0) == "logical") {
                if(t0 == FALSE ) {
                    ## Set the percentage for reaching the first sample containing three elements
                    percent <- 0.01
                    while(Ntip(paleotree::timeSliceTree(tree, tree$root.time - percent * tree$root.time, drop.extinct = TRUE, plot = FALSE)) < 3) {
                        ## Increase percentage until slice has three elements
                        percent <- percent + 0.01
                    }
                    ## Set t0 to root time +/- some age
                    t0 <- tree$root.time - percent * tree$root.time
                } else {
                    ## Set t0 to root time
                    t0 <- tree$root.time
                }
            } else {
                if(t0 > max(tree.age_tree[,1]) || t0 < min(tree.age_tree[,1])) {
                    stop.call("", "t0 is out of the tree age range.")
                }
            }
        }

        ## Set up time
        if(method == "discrete") time <- seq(from = tmax, to = t0, length.out = time + 1)
        if(method == "continuous") time <- seq(from = tmax, to = t0, length.out = time)
    }

    ## time vector must go from past to present
    if(time[1] < time[2]) time <- rev(time)

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
            ## ~~~~~~~~~~~
            ##  Include the make.model option here?
            ##  make.model should be tested on slice.tree function
            ## ~~~~~~~~~~~
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

    ## TREE (2)
    ## If inc.nodes is not TRUE
    if(inc.nodes != TRUE) {
        ## Check if at least all the data in the table are present in the tree
        if(any(is.na(match(rownames(data), tree$tip.label)))) {
            stop.call("", "The labels in the matrix and in the tree do not match!\nUse clean.data() to match both tree and data or make sure whether nodes should be included or not (inc.nodes = FALSE by default).")
        }
    } else {
        ## Check if the tree has node labels
        if(length(tree$node.label) != 0) {
            ## Check if the tree and the table are the same length
            if(nrow_data != (Ntip_tree + Nnode(tree))) {
                stop.call("", "The labels in the matrix and in the tree do not match!\nRemember to check the node labels in the tree and the matrix.")
            }
            ## Check if both nodes and tip labels match with the data rownames
            if(any(is.na(c(rownames(data), c(tree$tip.label, tree$node.label))))) {
                stop.call("", "The labels in the matrix and in the tree do not match!\nCheck especially the node labels in the tree and the matrix.")
            }
        } else {
            stop.call("", "The labels in the matrix and in the tree do not match!\nRemember to check the node labels in the tree and the matrix.")
        }
    }

    ## FADLAD
    if(missing(FADLAD)) {
        ## If missing, create the FADLAD table
        FADLAD <- data.frame("FAD" = tree.age_tree[1:Ntip_tree,1], "LAD" = tree.age_tree[1:Ntip_tree,1], row.names = tree.age_tree[1:Ntip_tree,2])
        ## message("No FAD/LAD table has been provided. \nAll tips are assumed to be single points in time.")
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
        if(any(tree$tip.label %in% as.character(rownames(FADLAD)) == FALSE)) {
            ##  message("Some tips have no FAD/LAD and are assumed to be single points in time.")
            ## If not generate the FADLAD for the missing taxa
            missing_FADLAD <- which(is.na(match(tree$tip.label, as.character(rownames(FADLAD)))))
            add_FADLAD <- data.frame(tree.age_tree[missing_FADLAD, 1], tree.age_tree[missing_FADLAD, 1], row.names = tree.age_tree[missing_FADLAD, 2])
            colnames(add_FADLAD) <- colnames(FADLAD)
            FADLAD <- rbind(FADLAD, add_FADLAD)
        }
        ## Remove FADLAD taxa not present in the tree
        if(nrow(FADLAD) != Ntip_tree) {
            FADLAD <- FADLAD[-c(which(is.na(match(rownames(FADLAD), tree$tip.label)))),]
        }
    }

    ## VERBOSE
    check.class(verbose, "logical")

    ## -------------------------------
    ##  GENRATING THE TIME subsets
    ## -------------------------------

    if(method == "discrete") {
        time_subsets <- chrono.subsets.discrete(data, tree, time, FADLAD, inc.nodes, verbose)
    }

    if(method == "continuous") {
        time_subsets <- chrono.subsets.continuous(data, tree, time, model, FADLAD, verbose)
    }

    ## Adding the original subsets
    #time_subsets <- c(make.origin.subsets(data), time_subsets)

    ## Output as a dispRity object
    return(make.dispRity(data = data, call = list("subsets" = c(method, model)), subsets = time_subsets))
}
