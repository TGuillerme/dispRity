#' @title Separating data in time subsamples.
#' @aliases time.series
#'
#' @description Splits the data into a time subsamples list.
#' 
#' @usage time.subsamples(data, tree, method, time, model, inc.nodes = FALSE, FADLAD, verbose = FALSE, t0 = FALSE)
#'
#' @param data A matrix.
#' @param tree A \code{phylo} object matching the data and with a \code{root.time} element. This argument can be left missing if \code{method = "discrete"} and all elements are present in the optional \code{FADLAD} argument.
#' @param method The time subsampling method: either \code{"discrete"} (or \code{"d"}) or \code{"continuous"} (or \code{"c"}).
#' @param time Either a single \code{integer} for the number of discrete or continuous samples or a \code{vector} containing the age of each sample.
#' @param model One of the following models: \code{"acctran"}, \code{"deltran"}, \code{"punctuated"} or \code{"gradual"}. Is ignored if \code{method = "discrete"}.
#' @param inc.nodes A \code{logical} value indicating whether nodes should be included in the time subsamples. Is ignored if \code{method = "continuous"}.
#' @param FADLAD An optional \code{data.frame} containing the first and last occurrence data.
#' @param verbose A \code{logical} value indicating whether to be verbose or not. Is ignored if \code{method = "discrete"}.
#' @param t0 If \code{time} is a number of samples, whether to start the sampling from the \code{tree$root.time} (\code{TRUE}), or from the first sample containing at least three elements (\code{FALSE} - default) or from a fixed time point (if \code{t0} is a single \code{numeric} value).
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{matrix}{the multidimensional space (a \code{matrix}).}
#' \item{call}{A \code{list} containing the called arguments.}
#' \item{subsamples}{A \code{list} containing matrices pointing to the elements present in each subsamples.}
#'
#' Use \link{summary.dispRity} to summarise the \code{dispRity} object.
#' 
#'  
#' @details
#' If \code{method = "continuous"} and when the sampling is done along an edge of the tree, the data selected for the time subsamples is:
#' \itemize{
#'   \item \code{"acctran"}: always the value from the ancestral node.
#'   \item \code{"deltran"}: always the value from the descendant node or tip.
#'   \item \code{"punctuated"}: randomly selected from the ancestral node or the descendant node or tip.
#'   \item \code{"gradual"}: either the ancestral node if the sampling point on the edge is \eqn{< edge.length/2} else the descendant node or tip.
#' }
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_tree) ; data(BeckLee_mat50)
#' data(BeckLee_mat99) ; data(BeckLee_ages)
#'
#' ## Time binning (discrete method)
#' ## Generate two discrete time bins from 120 to 40 Ma every 40 Ma
#' time.subsamples(data = BeckLee_mat50, tree = BeckLee_tree, method = "discrete",
#'      time = c(120, 80, 40), inc.nodes = FALSE, FADLAD = BeckLee_ages)
#' ## Generate the same time bins but including nodes
#' time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree, method = "discrete",
#'      time = c(120, 80, 40), inc.nodes = TRUE, FADLAD = BeckLee_ages)
#'
#' ## Time slicing (continuous method)
#' ## Generate five equidistant time slices in the dataset assuming a gradual
#' ## evolutionary model
#' time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree,
#'      method = "continuous", model = "acctran", time = 5,
#'      FADLAD = BeckLee_ages)
#'
#' @seealso \code{\link{tree.age}}, \code{\link{slice.tree}}, \code{\link{cust.subsamples}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' @author Thomas Guillerme

##Testing
# warning("DEBUG time.subsamples")
# source("sanitizing.R")
# source("time.subsamples_fun.R")
# data(BeckLee_tree) ; data(BeckLee_mat50)
# data(BeckLee_mat99) ; data(BeckLee_ages)
# data = BeckLee_mat50
# tree = BeckLee_tree
# method = "discrete"
# model = "acctran"
# time = 5
# inc.nodes = TRUE
# FADLAD = BeckLee_ages

# data <- matrix_ord_Hall
# tree <- tree_Hall
# method = "continuous"
# model = "gradual"
# time <- c(120, 44)
# verbose <- TRUE


time.subsamples <- function(data, tree, method, time, model, inc.nodes = FALSE, FADLAD, verbose = FALSE, t0 = FALSE) {
    
    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## data must be a matrix
    check.class(data, "matrix")
    ## nrow_data variable declaration
    nrow_data <- nrow(data)

    ## TREE (1)
    ## tree must be a phylo object
    if(!missing(tree)) {
        check.class(tree, "phylo")
        ## tree must be dated
        if(length(tree$root.time) != 1) stop("Tree must be a dated tree with a $root.time element.")
        ## tree.age_tree variable declaration
        tree.age_tree <- tree.age(tree, age = tree$root.time)
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
        if(missing(FADLAD)) stop("If no phylogeny is provided, all elements must be present in the FADLAD argument.")
        if(method == "continuous") stop("If no phylogeny is provided, method must be \"discrete\".")

        ## Checking FADLAD disponibilities
        names_data <- rownames(data)
        ## All names must be present in the data
        if(!all(names_data %in% rownames(FADLAD))) stop("If no phylogeny is provided, all elements must be present in the FADLAD argument.")
        ## Generating the star tree
        tree <- stree(nrow_data)
        tree$tip.label <- names_data
        tree$root.time <- max(names_data)
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
        if(time < 2) stop("Time must be greater or equal than 2.")

        if(missing(tree)) {
            ## Set tmax
            tmax <- min(FADLAD$LAD)
            ## Set t0
            t0 <- max(FADLAD$FAD)
        } else {
            ## Set tmax
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
                    stop("t0 is out of the tree age range.")
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
        all_models <- c("acctran", "deltran", "punctuated", "gradual")
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
    } else {
        ## Include nodes is mandatory
        inc.nodes <- TRUE
    }

    ## TREE (2)
    ## If inc.nodes is not TRUE
    if(inc.nodes != TRUE) {
        ## Check if at least all the data in the table are present in the tree
        if(any(is.na(match(rownames(data), tree$tip.label)))) {
            stop("The labels in the matrix and in the tree do not match!")
        }
    } else {
        ## Check if the tree has node labels
        if(length(tree$node.label) != 0) {
            ## Check if the tree and the table are the same length
            if(nrow_data != (Ntip_tree + Nnode(tree))) stop("The labels in the matrix and in the tree do not match!\nRemember to check the node labels in the tree and the matrix.")
            ## Check if both nodes and tip labels match with the data rownames
            if(any(is.na(c(rownames(data), c(tree$tip.label, tree$node.label))))) stop("The labels in the matrix and in the tree do not match!\nCheck especially the node labels in the tree and the matrix.")
        } else {
            stop("The labels in the matrix and in the tree do not match!\nRemember to check the node labels in the tree and the matrix.")
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
        if(!all(colnames(FADLAD) == c("FAD", "LAD"))) stop("FADLAD must be a data.frame with two columns being called respectively:\n\"FAD\" (First Apparition Datum) and \"LAD\" (Last Apparition Datum).")
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
    ##  GENRATING THE TIME subsamples
    ## -------------------------------

    if(method == "discrete") {
        time_subsamples <- time.subsamples.discrete(data, tree, time, FADLAD, inc.nodes, verbose)
    }

    if(method == "continuous") {
        time_subsamples <- time.subsamples.continuous(data, tree, time, model, FADLAD, verbose)
    }

    ## Adding the original subsamples
    #time_subsamples <- c(make.origin.subsamples(data), time_subsamples)

    ## Output as a dispRity object
    return(make.dispRity(data = data, call = list("subsamples" = c(method, model)), subsamples = time_subsamples))
}
