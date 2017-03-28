#' @title Separating ordinated data in time subsamples.
#' @aliases time.series
#'
#' @description Splits the ordinated data into a time subsamples list.
#' 
#' @usage time.subsamples(data, tree, method, time, model, inc.nodes = FALSE, FADLAD, verbose = FALSE)
#'
#' @param data An ordinated matrix of maximal dimensions \eqn{k*(k-1)}.
#' @param tree A \code{phylo} object matching the data and with a \code{root.time} element.
#' @param method The time subsamples method: either \code{"discrete"} (or \code{"d"}) or \code{"continuous"} (or \code{"c"}).
#' @param time Either a single \code{integer} for them number of discrete or continuous samples or a \code{vector} containing the age of each sample.
#' @param model One of the following models: \code{"acctran"}, \code{"deltran"}, \code{"punctuated"} or \code{"gradual"}. Is ignored if \code{method = "discrete"}.
#' @param inc.nodes A \code{logical} value indicating whether nodes should be included in the time subsamples. Is ignored if \code{method = "continuous"}.
#' @param FADLAD An optional \code{data.frame} containing the first and last occurrence data.
#' @param verbose A \code{logical} value indicating whether to be verbose or not. Is ignored if \code{method = "discrete"}.
#'
#' @return
#' This function outputs a \code{dispRity} object containing:
#' \item{data}{A \code{list} of the split ordinated data (each element is a \code{matrix}).}
#' \item{elements}{A \code{vector} containing all the rownames from the input matrix.}
#' \item{subsamples}{A \code{vector} containing the name of the subsamples.}
#' \code{dispRity} objects can be summarised using \code{print} (S3).
#' 
#' @details
#' If \code{method = "continuous"} and when the sampling is done along an edge of the tree, the ordinated data selected for the time subsamples is:
#' \itemize{
#'   \item \code{"acctran"}: always the one of the ancestral node.
#'   \item \code{"deltran"}: always the one of the offspring node or tip.
#'   \item \code{"punctuated"}: randomly selected from the ancestral node or the offspring node or tip.
#'   \item \code{"gradual"}: either the ancestral node if the sampling point on the edge is \eqn{< edge.length/2} else the offspring node or tip.
#' }
#'
#' @examples
#' ## Load the Beck & Lee 2014 data
#' data(BeckLee_tree) ; data(BeckLee_mat50)
#' data(BeckLee_mat99) ; data(BeckLee_ages)
#'
#' ## Time bining (discrete method)
#' ## Generate two discrete time bins from 120 to 40 Ma every 40 Ma
#' time.subsamples(data = BeckLee_mat50, tree = BeckLee_tree, method = "discrete",
#'      time = c(120, 80, 40), inc.nodes = FALSE, FADLAD = BeckLee_ages)
#' ## Generate the same one but including nodes
#' time.subsamples(data = BeckLee_mat99, tree = BeckLee_tree, method = "discrete",
#'      time = c(120, 80, 40), inc.nodes = TRUE, FADLAD = BeckLee_ages)
#'
#' ## Time slicing (continuous method)
#' ## Generate 5 equidistant time slices in the data set assuming gradual
#' ## evolutionary models
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
# data = BeckLee_mat99
# tree = BeckLee_tree
# method = "continuous"
# model = "acctran"
# time = 5
# inc.nodes = TRUE
# FADLAD = BeckLee_ages

time.subsamples <- function(data, tree, method, time, model, inc.nodes = FALSE, FADLAD, verbose = FALSE) {
    
    ## ----------------------
    ##  SANITIZING
    ## ----------------------
    ## DATA
    ## data must be a matrix
    check.class(data, "matrix")
    ## nrow_data variable declaration
    nrow_data <- nrow(data)
    ## data must be of size k*<=k-1
    if(ncol(data) > (nrow(data) - 1)) warning("Input data should have at maximum (rows-1) columns.")

    ## TREE (1)
    ## tree must be a phylo object
    check.class(tree, "phylo")
    ## Ntip_tree variable declaration
    Ntip_tree <- Ntip(tree)
    ## tree must be dated
    if(length(tree$root.time) != 1) stop("Tree must be a dated tree with a $root.time element.")

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

    ## TIME
    ## time must be numeric of integer
    silent <- check.class(time, c("numeric", "integer"))
    ## If time is a single value create the time vector by sampling evenly from just after the tree root time (1%) to the present
    if(length(time) == 1) {
        ## time must be at least 3 if discrete
        if(method == "discrete" && time < 3) stop("If method is discrete, time must have at least 3 elements.")
        ## or at least time 2 if continuous
        if(method == "continuous" && time < 2) stop("If method is discrete, time must have at least 2 elements.")
        ## Create the time vector
        ## Make sure the oldest slice has at least 3 taxa:
        ## Set the oldest slice at 1% of tree height
        percent <- 0.01
        while(Ntip(paleotree::timeSliceTree(tree, tree$root.time-percent*tree$root.time, drop.extinct = TRUE, plot = FALSE)) < 3) {
            ## Increase percent until slice has 3 elements
            percent <- percent + 0.01
        }
        ## Set up time
        if(method == "discrete") time <- seq(from = 0, to = tree$root.time-percent*tree$root.time, length.out = time+1)
        if(method == "continuous") time <- seq(from = 0, to = tree$root.time-percent*tree$root.time, length.out = time)    
    }

    ## time cannot be older than the root age
    if(any(time >= tree$root.time)) stop("Time cannot be older or equal to the tree's root age.")
    ## time vector must go from past to present
    if(time[1] < time[2]) time <- rev(time)

    ## MODEL
    ## if method is discrete ignore model
    if(method == "discrete") {
        model <- NULL
    } else {
        ## else model must be one of the following
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
        ## check if the tree and the table are the same length
        if(nrow_data != Ntip_tree) stop("The labels in the ordinated matrix and in the tree do not match!")
        ## Also check if the names are identical
        if(any(is.na(match(rownames(data), tree$tip.label)))) stop("The labels in the ordinated matrix and in the tree do not match!")        
    } else {
        ## Check if the tree has node labels
        if(length(tree$node.label) != 0) {
            ## Check if the tree and the table are the same length
            if(nrow_data != (Ntip_tree+Nnode(tree))) stop("The labels in the ordinated matrix and in the tree do not match!\nCheck especially the node labels in the tree and the ordinated matrix.")
            ## Check if both nodes and tip labels match with the data rownames
            if(any(is.na(c(rownames(data), c(tree$tip.label, tree$node.label))))) stop("The labels in the ordinated matrix and in the tree do not match!\nCheck especially the node labels in the tree and the ordinated matrix.")
        } else {
            stop("The labels in the ordinated matrix and in the tree do not match!\nCheck especially the node labels in the tree and the ordinated matrix.")
        }
    }

    ## FADLAD
    ## tree.age_tree variable declaration
    tree.age_tree <- tree.age(tree)
    if(missing(FADLAD)) {
        ## If missing, create the FADLAD table
        FADLAD <- data.frame("FAD" = tree.age_tree[1:Ntip_tree,1], "LAD" = tree.age_tree[1:Ntip_tree,1], row.names = tree.age_tree[1:Ntip_tree,2])
        ## message("No FAD/LAD table has been provided.\nEvery tips are assumed to be single points in time.")
    } else {
        ## Check if FADLAD is a table
        check.class(FADLAD, "data.frame")
        if(!all(colnames(FADLAD) == c("FAD", "LAD"))) stop("FADLAD must be a data.frame with two columns being called respectively:\n\"FAD\" (First Apparition Datum) and \"LAD\" (Last Apparition Datum).")
        ## Check if the FADLAD contains all taxa
        if(any(tree$tip.label %in% as.character(rownames(FADLAD)) == FALSE)) {
            ##  message("Some tips have no FAD/LAD and are assumed to be single points in time.")
            ## If not generate the FADLAD for the missing taxa
            missing_FADLAD <- which(is.na(match(tree$tip.label, as.character(rownames(FADLAD)))))
            add_FADLAD <- data.frame(tree.age_tree[missing_FADLAD,1], tree.age_tree[missing_FADLAD,1], row.names = tree.age_tree[missing_FADLAD,2])
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

    ## ----------------------
    ##  GENRATING THE TIME subsamples
    ## ----------------------

    if(method == "discrete") {
        time_subsamples <- time.subsamples.discrete(data, tree, time, FADLAD, inc.nodes)
    }

    if(method == "continuous") {
        time_subsamples <- time.subsamples.continuous(data, tree, time, model, FADLAD, verbose)
    }

    ## Adding the original subsamples
    #time_subsamples <- c(make.origin.subsamples(data), time_subsamples)

    ## Output as a dispRity object
    return(make.dispRity(data = data, call = list("subsamples" = c(method, model)), subsamples = time_subsamples))
}
