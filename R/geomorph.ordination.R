#' @title Imports data from geomorph
#'
#' @description Takes geomorph Procrustes object or a geomorph.data.frame object and ordinates it.
#'
#' @param data An array (p x k x n) typically obtained from a Procrustes superimposition \code{\link[geomorph]{gpagen}} or a \code{\link[geomorph]{geomorph.data.frame}} object.
#' @param ... Any optional arguments to be passed to \code{\link[stats]{prcomp}}.
#' 
#' @details
#' If \code{data} is a \code{geomorph.data.frame} object containing factors, directly performs a \code{\link{custom.subsets}} using these factors.
#' 
#' @return
#' A \code{matrix} or a \code{dispRity} object.
#' 
#' @examples
#' \dontrun{
#' require(geomorph)
#' ## Loading the plethodon dataset
#' data(plethodon)
#' 
#' ## Performing a Procrustes transform
#' procrustes <- geomorph::gpagen(plethodon$land,PrinAxes=FALSE)
#' 
#' ## Obtaining the ordination matrix
#' geomorph.ordination(procrustes)
#'
#' 
#' ## Using a geomorph.data.frame
#' geomorph_df <- geomorph.data.frame(procrustes, species = plethodon$species)
#' 
#' geomorph.ordination(geomorph_df)
#' 
#' ## Calculating disparity from dispRity or geomorph::morphol.disparity
#' geomorph_disparity <- geomorph::morphol.disparity(coords ~ 1,
#'                       groups= ~ species, data = geomorph_df)
#' dispRity_disparity <- dispRity(geomorph.ordination(geomorph_df),
#'                       metric = function(X) return(sum(X^2)/nrow(X)))
#' 
#' ## Extracting the raw disparity values
#' geomorph_val <- round(as.numeric(geomorph_disparity$Procrustes.var), 15)
#' dispRity_val <- as.vector(summary(dispRity_disparity, round = 15)$obs)
#' 
#' ## Comparing the values (to the 15th decimal!)
#' geomorph_val == dispRity_val # all TRUE
#' }
#'
#' @seealso \code{\link[geomorph]{gpagen}}, \code{\link[geomorph]{morphol.disparity}}, \code{\link[stats]{prcomp}}, \code{\link{custom.subsets}}, \code{\link{chrono.subsets}}, \code{\link{boot.matrix}}, \code{\link{dispRity}}.
#' 

# require(geomorph)
# source("sanitizing.R")
# source("geomorph.ordination_fun.R")
# data(plethodon)
# procrustes <- gpagen(plethodon$land,PrinAxes=FALSE)
# geomorph_df <- geomorph.data.frame(procrustes, species = plethodon$species, site = plethodon$site)
# data <- geomorph_df

geomorph.ordination <- function(data, ...) {

    match_call <- match.call()

    ## Sanitising
    ## data
    data_class <- check.class(data, c("gpagen", "geomorph.data.frame"))

    ## coords
    if(is.null(data$coords)) {
        stop(paste(as.expression(match_call$data), " must contain coordinates in $coords."), call. = FALSE)
    } else {
        coords <- data$coords
        check.class(coords, "array")
    }


    ## Ordinating the Procrustes data (code from Emma Sherratt)
    columns <- dim(coords)[1] * dim(coords)[2]
    rows <- dim(coords)[3]
    matrix_out <- aperm(coords, c(3, 2, 1))
    dim(matrix_out) <- c(rows, columns)
    rownames(matrix_out) <- dimnames(coords)[[3]]
    ordination <- stats::prcomp(matrix_out, ...)$x
    # ordination <- stats::prcomp(matrix_out)$x ; warning("DEBUG: geomorph_df")


    ## Class
    if(class(data) == "geomorph.data.frame") {
        ## Get the meta data
        factors <- which(unlist(lapply(data, class)) == "factor")

        ## Get the list of subsets
        group_list <- unlist(lapply(data[factors], make.groups.factors), recursive = FALSE)

        ## Get the names of the ordination elements (if missing)
        if(is.null(rownames(ordination))) {
            if(is.null(names(data$coords))) {
                rownames(ordination) <- seq_along(1:nrow(ordination))
            } else {
                rownames(ordination) <- dimnames(data$coords)[[3]]
            }
        }
        return(custom.subsets(ordination, group = group_list))

    } else {
        ## Just output the ordinated matrix
        return(ordination)
    }
}