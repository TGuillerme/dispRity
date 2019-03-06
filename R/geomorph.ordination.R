#' @title Imports data from geomorph
#'
#' @description Takes geomorph Procrustes object or a geomorph.data.frame object and ordinates it.
#'
#' @param data An array (p x k x n) typically obtained from a Procrustes superimposition \code{\link[geomorph]{gpagen}} or a \code{\link[geomorph]{geomorph.data.frame}} object.
#' @param ordinate Logical, whether to ordinate the data using \code{\link[stats]{prcomp}} (\code{TRUE}; default) or not (\code{FALSE}; the code then returns the raw coordinates matrix).
#' @param ... Any optional arguments to be passed to \code{\link[stats]{prcomp}} (is ignored if \code{ordinate = FALSE}).
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
#' procrustes <- geomorph::gpagen(plethodon$land, PrinAxes = FALSE)
#' 
#' ## Obtaining the ordination matrix
#' geomorph.ordination(procrustes)
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
#' dispRity_val <- as.vector(summary(dispRity_disparity, digits = 15)$obs)
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

geomorph.ordination <- function(data, ordinate = TRUE, ...) {

    match_call <- match.call()

    ## Sanitising
    ## data
    data_class <- check.class(data, c("gpagen", "geomorph.data.frame"))

    ## coords
    if(is.null(data$coords)) {
        stop.call(match_call$data, " must contain coordinates in $coords.")
    } else {
        coords <- data$coords
        check.class(coords, "array")
    }

    ## Ordination
    check.class(ordinate, "logical")

    ## Transforming the coords into a matrix (code from Emma Sheratt, equivalent to geomorph::two.d.array)
    columns <- dim(coords)[1] * dim(coords)[2]
    rows <- dim(coords)[3]
    matrix_out <- aperm(coords, c(3, 2, 1))
    dim(matrix_out) <- c(rows, columns)
    rownames(matrix_out) <- dimnames(coords)[[3]]

    if(ordinate){
        ## Ordinating the Procrustes data
        ordination <- stats::prcomp(matrix_out, ...)$x
        # ordination <- stats::prcomp(matrix_out)$x ; warning("DEBUG: geomorph_df")
    } else {
        ## Simply using the matrix
        ordination <- matrix_out
    }

    ## Class
    if(class(data) == "geomorph.data.frame") {
        ## Get the meta data
        factors <- which(unlist(lapply(data, class)) == "factor")

        if(length(factors) == 0) {
            ## Try coercing the into factors
            no_factors <- which(names(data) == "Csize" | names(data) == "coords")
            data_tmp <- data[-no_factors]
            warning(paste0("Attempting to coerce variables in ", as.expression(match_call$data), " as factor."), call. = FALSE)
            data[-no_factors] <- lapply(data_tmp, as.factor)

            ## Get the meta data
            factors <- which(unlist(lapply(data, class)) == "factor")
        }

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