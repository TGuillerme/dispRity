#' @title Curves differences
#'
#' @description Getting the differences between two curves
#'
#' @param x The \textit{x} axis coordinates
#' @param y1 The \textit{y} coordinates for the first curve
#' @param y2 The \textit{y} coordinates for the second curve (can be \code{NULL})
# @param x2 Optional, the \textit{x} coordinates for the second curve (can be \code{NULL})
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

absolute.curve.area <- function(x, y1, y2 = NULL) { # Add and x2 = NULL option if Xs don't match

    ## Number of points
    n <- length(x)

    ## If y2 is null, get the area from the minimum y1 value
    if(is.null(y2)) {
        y2 <- rep(min(y1), n)
    }

    ## Detect any crossing points
    crossing_points <- pracma::poly_crossings(rbind(x, y1), rbind(x, y2))

    if(is.null(crossing_points) || length(crossing_points) == 0 ) {
        ## Get the polygon coordinates
        polygon_coords <- get.poly.coords(x, y1, y2)
        return(abs(pracma::polyarea(polygon_coords[1,], polygon_coords[2,])))

    } else {

        ## Number of crossing points
        n_crossing_points <- nrow(crossing_points)

        ## Check if the two curves have the same starting final point
        if(all(c(x[1], y1[1]) == c(x[1], y2[1])) || all(c(x[n], y1[n]) == c(x[n], y2[n]))) {
            ## Check if the all the crossings are the starting point
            if(all(c(x[1], y1[1]) == crossing_points) || all(c(x[n], y1[n]) == crossing_points)) {
                ## Get the polygon coordinates
                polygon_coords <- get.poly.coords(x, y1, y2)
                return(abs(pracma::polyarea(polygon_coords[1,], polygon_coords[2,])))
            } 
        } else {

            ## Remove first crossing point if necessary
            if(all(c(x[1], y1[1]) == crossing_points[1,])) {
                crossing_points <- crossing_points[-1,]
                n_crossing_points <- n_crossing_points-1
            }

            ## Remove last crossing point if necessary
            if(all(c(x[n], y1[n]) == crossing_points[n_crossing_points,])) {
                crossing_points <- crossing_points[-n_crossing_points,]
                n_crossing_points <- n_crossing_points-1
            }

            ## Initialise the area
            area <- numeric()

            ## Loop through the areas
            for(sub_polygon in 1:n_crossing_points) {

                ## Get the coordinates
                trim_x <- trim.coordinates(x, crossing_points[sub_polygon, 1])
                trim_y1 <- trim.coordinates(y1, crossing_points[sub_polygon, 2])
                trim_y2 <- trim.coordinates(y2, crossing_points[sub_polygon, 2])


                ####
                # PROBLEM WITH COORDINATES 
                ###

            }

            return("Nope1")

        }

        return("Nope3")
    }
}

get.poly.coords <- function(x, y1, y2) {
    return(cbind(rbind(x, y1), rbind(rev(x), y2)))
}

trim.coordinates <- function(coordinates, cutoff) {
    ## Getting the values post/pre cutoff
    coordinates_pre_cutoff <- coordinates[which(coordinates <= cutoff)]
    coordinates_pos_cutoff <- coordinates[which(coordinates >= cutoff)]
    ## Adding the cutoff value if not present
    if(!any(coordinates == cutoff)) {
        # if(coordinates_pre_cutoff[1] > coordinates_pos_cutoff[1]) {
            coordinates_pre_cutoff <- c(cutoff, coordinates_pre_cutoff)
            coordinates_pos_cutoff <- c(coordinates_pos_cutoff, cutoff)
        # } else {
        #     coordinates_pre_cutoff <- c(, coordinates_pre_cutoff)
        #     coordinates_pos_cutoff <- c(cutoff, coordinates_pos_cutoff)
        # }
        return(list("pre" = coordinates_pos_cutoff, "pos" = coordinates_pre_cutoff))
    } 

    ## Sorting the cutoffs if reversed
    if(coordinates_pre_cutoff[1] > coordinates_pos_cutoff[1]) {
        return(list("pre" = coordinates_pos_cutoff, "pos" = coordinates_pre_cutoff))
    } else {
        return(list("pre" = coordinates_pre_cutoff, "pos" = coordinates_pos_cutoff))
    }
}


# make.polygon <- function(x, y) {
#     ## Make a polygon from a x and y coordinates
#     x_coords <- rep(x, length(x))
#     y_coords <- rep(y, by = length(y))
# }

x <- seq(from = 1, to = 2, by = 0.2)
black1 <- rep(1, 6)
black2 <- rep(2, 6)
red1 <- seq(from =1, to = 2, by = 0.2)
red2 <- rev(red1)

y1 <- red1
y2 <- black2

plot(0,0, xlim = c(0, 3), ylim = c(0, 3))
segments(x0 = 1, y0 = black1[1], x1 = 2, y1 = black1[6])
segments(x0 = 1, y0 = black2[1], x1 = 2, y1 = black2[6])
segments(x0 = 1, y0 = red1[1], x1 = 2, y1 = red1[6], col = "red")
segments(x0 = 1, y0 = red2[1], x1 = 2, y1 = red2[6], col = "red")


absolute.curve.area(x, y1 = black1) == 0 # TRUE
absolute.curve.area(x, y1 = black2) == 0 # TRUE
absolute.curve.area(x, y1 = red1) == 0.5 # TRUE
absolute.curve.area(x, y1 = red2) == 0.5 # TRUE
absolute.curve.area(x, y1 = black1, y2 = black2) == 1 # TRUE
absolute.curve.area(x, y1 = black1, y2 = red2) == 0.5 # TRUE
absolute.curve.area(x, y1 = red1, y2 = black2) == 0.5 # TRUE
absolute.curve.area(x, y1 = red1, y2 = red2) == 0.5 # NOPE


