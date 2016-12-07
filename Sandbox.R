## DEBUG

 # disparity should contain
 # data
 # elements
 # series

## Generating a dummy ordinated matrix
ordinated_matrix <- matrix(data = rnorm(90), nrow = 10, ncol = 9, dimnames = list(letters[1:10]))
## Creating a list of dummy factors (1 or 2)
factors <- as.data.frame(matrix(data = c(rep(1,5), rep(2,5)), nrow = 10, ncol = 1, dimnames = list(letters[1:10])))
## Splitting the dummy ordinated matrix
cust.series(ordinated_matrix, factors)


## New disparity object:

disparity_object <- list()
## The matrix
disparity_object$matrix <- ordinated_matrix
## The series
disparity_object$series <- list("origin" = c(1:nrow(ordinated_matrix)), "V1.1" = c(1,2,3,4,5), "V1.2" = c(6,7,8,9,10))
## The rarefaction (containing the list of bootstraps!)
disparity_object$series[[1]] <- list(
                                     list(seq(1:10), c(9,4,1,4,1,2,5,10,8,1), c(1,8,8,10,2,1,7,9,5,3)), # First rarefaction level (full) Note that the first element (0) is the normal list for no rarefaction
                                     list(seq(1:10), c(2,3,10), c(5,8,1))  # Second rarefaction level (...)
                                     )
disparity_object$series[[2]] <- list(
                                     list(seq(1:5), c(5,2,4,2,2), c(2,3,3,2,5)), # First rarefaction level (full)
                                     list(seq(1:5), c(1,4,1), c(5,3,3))  # Second rarefaction level (...)
                                     )
disparity_object$series[[3]] <- list(
                                     list(seq(from=5, to=10), c(10,7,8,9,7), c(7,9,5,8,5)), # First rarefaction level (full)
                                     list(seq(from=5, to=10), c(9,10,9), c(10,7,9))  # Second rarefaction level (...)
                                     )
## The elements (unchanged)
disparity_object$elements <- rownames(ordinated_matrix)




## The bootstrap/rarefaction should work something like:
series_rows <- nrow(disparity_object$matrix[disparity_object$series[[series+1]],])
rarefaction <- series_rows
bootstrap <- sample(1:series_rows, rarefaction, replace = TRUE) ## Change the replace = TRUE depending on the algorithm