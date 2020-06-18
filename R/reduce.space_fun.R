# ' @description Optimise a parameter selection
# ' @param function the function to run for optimisation
# ' @param optimise the argument to optimise
# ' @param criterion the optimisation criterion (e.g. remove = 0.5)
# ' @param tuning a list of three parameters: "max" for the maximum of operations, "tol" for the tuning (e.g. 0.1 close) "good" for when to decide it's good enough (i.e. stop if it reaches the tuning after X number of times).
# ' @param parameters other parameters for the function
# ' @param to_remove the logical list of elements to remove for automatically running the optimisation
# ' @param remove the proportion of elements to be removed (in probability)
# ' @param verbose
# ' @param space
## Optimisation function
optimise.parameter <- function(fun, args, criterion, tuning, verbose) {

    ## Initialise the optimisation
    counter <- 0
    increment <- 1

    ## First run
    difference <- length(which(do.call(fun, args)))-criterion

    ## Optimisation loop
    while(difference != 0) {

        ## Tolerance
        if(abs(difference) < tuning$tol*criterion) {
            break
        }

        ## Modify the parameter to optimise
        args$parameters$optimise <- ifelse(difference <= 0, args$parameters$optimise + increment, args$parameters$optimise - increment)

        ## Second run
        new_difference <- length(which(do.call(fun, args))) - criterion

        ## Check if the increment is to big
        while(abs(new_difference/difference) > 1) {
            ## Reject the optimisation
            args$parameters$optimise <- ifelse(difference < 0, args$parameters$optimise - increment, args$parameters$optimise + increment)
            ## And decrease the increment
            increment <- increment/tuning$inc.steps
            ## Update the optimisation
            args$parameters$optimise <- ifelse(difference < 0, args$parameters$optimise + increment, args$parameters$optimise - increment)

            # print(args$parameters$optimise)

            ## Re-do the second run
            new_difference <- length(which(do.call(fun, args))) - criterion
            
            ## Increment the counter
            if(counter < tuning$max) {
                counter <- counter + 1
                if(verbose) cat(".")
            } else {
                break
            }
        }

        ## Update the difference
        difference <- new_difference

        ## Increment the counter
        if(counter < tuning$max) {
            counter <- counter + 1
            if(verbose) cat(".")
        } else {
            break
        }
    }

    ## Return the optimal parameter
    return(args$parameters$optimise)
}
## Wrapping optimisation function
optimise.results <- function(to_remove, fun, remove, args, tuning, verbose = FALSE, space, return.optim = FALSE) {

    ## Set the optimality criterion
    criterion <- round(remove * nrow(space))
    ## Check if optimisation is necessary
    if(length(which(to_remove)) != criterion) {
        
        if(verbose) cat("Run parameter optimisation:")

        ## Find the optimal parameter
        args$parameters$optimise <- optimise.parameter(fun, args, criterion = criterion, tuning = tuning, verbose = verbose)
        
        ## Rerun the function with the optimal parameter
        to_remove <- do.call(fun, args)

        if(verbose) cat("Done.\n")
    }

    if(!return.optim) {
        return(to_remove)
    } else {
        return(list(remove = to_remove, optim = args$parameters$optimise))
    }
}

## The different run functions
run.limit.removal <- function(space, parameters) {
    return(apply(space, 1, point.in.circle, centre = parameters$centre, radius = parameters$optimise))
}
# run.displacement.removal <- function(space, parameters, scree) {
#     return(apply(space, 1, select.value, value = parameters$optimise* ))
# }
run.density.removal <- function(space, parameters, scree) {
    close_neigbhours <- get.neigbhours(distance = parameters$distance, diameter = parameters$optimise)
    return(1:nrow(space) %in% close_neigbhours)
}

# ' @description Selecting points within a circle
# ' @param point a point in space
# ' @param centre the centre from which to count the radius
# ' @param radius the radius for removal
# ' 
point.in.circle <- function(point, centre, radius) {

    ## Measure the distance from the center
    distance <- as.numeric(dist(rbind(centre, point)))

    ## Results
    return(ifelse(distance < radius, TRUE, FALSE))
}

# ' @description Select only the points above one value
# ' @param point a point in space
# ' @param value the threshold value
# ' 
# select.value <- function(point, value) {
#     ## Get a point above a certain value
#     return(ifelse(all(point > value), TRUE, FALSE))
# }

# ' @description Selects pairs of nearest neighbours
# ' @param trait_space the space
# ' @param distance a distance matrix of the trait space
# ' @param diameter the diameter for cosidering closeness or distance
# '
get.neigbhours <- function(distance, diameter = 0.1) {

    ## Get the neighbors
    neighbors <- which(distance < diameter, arr.ind = TRUE)

    ## Select the neighbors
    return(unique(neighbors[neighbors[,1] != neighbors[,2]]))
}