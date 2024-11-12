#Inferring the number of elements creating the resulting pairwise combination
find.num.elements <- function(x) {
    # Initialising the search
    n = 2

    # Equation to solve
    solve.eq <- function(n) {
        return(n^2/2 - n/2)
    }

    solution <- solve.eq(n)

    # Loop through the solutions
    while(solution < x) {
        n = n + 1
        solution <- solve.eq(n)
    }

    # Warning if equation is not fully solved
    if(solution != x) {
        stop(paste("The number of elements to plot is not a function of an entire number.",
            "To get the right number of combinations, you must satisfy:",
            "    ncol(combn(seq(1:n_elements), 2)) == nrow(data)",
            "where 'n_elements' is the number of elements compared pairwise.", sep = "\n"), call. = FALSE)
    }
    return(n)
}