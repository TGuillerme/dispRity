# Transforming a morphological matrix into a phyDat object (automatically!)
get.contrast.matrix <- function(matrix) {
    
    # Extracting the states
    states <- sort(unique(as.vector(matrix)))
    
    # Check if there is a "?" token
    if(any(states == "?")) {
        # remove the "?" state
        states_num <- states[-which(states == "?")]
        # Create a simple square matrix with 0s...
        contrast_matrix <- matrix(data = rep(0, length(states_num)*length(states_num)), ncol = length(states_num), dimnames = list(as.character(states_num), as.character(states_num)))
        # Set the diagonal to 0 
        diag(contrast_matrix) <- 1
        # Add the joker character as a row full of 1s
        joker_matrix <- matrix(data = rep(1, length(states_num)), ncol = length(states_num), dimnames = list("?", as.character(states_num)))
        contrast_matrix <- rbind(contrast_matrix, joker_matrix)
    } else {
        # Create a simple square matrix with 0s...
        contrast_matrix <- matrix(data = rep(0, length(states)*length(states)), ncol = length(states), dimnames = list(as.character(states), as.character(states)))
        # Set the diagonal to 0 
        diag(contrast_matrix) <- 1
    }

    return(contrast_matrix)
}

