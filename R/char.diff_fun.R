## R version of the code (SLOW!)
char.diff_R <- function(X,Y){ 

    # Convert character
    convert.character <- function(X) {
        if(class(X) == "numeric") {
            X <- LETTERS[X+1]
        } else {
            X <- as.factor(X)
            levels(X) <- 1:length(levels(X))
            X <- as.numeric(X)
        }
        return(X)
    }

    # Transform states into similar values
    normalise.character <- function(X, states) {
        # Convert X to character
        if(class(X) != "numeric") {
            X <- convert.character(X)
        }
        X <- as.character(X)
        # Get the states of X
        states_match <- sort(match(states, X))

        # Replacing the original states
        for(state in 1:length(states)) {
            X <- gsub(X[states_match[state]], LETTERS[state], X)
        }
        X <- convert.character(X)
        return(X)
    }

    # Convert the characters to numeric (if needed)
    if(class(X) != "numeric") {
        X <- convert.character(X)
    }
    if(class(Y) != "numeric") {
        Y <- convert.character(Y)
    }

    # Remove any uncomparable characters
    na_X <- which(is.na(X))
    na_Y <- which(is.na(Y))
    if(length(c(na_X, na_Y)) > 0) {
        X <- X[-c(na_X, na_Y)]
        Y <- Y[-c(na_X, na_Y)]
    }
    
    if(length(X) == 0) {
        return(NA)
    }

    # Check if characters are binary
    states_X <- as.numeric(levels(as.factor(X)))
    states_Y <- as.numeric(levels(as.factor(Y)))

    if(length(states_X) <= 2 & length(states_Y) <= 2) {
        # Simple binary Fitch comparison (fast)
        differences <- ifelse(X-Y != 0, 1, 0)

        # Calculate the difference
        return( round( 1 - ( abs(sum(abs(differences))/length(X)-0.5)/0.5 ), digit = 10))
    } else {
        # Normalise the characters
        X <- normalise.character(X, states_X)
        Y <- normalise.character(Y, states_Y)

        # Calculate the differences
        differences <- X-Y

        #Default fitch for now.
        #type <- "Fitch"

        #if(type == "Fitch") {
            # Make the differences binary (i.e. if the difference is != 0, set to 1)
            differences <- ifelse(differences != 0, 1, 0)
        #}

        # Get the characters difference
        return( round( 1 - ( abs(sum(abs(differences))/length(X)-0.5)/0.5 ), digit = 10))
    }
}
