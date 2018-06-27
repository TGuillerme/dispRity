MorphDistMatrix.verbose <- function(morph.matrix, transform.proportional.distances="arcsine_sqrt", verbose=TRUE) {

  #verbose
  if(verbose == TRUE) {
    message("This is a verbose version of Claddis::MorphDistMatrix function v0.1.\n", appendLF=FALSE)
  }

  # Check format of transform.proportional.distances:
  if(transform.proportional.distances != "none" && transform.proportional.distances != "sqrt" && transform.proportional.distances != "arcsine_sqrt") {

    # Give error if something other than three possible settings is given:
    stop("ERROR: transform.proportional.distances must be one of \"none\", \"sqrt\", or \"arcsine_sqrt\".")

  }

  # Isolate ordering element of morphology matrix:
  ordering <- morph.matrix$ordering

  # Isolate max values element of morphology matrix:
  max.vals <- morph.matrix$max.vals
  
  # Isolate min values element of morphology matrix:
  min.vals <- morph.matrix$min.vals
  
  # Isolate weighting element of morphology matrix:
  weights <- morph.matrix$weights
  
  # Isolate character-taxon matrix element of morphology matrix:
  morph.matrix <- morph.matrix$matrix

  # Create empty vectors to store S and W value for Wills 2001 equations (1 and 2):
  differences <- maximum.differences <- vector(mode="numeric")
    
  # Distance matrices for storing:
  comp.char.matrix <- gower.dist.matrix <- max.dist.matrix <- dist.matrix <- matrix(0, nrow=length(rownames(morph.matrix)), ncol=length(rownames(morph.matrix)))
    
  # Fill comparable characters diagonal:
  comp.fill<-function(X) {
    return(length(X) - length(grep(TRUE, is.na(X))))
  }
  diag(comp.char.matrix)<-apply(morph.matrix, 1, comp.fill)

  #for(i in 1:length(morph.matrix[, 1])) {
  #  comp.char.matrix[i,i] <- length(morph.matrix[i, ]) - length(grep(TRUE, is.na(morph.matrix[i, ])))
  #}

  # Set up empty matrix for storing data to calculate the Generalised Euclidean Distance of Wills (2001):
  GED.data <- matrix(nrow=0, ncol=ncol(morph.matrix))

  #verbose
  if(verbose == TRUE) {
    message("Calculating the distances:", appendLF=FALSE)
  }

  # Go through matrix rows:
  for(i in 1:(length(morph.matrix[, 1]) - 1)) {
        
    # Go through matrix columns:
    for(j in (i + 1):length(morph.matrix[, 1])) {
            
      # Get just the comparable characters (those coded for both taxa):
      compchar <- intersect(grep(TRUE, !is.na(morph.matrix[rownames(morph.matrix)[i], ])), grep(TRUE, !is.na(morph.matrix[rownames(morph.matrix)[j], ])))
            
      # Get comparable characters for ith taxon:
      firstrow <- morph.matrix[rownames(morph.matrix)[i], compchar]

      # Get comparable characters for jth taxon:
      secondrow <- morph.matrix[rownames(morph.matrix)[j], compchar]
            
      # Deal with polymorphic characters (if present):
      if(length(grep("&", unique(c(firstrow, secondrow)))) > 0) {
                
        # Find ampersands (polymorphisms):
        ampersand.elements <- sort(c(grep("&", firstrow), grep("&", secondrow)))
                
        # Go through each polymorphic character:
        for(k in 1:length(ampersand.elements)) {
                    
          # Find out if two codings overlap once all polymorphism resolutions are considered:
          intersection.value <- intersect(strsplit(firstrow[ampersand.elements[k]], "&")[[1]], strsplit(secondrow[ampersand.elements[k]], "&")[[1]])
                    
          # Case if polymorphic and non-polymorphic values overlap:
          if(length(intersection.value) > 0) {
                        
            # Set ith value as zero (no difference):
            firstrow[ampersand.elements[k]] <- 0

            # Set jth value as zero (no difference)
            secondrow[ampersand.elements[k]] <- 0

          }
                    
          # Case if polymorphic and non-polymorphic values do not overlap:
          if(length(intersection.value) == 0) {
                        
            # Case if character is unordered (max difference is 1):
            if(ordering[compchar[ampersand.elements[k]]] == "unord") {
                            
              # Set ith value as zero:
              firstrow[ampersand.elements[k]] <- 0

              # Set jth value as 1 (making the ij difference equal to one):
              secondrow[ampersand.elements[k]] <- 1

            }
                        
            # Case if character is ordered (max difference is > 1):
            if(ordering[compchar[ampersand.elements[k]]] == "ord") {
                            
              # Get first row value(s):
              firstrowvals <- as.numeric(strsplit(firstrow[ampersand.elements[k]], "&")[[1]])
                            
              # Get second row value(s):
              secondrowvals <- as.numeric(strsplit(secondrow[ampersand.elements[k]], "&")[[1]])
                            
              # Make mini distance matrix:
              poly.dist.mat <- matrix(0, nrow=length(firstrowvals), ncol=length(secondrowvals))
                            
              # Go through each comparison:
              for(l in 1:length(firstrowvals)) {
                                
                # Record absolute difference:
                for(m in 1:length(secondrowvals)) poly.dist.mat[l, m] <- sqrt((firstrowvals[l] - secondrowvals[m]) ^ 2)

              }
                            
              # Set first value as zero:
              firstrow[ampersand.elements[k]] <- 0
                            
              # Set second value as minimum possible difference:
              secondrow[ampersand.elements[k]] <- min(poly.dist.mat)

            }

          }

        }

      }
      #End dealing with polymorphic characters (if present)

            
      # Get the absolute difference between the two rows:
      raw.diffs <- diffs <- abs(as.numeric(firstrow) - as.numeric(secondrow))
            
      # If there are differences greater than 1 for unordered characters then rescore as 1:
      if(length(grep(TRUE, diffs > 1)) > 0) diffs[grep(TRUE, diffs > 1)[grep(TRUE, ordering[compchar[grep(TRUE, diffs > 1)]] == "unord")]] <- 1

      # Find the incomparable characters:
      incompchar <- setdiff(1:ncol(morph.matrix), compchar)

      # Store data for GED with NAs for missing distances:
      GED.data <- rbind(GED.data, rbind(c(diffs, rep(NA, length(incompchar))), c(weights[compchar], weights[incompchar])))

      # Get weighted differences:
      diffs <- as.numeric(weights[compchar]) * diffs

      # Get raw Euclidean distance:
      raw.dist <- dist(rbind(diffs, rep(0, length(diffs))), method="euclidean")

      # Work out maximum difference (again, checked against ordering) using compchar characters only:
      raw.maxdiffs <- maxdiffs <- as.numeric(max.vals[compchar]) - as.numeric(min.vals[compchar])

      # Correct maximum possible differences for unordered characters:
      if(length(grep(TRUE, maxdiffs > 1)) > 0) maxdiffs[grep(TRUE, maxdiffs > 1)[grep(TRUE, ordering[compchar[grep(TRUE, maxdiffs > 1)]] == "unord")]] <- 1

      # Get vector of maximum differences (corrected for character weights):
      maxdiffs <- as.numeric(weights[compchar]) * maxdiffs

      # Store raw distance:
      dist.matrix[i, j] <- dist.matrix[j, i] <- raw.dist

      # Store Gower distance:
      gower.dist.matrix[i, j] <- gower.dist.matrix[j, i] <- sum(diffs) / sum(weights[compchar])

      # Store maximum-rescaled distance:
      max.dist.matrix[i, j] <- max.dist.matrix[j, i] <- sum(diffs) / sum(maxdiffs)

      # Store N comparable characters:
      comp.char.matrix[i, j] <- comp.char.matrix[j, i] <- length(compchar)

      # Add to maximum differences (S_ijk * W_ijk in equation 1 of Wills 2001):
      differences <- c(differences, diffs)

      # Add to maximum differences (S_ijk_max * W_ijk in equation 1 of Wills 2001):
      maximum.differences <- c(maximum.differences, maxdiffs)
      
      #verbose
      if(verbose == TRUE) {
        message(".", appendLF=FALSE)
      }

    }

  }
  #End of the loop for calculating the distance
  #verbose
  if(verbose == TRUE) {
    message("Done.\n", appendLF=FALSE)
  }

  # Calculated weighted mean univariate distance for calculating GED (equation 2 in Wills 2001):
  S_ijk_bar <- sum(differences) / sum(maximum.differences)

  # Replace missing distances with S_ijk_bar (i.e., results of equation 2 in Wills 2001 into equation 1 of Wills 2001):
  GED.data[is.na(GED.data)] <- S_ijk_bar

  # Isolate the distances:
  S_ijk <- GED.data[grep(TRUE, (1:nrow(GED.data) %% 2) == 1), ]

  # Isolate the weights:
  W_ijk <- GED.data[grep(TRUE, (1:nrow(GED.data) %% 2) == 0), ]

  # Calculate the GED (equation 1 of Wills 2001) for each pairwise comparison (ij):
  GED_ij <- sqrt(apply(W_ijk * (S_ijk ^ 2), 1, sum))

  # Create empty GED distance matrix:
  GED.dist.matrix <- matrix(0, nrow=nrow(morph.matrix), ncol=nrow(morph.matrix))

  # Set initial value for counter:
  counter <- 1

  #verbose
  if(verbose == TRUE) {
    message("Storing the distances:", appendLF=FALSE)
  }

  # Go through matrix rows:
  for(i in 1:(length(morph.matrix[, 1]) - 1)) {

    # Go through matrix columns:
    for(j in (i + 1):length(morph.matrix[, 1])) {

      # Store distance:
      GED.dist.matrix[i, j] <- GED.dist.matrix[j, i] <- GED_ij[counter]

      # Update counter:
      counter <- counter + 1

      #verbose
      if(verbose == TRUE) {
        message(".", appendLF=FALSE)
      }
    }

  }

  #verbose
  if(verbose == TRUE) {
    message("Done.\n", appendLF=FALSE)
  }
    
  # Set diagonals as zero:
  diag(gower.dist.matrix) <- diag(max.dist.matrix) <- 0

  # Add row and column names (taxa) to distance matrices:
  rownames(comp.char.matrix) <- colnames(comp.char.matrix) <- rownames(GED.dist.matrix) <- colnames(GED.dist.matrix) <- rownames(gower.dist.matrix) <- colnames(gower.dist.matrix) <- rownames(max.dist.matrix) <- colnames(max.dist.matrix) <- rownames(dist.matrix) <- colnames(dist.matrix) <- rownames(morph.matrix)

  # If transformation option is not "none":
  if(transform.proportional.distances != "none") {

    #verbose
    if(verbose == TRUE) {
      message("Transforming the proportional distances:...", appendLF=FALSE)
    }

    # If transformation option is "sqrt":
    if(transform.proportional.distances == "sqrt") {

      # Replace NaN with NA for Gower distances and take square root:
      gower.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, sqrt(gower.dist.matrix))), nrow=nrow(gower.dist.matrix), dimnames=list(rownames(gower.dist.matrix), rownames(gower.dist.matrix)))

      # Replace NaN with NA for Max distances and take square root:
      max.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, sqrt(max.dist.matrix))), nrow=nrow(max.dist.matrix), dimnames=list(rownames(max.dist.matrix), rownames(max.dist.matrix)))

    # If transformation option is "arcsine_sqrt":
    } else {

      # Establish correction factor to ensure Gower data is proportional:
      gower.correction <- max(c(max(sort(gower.dist.matrix)), 1))

      # Ensure all Gower values are on 0 to 1 scale then take arcsine of sqrt to get values that better approximate a normal distribution:
      gower.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, asin(sqrt(gower.dist.matrix / gower.correction)))), nrow=nrow(gower.dist.matrix), dimnames=list(rownames(gower.dist.matrix), rownames(gower.dist.matrix)))

      # Take arcsine square root of all MOD dist values:
      max.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, asin(sqrt(max.dist.matrix)))), nrow=nrow(max.dist.matrix), dimnames=list(rownames(max.dist.matrix), rownames(max.dist.matrix)))

    }
    
    #verbose
    if(verbose == TRUE) {
      message("Done.\n", appendLF=FALSE)
    }

  # If transformation option is "none":
  } else {

    # Replace NaN with NA for Gower distances:
    gower.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, gower.dist.matrix)), nrow=nrow(gower.dist.matrix), dimnames=list(rownames(gower.dist.matrix), rownames(gower.dist.matrix)))

    # Replace NaN with NA for Max distances:
    max.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, max.dist.matrix)), nrow=nrow(max.dist.matrix), dimnames=list(rownames(max.dist.matrix), rownames(max.dist.matrix)))

  }

  # Compile results as a list:
  result <- list(dist.matrix, GED.dist.matrix, gower.dist.matrix, max.dist.matrix, comp.char.matrix)
    
  # Add names to results list:
  names(result) <- c("raw.dist.matrix", "GED.dist.matrix", "gower.dist.matrix", "max.dist.matrix", "comp.char.matrix")
    
  # Output result:
  return(result)

}
