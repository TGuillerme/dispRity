## TESTING ordination

context("ordinations")


#Loading Claddis
require(Claddis)

## Manual MorphDistMatrix fun declaration
MorphDistMatrix.Claddis <- function(morph.matrix, transform.proportional.distances = "arcsine_sqrt") {

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
  differences <- maximum.differences <- vector(mode = "numeric")
    
  # Distance matrices for storing:
  comp.char.matrix <- gower.dist.matrix <- max.dist.matrix <- dist.matrix <- matrix(0, nrow = length(rownames(morph.matrix)), ncol = length(rownames(morph.matrix)))
    
  # Fill comparable characters diagonal:
  for(i in 1:length(morph.matrix[, 1])) comp.char.matrix[i,i] <- length(morph.matrix[i, ]) - length(which(is.na(morph.matrix[i, ])))

  # Set up empty matrix for storing data to calculate the Generalised Euclidean Distance of Wills (2001):
  GED.data <- matrix(nrow = 0, ncol = ncol(morph.matrix))

  # Go through matrix rows:
  for(i in 1:(length(morph.matrix[, 1]) - 1)) {
        
    # Go through matrix columns:
    for(j in (i + 1):length(morph.matrix[, 1])) {
            
      # Get just the comparable characters (those coded for both taxa):
      compchar <- intersect(which(!is.na(morph.matrix[rownames(morph.matrix)[i], ])), which(!is.na(morph.matrix[rownames(morph.matrix)[j], ])))
            
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
              poly.dist.mat <- matrix(0, nrow = length(firstrowvals), ncol = length(secondrowvals))
                            
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
            
      # Get the absolute difference between the two rows:
      raw.diffs <- diffs <- abs(as.numeric(firstrow) - as.numeric(secondrow))
            
      # If there are differences greater than 1 for unordered characters then rescore as 1:
      if(length(which(diffs > 1)) > 0) diffs[which(diffs > 1)[which(ordering[compchar[which(diffs > 1)]] == "unord")]] <- 1

      # Find the incomparable characters:
      incompchar <- setdiff(1:ncol(morph.matrix), compchar)

      # Get weighted differences:
      diffs <- as.numeric(weights[compchar]) * diffs

      # Store data for GED with NAs for missing distances:
      GED.data <- rbind(GED.data, rbind(c(diffs, rep(NA, length(incompchar))), c(weights[compchar], weights[incompchar])))

      # Get raw Euclidean distance:
      raw.dist <- dist(rbind(diffs, rep(0, length(diffs))), method = "euclidean")

      # Work out maximum difference (again, checked against ordering) using compchar characters only:
      raw.maxdiffs <- maxdiffs <- as.numeric(max.vals[compchar]) - as.numeric(min.vals[compchar])

      # Correct maximum possible differences for unordered characters:
      if(length(which(maxdiffs > 1)) > 0) maxdiffs[which(maxdiffs > 1)[which(ordering[compchar[which(maxdiffs > 1)]] == "unord")]] <- 1

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

    }

  }

  # Calculated weighted mean univariate distance for calculating GED (equation 2 in Wills 2001):
  S_ijk_bar <- sum(differences) / sum(maximum.differences)

  # Replace missing distances with S_ijk_bar (i.e., results of equation 2 in Wills 2001 into equation 1 of Wills 2001):
  GED.data[is.na(GED.data)] <- S_ijk_bar

  # Isolate the distances:
  S_ijk <- GED.data[which((1:nrow(GED.data) %% 2) == 1), ]

  # Isolate the weights:
  W_ijk <- GED.data[which((1:nrow(GED.data) %% 2) == 0), ]

  # Calculate the GED (equation 1 of Wills 2001) for each pairwise comparison (ij):
  GED_ij <- sqrt(apply(W_ijk * (S_ijk ^ 2), 1, sum))

  # Create empty GED distance matrix:
  GED.dist.matrix <- matrix(0, nrow = nrow(morph.matrix), ncol = nrow(morph.matrix))

  # Set initial value for counter:
  counter <- 1

  # Go through matrix rows:
  for(i in 1:(length(morph.matrix[, 1]) - 1)) {

    # Go through matrix columns:
    for(j in (i + 1):length(morph.matrix[, 1])) {

      # Store distance:
      GED.dist.matrix[i, j] <- GED.dist.matrix[j, i] <- GED_ij[counter]

      # Update counter:
      counter <- counter + 1

    }

  }
    
  # Set diagonals as zero:
  diag(gower.dist.matrix) <- diag(max.dist.matrix) <- 0

  # Add row and column names (taxa) to distance matrices:
  rownames(comp.char.matrix) <- colnames(comp.char.matrix) <- rownames(GED.dist.matrix) <- colnames(GED.dist.matrix) <- rownames(gower.dist.matrix) <- colnames(gower.dist.matrix) <- rownames(max.dist.matrix) <- colnames(max.dist.matrix) <- rownames(dist.matrix) <- colnames(dist.matrix) <- rownames(morph.matrix)

  # If transformation option is not "none":
  if(transform.proportional.distances != "none") {

    # If transformation option is "sqrt":
    if(transform.proportional.distances == "sqrt") {

      # Replace NaN with NA for Gower distances and take square root:
      gower.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, sqrt(gower.dist.matrix))), nrow = nrow(gower.dist.matrix), dimnames = list(rownames(gower.dist.matrix), rownames(gower.dist.matrix)))

      # Replace NaN with NA for Max distances and take square root:
      max.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, sqrt(max.dist.matrix))), nrow = nrow(max.dist.matrix), dimnames = list(rownames(max.dist.matrix), rownames(max.dist.matrix)))

    # If transformation option is "arcsine_sqrt":
    } else {

      # Establish correction factor to ensure Gower data is proportional:
      gower.correction <- max(c(max(sort(gower.dist.matrix)), 1))

      # Ensure all Gower values are on 0 to 1 scale then take arcsine of sqrt to get values that better approximate a normal distribution:
      gower.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, asin(sqrt(gower.dist.matrix / gower.correction)))), nrow = nrow(gower.dist.matrix), dimnames = list(rownames(gower.dist.matrix), rownames(gower.dist.matrix)))

      # Take arcsine square root of all MOD dist values:
      max.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, asin(sqrt(max.dist.matrix)))), nrow = nrow(max.dist.matrix), dimnames = list(rownames(max.dist.matrix), rownames(max.dist.matrix)))

    }

  # If transformation option is "none":
  } else {

    # Replace NaN with NA for Gower distances:
    gower.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, gower.dist.matrix)), nrow = nrow(gower.dist.matrix), dimnames = list(rownames(gower.dist.matrix), rownames(gower.dist.matrix)))

    # Replace NaN with NA for Max distances:
    max.dist.matrix <- matrix(as.numeric(gsub(NaN, NA, max.dist.matrix)), nrow = nrow(max.dist.matrix), dimnames = list(rownames(max.dist.matrix), rownames(max.dist.matrix)))

  }

  # Compile results as a list:
  result <- list(dist.matrix, GED.dist.matrix, gower.dist.matrix, max.dist.matrix, comp.char.matrix)
    
  # Add names to results list:
  names(result) <- c("raw.dist.matrix", "GED.dist.matrix", "gower.dist.matrix", "max.dist.matrix", "comp.char.matrix")
    
  # Output result:
  return(result)
}



test_that("Claddis.support works", {
    expect_equal(MorphDistMatrix.Claddis(Michaux1989), MorphDistMatrix.support(Michaux1989))
    expect_equal(MorphDistMatrix.Claddis(Gauthier1986), MorphDistMatrix.support(Gauthier1986))

    #Testing with a more complex (bigger) matrix
    #Some random matrix
    matrix <- matrix(sample(c("0", "1", "2", "0&1", "0&2", "1&2", "0&1&2"), 5000, replace=TRUE, prob=c(0.425, 0.42, 0.12, 0.01, 0.01, 0.01, 0.005))
    , nrow=50, ncol=100, dimnames=list(c(1:50)))
    #Adding 25% of missing characters
    matrix[sample(1:5000, 200)] <- NA

    #ordering
    ordering <- sample(c("unord", "ord"), 100, replace=TRUE, prob=c(0.9,0.1))

    #weights
    weights <- sample(c(1:3), 100, replace=TRUE, prob=c(0.85,0.1,0.05))

    #Function for making a morph.matrix like object
    make.nexus<-function(matrix, header, ordering, weights) {
        nexus<-list()
        nexus$header<-header
        nexus$matrix<-matrix
        nexus$ordering<-ordering
        nexus$weights<-weights
        nexus$max.vals<-as.numeric(apply(matrix, 2, max, na.rm=TRUE))
        nexus$min.vals<-as.numeric(apply(matrix, 2, min, na.rm=TRUE))
        return(nexus)
    }

    morph.matrix <- make.nexus(matrix, header="example", ordering, weights)

    expect_equal(MorphDistMatrix.Claddis(morph.matrix), MorphDistMatrix.support(morph.matrix))

})

# Testing if each individual matrix is corrects
test_that("Claddis.support works individually", {
    results_base <- MorphDistMatrix.Claddis(Michaux1989)
    results_fast <- MorphDistMatrix.support(Michaux1989)
    results_raw <- MorphDistMatrix.support(Michaux1989, distance = "Raw")
    results_GED <- MorphDistMatrix.support(Michaux1989, distance = "GED")
    results_Gower <- MorphDistMatrix.support(Michaux1989, distance = "Gower")
    results_Max <- MorphDistMatrix.support(Michaux1989, distance = "Max")
    results_Comp <- MorphDistMatrix.support(Michaux1989, distance = "Comp")
    expect_equal(results_base$raw.dist.matrix, results_raw)
    expect_equal(results_base$GED.dist.matrix, results_GED)
    expect_equal(results_base$gower.dist.matrix, results_Gower)
    expect_equal(results_base$max.dist.matrix, results_Max)
    expect_equal(results_base$comp.char.matrix, results_Comp)
})


test_that("Claddis.ordination works", {
    
    data_matrix <- matrix(c("0", "0", "1", "1", "0", "0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "1", "0", "1", "0", "0", "0", "0", "1", "0", "1", "0", "0", "1", "0", "0", "0", "1", "0", "1", "0", "1", "0", "1", "0", "0", "0", "0", "1", "0"), byrow = FALSE, nrow = 4)
    rownames(data_matrix) <- c("Ancilla", "Turrancilla", "Ancillista", "Amalda")
    data <- list(header = "", matrix = data_matrix, ordering = rep("unord", 11), weights = rep(1, 11), max.vals = rep(1, 11), min.vals = rep(0, 11), step.matrices = NULL, symbols = c("0", "1"))

    ## Sanitizing
    expect_error(Claddis.ordination(matrix(5), distance = "Gower", transform = "none", k = 2))
    expect_error(Claddis.ordination(data, distance = "bob", transform = "none", k = 2))
    expect_error(Claddis.ordination(data, distance = "Gower", transform = 1, k = 2))
    expect_error(Claddis.ordination(data, distance = "Gower", transform = "none", k = 10))
    expect_error(Claddis.ordination("blob", distance = "Gower", transform = "none", k = 10))

    test <- Claddis.ordination(data)
    expect_equal(dim(test), c(4,3))
    expect_equal(rownames(test), c("Ancilla", "Turrancilla", "Ancillista", "Amalda"))
    expect_equal(round(as.vector(test), 5) , round(c(7.252259e-17, -5.106645e-01, 5.106645e-01, -3.207162e-16, 4.154578e-01, -4.566150e-16, -8.153839e-16, -4.154578e-01, 2.534942e-01, -2.534942e-01, -2.534942e-01, 2.534942e-01), 5))
})



test_that("geomorph.ordination works", {

    ## Internal: make.groups.factors
    set.seed(1)
    one_factor_list <- as.factor(sample(LETTERS[1:2], 10, replace = TRUE))
    expect_equal(make.groups.factors(one_factor_list), list(A = c(1,2,5,10), B = c(3,4,6,7,8,9)))

    set.seed(1)
    array <- array(rnorm(100), c(5,2,10))
    dummy_procrustes <- list(coords = array)
    class(dummy_procrustes) <- "gpagen"
    dummy_geomorph_df <- list(coords = array, factor1 = as.factor(sample(LETTERS[1:2], 10, replace = TRUE)), factor2 = as.factor(c(rep(1, 5), rep(2, 5))))
    class(dummy_geomorph_df) <- "geomorph.data.frame"

    ## Sanitizing
    expect_error(geomorph.ordination(array))
    expect_error(geomorph.ordination(list(coords = array)))
    expect_error(geomorph.ordination(dummy_procrustes, center = "no"))
    dummy_procrustes2 <- dummy_procrustes
    dummy_procrustes2$coords <- NULL
    expect_error(geomorph.ordination(dummy_procrustes2))

    ## Procrustes to ordination
    test <- geomorph.ordination(dummy_procrustes)
    expect_equal(dim(test), c(10,10))
    expect_equal(colnames(test), paste0("PC", 1:10))

    ## geomorph.data.frame to ordination
    test <- geomorph.ordination(dummy_geomorph_df)
    expect_is(test, "dispRity")
    expect_equal(length(test$subsets), 4)
    expect_equal(
        names(test$subsets)
        ,c("factor1.A", "factor1.B", "factor2.1", "factor2.2")
        )
    expect_equal(
        as.vector(unlist(lapply(test$subsets, lapply, length)))
        , c(6,4,5,5)
        )
    
    dummy_geomorph_df2 <- dummy_geomorph_df
    dimnames(dummy_geomorph_df2$coords)[[3]] <- letters[1:10]

    test <- geomorph.ordination(dummy_geomorph_df2)
    expect_equal(dimnames(test$matrix)[[1]], letters[1:10])
    expect_equal(dimnames(test$matrix)[[2]], paste0("PC", 1:10))

})
