
# This part contains the class metric and the summary metric functions.
# The class functions is the matrix descriptor the metric is based on (e.g. variance, range, distance from centroid, etc).
# The summary function is the summary of the matrix descriptor (e.g. mean, product, etc).

# Class functions:
# These functions should take a matrix as an input and output a vector
# Typically one can use apply functions or loops

#Calculating each axis variance
variance<-function(data_matrix) {
    return(apply(data_matrix, 2, var))
}

#Calculating each axis ranges
range<-function(data_matrix) {
    #Max values
    max_values<-apply(data_matrix, 2, max)
    #Min values
    min_values<-apply(data_matrix, 2, min)
    #Ranges values
    ranges<-max_values-min_values
    return(ranges)
}

#Calculating the distance from centroid
centroid.dist<-function(data_matrix) {

    #Calculating the centroid point
    centroid<-apply(data_matrix, 2, mean)

    #Calculating the distance from centroid
    cent.dist<-NULL
    for (j in 1:nrow(data_matrix)){
        cent.dist[j] <- dist(rbind(data_matrix[j,], centroid), method="euclidean")
    }

    return(cent.dist)
}

# Summary functions:
# These functions should take a vector as an input and output a single numeric character

mode.val<-function(X){
    return(as.numeric(names(sort(-table(X))[1])))
}
