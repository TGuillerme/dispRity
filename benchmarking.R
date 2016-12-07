


## Ordinating the matrices
matrix_ord_Beck 
matrix_ord_Hall 


time_slices <- rev(seq(from = 0, to = 120, by = 5))

time_slice_beck21 <- system.time(time_series_Beck <- time.series(matrix_ord_Beck, tree_Beck,
    method = "continuous", time = time_slices, model = "gradual",
    FADLAD = FADLAD_Beck, verbose = TRUE))
time_slice_hall21 <- system.time(time_series_Hall <- time.series(matrix_ord_Hall, tree_Hall,
    method = "continuous", time = time_slices, model = "gradual",
    FADLAD = FADLAD_Hall, verbose = TRUE))


## Bootstrapping the time series
boot_beck21 <- system.time(time_series_Beck_bs <- boot.matrix(time_series_Beck, bootstrap = 1000))
boot_hall21 <- system.time(time_series_Hall_bs <- boot.matrix(time_series_Hall, bootstrap = 1000))

## Disparity

## Calculating the distance from the centroids
disparity_beck21 <- system.time(dist_centroids_Beck <- dispRity(time_series_Beck_bs, metric = c(centroids, median)))
disparity_hall21 <- system.time(dist_centroids_Hall <- dispRity(time_series_Hall_bs, metric = c(centroids, median)))

## Sizes
size_beck21 <- object.size(dist_centroids_Beck)
size_hall21 <- object.size(dist_centroids_Hall)




## 3.0!

time_slice_beck30 <- system.time(time_series_Beck <- time.series(matrix_ord_Beck, tree_Beck,
    method = "continuous", time = time_slices, model = "gradual",
    FADLAD = FADLAD_Beck, verbose = TRUE))
time_slice_hall30 <- system.time(time_series_Hall <- time.series(matrix_ord_Hall, tree_Hall,
    method = "continuous", time = time_slices, model = "gradual",
    FADLAD = FADLAD_Hall, verbose = TRUE))


## Bootstrapping the time series
boot_beck30 <- system.time(time_series_Beck_bs <- boot.matrix(time_series_Beck, bootstrap = 1000))
boot_hall30 <- system.time(time_series_Hall_bs <- boot.matrix(time_series_Hall, bootstrap = 1000))

## Disparity

## Calculating the distance from the centroids
disparity_beck30 <- system.time(dist_centroids_Beck <- dispRity(time_series_Beck_bs, metric = c(centroids, median)))
disparity_hall30 <- system.time(dist_centroids_Hall <- dispRity(time_series_Hall_bs, metric = c(centroids, median)))

## Sizes
size_beck30 <- object.size(dist_centroids_Beck)
size_hall30 <- object.size(dist_centroids_Hall)


## Time slice performance
time_slice_beck21/time_slice_beck30
time_slice_hall21/time_slice_hall30

# Slightly slower!

## Bootstrap performance
boot_beck21/boot_beck30
boot_hall21/boot_hall30

# 10-100 times faster!

## Disparity performance
disparity_beck21/disparity_beck30
disparity_hall21/disparity_hall30

# Around 2 times faster

## Size difference

size_beck21 <- 849159232
size_hall21 <- 1529568904

size_beck21/size_beck30
size_hall21/size_hall30

# 300-400 times smaller!
