## devtools
library(devtools)

## Data
matrix <- matrix(ncol = 99, nrow = 100, data = rnorm(99*100))

## Cleaning package
remove.packages("dispRity")

## Installing last release
install_github("TGuillerme/dispRity", ref = "release")
library(dispRity)
time_v_021 <- system.time(data_v_021 <- dispRity(boot.matrix(matrix, 1000, rarefaction = TRUE, verbose = TRUE), metric = c(median, centroids), verbose = TRUE))

## Cleaning package
remove.packages("dispRity")

## Installing last release
install_github("TGuillerme/dispRity", ref = "dev-testing")
library(dispRity)
time_v_03 <- system.time(data_v_03 <- dispRity(boot.matrix(matrix, 1000, rarefaction = TRUE, verbose = TRUE), metric = c(median, centroids), verbose = TRUE))

## Cleaning package
remove.packages("dispRity")

## Speed difference
time_v_021 - time_v_03

## Speed improvement
cat("Calculation is", max(as.numeric(time_v_021/time_v_03)[1:3]), "times faster.")

## Size improvement
cat("Object is", as.numeric(object.size(data_v_021))/as.numeric(object.size(data_v_03)), "times smaller.")
