# Road to 1.10

 * `multi.ace` updates
 * `dispRity` can now handle abundance data through the `abundance` argument in all main functions.
  - [ ] allow new internal structure object `"$abundance"` which is a list of matrices of taxa * site?
  - [ ] modify internal architecture to pull from $abundance for calculations (replicating rows relative to abundance or to bootstrap)
  - [ ] allow plots to show abundance (points size scale with abundance)

##


```r
library(dispRity)
data(BeckLee_mat50) 
abundance_data <- matrix(sample(c(0,1,2,3), 200, replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.1)), nrow = 50, ncol = 4)
rownames(abundance_data) <- rownames(BeckLee_mat50)
colnames(abundance_data) <- c("site1", "site2", "site3", "site4") 

some_data <- make.dispRity(data = BeckLee_mat50)

some_data$abundance[[1]]$matrix <- abundance_data

## Needs checking:
    # 1- same rownames in both $abundance and $matrix
    # 2- same length in $abundance and $matrix OR 1 $abundance and >1 $matrix

## Metric fun:
    # 1- abundance only metric  -> detected if args are only abundance
average1 <- function(matrix, ...) {
    sum(matrix)/length(matrix)
}
## Toggling to abundance
average2 <- as.abundance(average1)
## becomes:
average2 <- function(abundance, ...) {
    sum(abundance)/length(abundance)
}
    # 2- abundance + trait metric -> detected if args are matrix + abundance (+ tree + matrix2).
abundance.average <- function(matrix, abundance, ...) {
    sum(matrix * abundance)/ length(matrix)
}

## metric 1
dispRity(some_data, metric = as.abundance(average1))
dispRity(some_data, metric = average2)

# metric 2
dispRity(some_data, metric = abundance.average)

## Needs checking
    # 1- if metric has abundance, does data has abundance?


## Works for custom.subsets
## Works for chrono.subsets 
## Works with tests

```